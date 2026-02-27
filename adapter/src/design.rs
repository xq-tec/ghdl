#![expect(unused, reason = "// TODO remove before release")]

use std::fmt;
use std::num::NonZeroU32;
use std::ops::Deref;

use compact_str::CompactString;
use ghdl_ast as ast;
use hdl_simulation_protocol::design_hierarchy as hierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalInstanceId;
use serde::Deserialize;
use tracing::info;
use tracing::instrument;

#[derive(Debug, Deserialize)]
pub struct Signal {
    decl: u32,
    #[serde(skip_deserializing)]
    name: CompactString,
    #[serde(rename = "type")]
    typ: Type,
}

#[derive(Debug, Deserialize)]
struct Instance {
    stmt: u32,
    source: u32,
    #[serde(skip_deserializing)]
    name: CompactString,
    objects: Vec<ObjectKind>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ObjectKind {
    Object { val_kind: ValKind },
    Instance { id: u32 },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ValKind {
    Signal {
        id: NonZeroU32,
    },
    Memory,
    #[serde(other)]
    Other,
}

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
enum Type {
    Bit {
        left: i64,
        right: i64,
        dir: Dir,
    },
    Logic {
        left: i64,
        right: i64,
        dir: Dir,
    },
    Discrete {
        left: i64,
        right: i64,
        dir: Dir,
    },
    Float {
        #[serde(deserialize_with = "deserialize_f64")]
        left: f64,
        #[serde(deserialize_with = "deserialize_f64")]
        right: f64,
        dir: Dir,
    },
    Array {
        left: i32,
        right: i32,
        dir: Dir,
        is_last: bool,
        element_type: Box<Type>,
    },
    #[serde(other)]
    Other,
}

impl From<&Type> for hierarchy::SignalType {
    fn from(typ: &Type) -> Self {
        match typ {
            Type::Bit { .. } => hierarchy::SignalType::Bit,
            Type::Logic { .. } => hierarchy::SignalType::Logic,
            &Type::Discrete { left, right, dir } => {
                let (min, max) = dir.min_max(left, right);
                hierarchy::SignalType::Integer {
                    min,
                    max,
                    direction: dir.into(),
                }
            },
            &Type::Float { left, right, dir } => {
                let (min, max) = dir.min_max(left, right);
                hierarchy::SignalType::Real {
                    min,
                    max,
                    direction: dir.into(),
                }
            },
            &Type::Array {
                left,
                right,
                dir,
                is_last,
                ref element_type,
            } => hierarchy::SignalType::Array {
                left,
                right,
                direction: dir.into(),
                element_type: Box::new(element_type.as_ref().into()),
            },
            Type::Other => todo!(),
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Dir {
    To,
    Downto,
}

impl Dir {
    fn min_max<T>(&self, left: T, right: T) -> (T, T) {
        match self {
            Dir::To => (left, right),
            Dir::Downto => (right, left),
        }
    }
}

impl From<Dir> for hierarchy::Direction {
    fn from(dir: Dir) -> Self {
        match dir {
            Dir::To => hierarchy::Direction::To,
            Dir::Downto => hierarchy::Direction::Downto,
        }
    }
}

fn deserialize_f64<'de, D>(deserializer: D) -> Result<f64, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::Error;

    let string_repr = <&str>::deserialize(deserializer)?;
    let bits_str = string_repr
        .strip_prefix('#')
        .ok_or_else(|| D::Error::custom("missing # prefix"))?;
    let bits = u64::from_str_radix(bits_str, 16).map_err(D::Error::custom)?;
    Ok(f64::from_bits(bits))
}

/// Registers the design hierarchy with the WebSocket server.
#[instrument(skip(state))]
#[unsafe(no_mangle)]
pub extern "C" fn adapter_register_design(
    state: &mut crate::sim_interface::AdapterState,
    root_instance: u32,
    instance_count: u32,
    signal_count: u32,
) {
    info!("Registering design");
    let signals = collect_signals(signal_count);
    let instances = collect_instances(instance_count);

    let root_module = build_module(root_instance, &instances, &signals);
    let hierarchy = hierarchy::DesignHierarchy {
        root_modules: vec![root_module],
    };
    state.set_design_hierarchy(hierarchy, signals);
    info!("design hierarchy built successfully");
}

fn collect_signals(signal_count: u32) -> Vec<Signal> {
    // Signal IDs start at 1, so we put a dummy at index 0
    let mut signals: Vec<Signal> = Vec::with_capacity(1 + signal_count as usize);
    signals.push(Signal {
        decl: 0,
        name: CompactString::new(""),
        typ: Type::Bit {
            left: 0,
            right: 0,
            dir: Dir::To,
        },
    });

    let mut buffer = Vec::with_capacity(4096);
    for signal_id in 1..=signal_count {
        buffer.clear();
        adapter_encode_signal(&mut buffer, signal_id);
        match serde_json::from_slice::<Signal>(&buffer) {
            Ok(mut signal) => {
                if let Some(node_id) = NonZeroU32::new(signal.decl) {
                    buffer.clear();
                    adapter_encode_ast_node(&mut buffer, node_id);
                    let node = serde_json::from_slice::<ast::Node>(&buffer).unwrap();
                    if let ast::Node::SignalDeclaration(signal_declaration) = node {
                        signal.name = signal_declaration.identifier.normalized.0;
                    } else {
                        panic!("Expected signal declaration, got {node:?}");
                    }
                }
                signals.push(signal);
            },

            Err(e) => {
                panic!("Error deserializing signal {signal_id}: {e}");
            },
        }
    }

    signals
}

fn collect_instances(instance_count: u32) -> Vec<Instance> {
    // Collect all instances; instance IDs start at 1, so we put a dummy at index 0
    let mut instances: Vec<Instance> = Vec::with_capacity(1 + instance_count as usize);
    instances.push(Instance {
        stmt: 0,
        source: 0,
        name: CompactString::new(""),
        objects: vec![],
    });

    let mut buffer = Vec::with_capacity(4096);
    for instance_id in 1..=instance_count {
        buffer.clear();
        adapter_encode_instance(&mut buffer, instance_id);
        match serde_json::from_slice::<Instance>(&buffer) {
            Ok(mut instance) => {
                if let Some(node_id) = NonZeroU32::new(instance.stmt) {
                    buffer.clear();
                    adapter_encode_ast_node(&mut buffer, node_id);
                    let node = serde_json::from_slice::<ast::Node>(&buffer).unwrap();
                    match node {
                        ast::Node::ComponentInstantiationStatement(stmt) => {
                            instance.name = stmt.label.original().clone();
                        },
                        _ => {
                            instance.name = CompactString::new("TODO");
                        },
                    }
                }
                instances.push(instance);
            },
            Err(e) => {
                panic!("Error deserializing instance {instance_id}: {e}");
            },
        }
    }

    instances
}

fn build_module(
    instance_id: u32,
    instances: &[Instance],
    all_signals: &[Signal],
) -> hierarchy::Module {
    let instance = &instances[instance_id as usize];
    let name = instance.name.clone();
    let mut submodules = vec![];
    let mut signals = vec![];

    for object in &instance.objects {
        match object {
            ObjectKind::Object {
                val_kind: ValKind::Signal { id: signal_id },
            } => {
                let signal = &all_signals[signal_id.get() as usize];
                signals.push(hierarchy::Signal {
                    name: signal.name.clone(),
                    id: SignalInstanceId(*signal_id),
                    sub_id_start: None,
                    typ: (&signal.typ).into(),
                });
            },
            ObjectKind::Object { .. } => {
                // TODO
            },
            ObjectKind::Instance { id } => {
                let child_module = build_module(*id, instances, all_signals);
                submodules.push(child_module);
            },
        }
    }

    hierarchy::Module {
        name,
        kind: hierarchy::ModuleKind::DesignEntity {
            entity: CompactString::new("TODO"),
            architecture: CompactString::new("TODO"),
        },
        submodules,
        signals,
    }
}

unsafe extern "C" {
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_signal(buffer: &mut Vec<u8>, signal_id: u32);
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_instance(buffer: &mut Vec<u8>, instance_id: u32);
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_ast_node(buffer: &mut Vec<u8>, node_id: NonZeroU32);
}
