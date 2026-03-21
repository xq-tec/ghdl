#![expect(unused, reason = "// TODO remove before release")]

use std::fmt;
use std::num::NonZeroU32;
use std::ops::Deref;

use compact_str::CompactString;
use compact_str::format_compact;
use ghdl_ast as ast;
use ghdl_ast::GenericNodeId;
use hdl_simulation_protocol::design_hierarchy as hierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalInstanceId;
use rustc_hash::FxHashMap;
use serde::Deserialize;
use tracing::info;
use tracing::instrument;

#[derive(Debug, Deserialize)]
pub struct Signal {
    decl: Option<ast::GenericNodeId>,
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
                // TODO this doesn't work for empty ranges (e.g., 10 downto 11)
                let (min, max) = dir.min_max(left, right);
                hierarchy::SignalType::Integer {
                    min,
                    max,
                    direction: dir.into(),
                }
            },
            &Type::Float { left, right, dir } => {
                // TODO this doesn't work for empty ranges (e.g., 10 downto 11)
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
            } => {
                let direction: hierarchy::Direction = dir.into();
                let element_type: Box<hierarchy::SignalType> =
                    Box::new(element_type.as_ref().into());
                let length = direction.length_for(left, right);
                let element_count = match &*element_type {
                    hierarchy::SignalType::Array { element_count, .. } => length * *element_count,
                    _ => length,
                };

                hierarchy::SignalType::Array {
                    left,
                    right,
                    direction,
                    element_count,
                    element_type,
                }
            },
            Type::Other => hierarchy::SignalType::Unsupported,
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
extern "C" fn adapter_register_design(
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

struct DecodingError {
    json_error: serde_json::Error,
    encoded: String,
}

impl fmt::Debug for DecodingError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "DecodingError({}, '{}')", self.json_error, self.encoded)
    }
}

fn retrieve_signal(signal_id: u32) -> Result<Signal, DecodingError> {
    let mut buffer = Vec::with_capacity(4096);
    adapter_encode_signal(&mut buffer, signal_id);
    serde_json::from_slice::<Signal>(&buffer).map_err(|e| DecodingError {
        json_error: e,
        encoded: String::from_utf8_lossy(&buffer).to_string(),
    })
}

fn get_signal_name(decl_id: ast::GenericNodeId) -> Option<CompactString> {
    let node = retrieve_ast_node(decl_id).ok()?;
    match node {
        ast::Node::SignalDeclaration(signal) => Some(signal.identifier.normalized.0),
        ast::Node::InterfaceSignalDeclaration(signal) => Some(signal.identifier.normalized.0),
        ast::Node::Attribute(attribute) => {
            let prefix = retrieve_ast_node(attribute.prefix).ok()?;
            let prefix: ast::Prefix<'_> = (&prefix).try_into().ok()?;
            let name = match prefix {
                ast::Prefix::SimpleName(simple_name) => simple_name.identifier.original(),
                _ => return None,
            };
            Some(format_compact!("{name}'{kind}", kind = attribute.kind))
        },
        _ => panic!("Expected signal declaration, got {node:?}"),
    }
}

fn retrieve_ast_node(node_id: impl ast::AstNodeId) -> Result<ast::Node, DecodingError> {
    fn inner(node_id: NonZeroU32) -> Result<ast::Node, DecodingError> {
        let mut buffer = Vec::with_capacity(4096);
        adapter_encode_ast_node(&mut buffer, node_id);
        serde_json::from_slice::<ast::Node>(&buffer).map_err(|e| DecodingError {
            json_error: e,
            encoded: String::from_utf8_lossy(&buffer).to_string(),
        })
    }

    inner(node_id.to_raw())
}

fn collect_signals(signal_count: u32) -> Vec<Signal> {
    // Signal IDs start at 1, so we put a dummy at index 0
    let mut signals: Vec<Signal> = Vec::with_capacity(1 + signal_count as usize);
    signals.push(Signal {
        decl: None,
        name: CompactString::new(""),
        typ: Type::Bit {
            left: 0,
            right: 0,
            dir: Dir::To,
        },
    });

    for signal_id in 1..=signal_count {
        match retrieve_signal(signal_id) {
            Ok(mut signal) => {
                if let Some(node_id) = signal.decl
                    && let Some(name) = get_signal_name(node_id)
                {
                    signal.name = name;
                } else {
                    signal.name = CompactString::new("?")
                }
                signals.push(signal);
            },

            Err(e) => {
                panic!("Error deserializing signal {signal_id}: {e:?}");
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
