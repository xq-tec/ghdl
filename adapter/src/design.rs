#![expect(unused, reason = "// TODO remove before release")]

use std::fmt;
use std::num::NonZeroU32;
use std::ops::Deref;

use compact_str::CompactString;
use ghdl_ast as ast;
use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::SignalValueType;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntry;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntryKind;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchySignalType;
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct Signal {
    decl: u32,
    #[serde(skip_deserializing)]
    name: CompactString,
    type_kind: TypeKind,
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
pub enum TypeKind {
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
    #[serde(other)]
    Other,
}

impl TypeKind {
    pub fn to_value_type(&self) -> SignalValueType {
        match self {
            TypeKind::Bit { .. } => SignalValueType::U8,
            TypeKind::Logic { .. } => SignalValueType::Logic,
            TypeKind::Discrete { .. } => SignalValueType::U8,
            TypeKind::Float { .. } => SignalValueType::F64,
            TypeKind::Other => SignalValueType::U8, // TODO
        }
    }
}

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
pub enum Dir {
    To,
    Downto,
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

/// Builds a DesignHierarchyTreeEntry for an instance recursively.
///
/// Takes the instance ID, the list of all instances (indexed by ID), and the list
/// of all signals (indexed by ID). Both lists have a dummy element at index 0 since
/// IDs start at 1.
fn build_instance_entry(
    instance_id: u32,
    instances: &[Instance],
    signals: &[Signal],
) -> DesignHierarchyEntry {
    let instance = &instances[instance_id as usize];

    let name = if instance.name.is_empty() {
        compact_str::format_compact!("instance_{instance_id}")
    } else {
        instance.name.clone()
    };

    let mut entry = DesignHierarchyEntry::new(name, DesignHierarchyEntryKind::Module);

    for obj in &instance.objects {
        match obj {
            &ObjectKind::Object {
                val_kind: ValKind::Signal { id: signal_id },
            } => {
                let signal = &signals[signal_id.get() as usize];
                let signal_entry = DesignHierarchyEntry::new(
                    signal.name.clone(),
                    DesignHierarchyEntryKind::Signal(
                        SignalInstanceId(signal_id),
                        DesignHierarchySignalType::Scalar,
                        signal.type_kind.to_value_type(),
                    ),
                );
                entry.add_child(signal_entry);
            },
            ObjectKind::Object {
                val_kind: ValKind::Memory,
            } => {
                // TODO
            },
            ObjectKind::Instance { id } => {
                let child_entry = build_instance_entry(*id, instances, signals);
                entry.add_child(child_entry);
            },
            ObjectKind::Object {
                val_kind: ValKind::Other,
            } => {
                // TODO
            },
        }
    }

    entry
}

/// Registers the design hierarchy with the WebSocket server.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_register_design(
    state: &mut crate::sim_interface::AdapterState,
    root_instance: u32,
    instance_count: u32,
    signal_count: u32,
) {
    let mut buffer = Vec::with_capacity(4096);

    eprintln!(
        "Registering design: root_instance={root_instance}, instances={instance_count}, signals={signal_count}"
    );

    // Collect all signals; signal IDs start at 1, so we put a dummy at index 0
    let mut signals: Vec<Signal> = Vec::with_capacity(1 + signal_count as usize);
    signals.push(Signal {
        decl: 0,
        name: CompactString::new(""),
        type_kind: TypeKind::Bit {
            left: 0,
            right: 0,
            dir: Dir::To,
        },
    });
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

    // Collect all instances; instance IDs start at 1, so we put a dummy at index 0
    let mut instances: Vec<Instance> = Vec::with_capacity(1 + instance_count as usize);
    instances.push(Instance {
        stmt: 0,
        source: 0,
        name: CompactString::new(""),
        objects: vec![],
    });
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
                            // TODO
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

    // Build the design hierarchy tree starting from the root instance
    let root_entry = build_instance_entry(root_instance, &instances, &signals);

    let hierarchy = DesignHierarchy { root: root_entry };

    eprintln!("Design hierarchy tree built successfully");

    state.set_design_hierarchy(hierarchy, signals);
}

unsafe extern "C" {
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_signal(buffer: &mut Vec<u8>, signal_id: u32);
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_instance(buffer: &mut Vec<u8>, instance_id: u32);
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_ast_node(buffer: &mut Vec<u8>, node_id: NonZeroU32);
}
