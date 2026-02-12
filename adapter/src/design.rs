#![expect(unused, reason = "// TODO remove before release")]

use std::num::NonZeroU32;

use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::SignalValueType;
use serde::Deserialize;

use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntry;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntryKind;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchySignalType;

#[derive(Debug, Deserialize)]
pub struct Signal {
    decl: u32,
    name: Option<String>,
    pub type_kind: TypeKind,
}

#[derive(Debug, Deserialize)]
struct Instance {
    stmt: u32,
    source: u32,
    name: Option<String>,
    objects: Vec<ObjectKind>,
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ObjectKind {
    Object {
        val_kind: ValKind,
        name: Option<String>,
    },
    Instance {
        id: u32,
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ValKind {
    Signal {
        id: u32,
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

    let name = instance
        .name
        .clone()
        .unwrap_or_else(|| format!("instance_{instance_id}"));

    let mut entry = DesignHierarchyEntry::new(name, DesignHierarchyEntryKind::Module);

    for obj in &instance.objects {
        match obj {
            ObjectKind::Object {
                val_kind: ValKind::Signal { id },
                name,
            } => {
                let signal_name = name.clone().unwrap_or_else(|| format!("signal_{id}"));

                if let Some(non_zero_id) = NonZeroU32::new(*id) {
                    // Get the signal type from the signals list
                    let type_kind = &signals[*id as usize].type_kind;
                    let signal_entry = DesignHierarchyEntry::new(
                        signal_name,
                        DesignHierarchyEntryKind::Signal(
                            SignalInstanceId(non_zero_id),
                            DesignHierarchySignalType::Scalar,
                            type_kind.to_value_type(),
                        ),
                    );
                    entry.add_child(signal_entry);
                }
            }
            ObjectKind::Object {
                val_kind: ValKind::Memory,
                name,
            } => {
                // TODO
            }
            ObjectKind::Instance { id } => {
                let child_entry = build_instance_entry(*id, instances, signals);
                entry.add_child(child_entry);
            }
            ObjectKind::Object {
                val_kind: ValKind::Other,
                name,
            } => {
                // TODO
            }
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
        name: None,
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
            Ok(signal) => signals.push(signal),
            Err(e) => {
                panic!("Error deserializing signal {signal_id}: {e}");
            }
        }
    }

    // Collect all instances; instance IDs start at 1, so we put a dummy at index 0
    let mut instances: Vec<Instance> = Vec::with_capacity(1 + instance_count as usize);
    instances.push(Instance {
        stmt: 0,
        source: 0,
        name: None,
        objects: vec![],
    });
    for instance_id in 1..=instance_count {
        buffer.clear();
        adapter_encode_instance(&mut buffer, instance_id);
        match serde_json::from_slice::<Instance>(&buffer) {
            Ok(instance) => instances.push(instance),
            Err(e) => {
                panic!("Error deserializing instance {instance_id}: {e}");
            }
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
}
