#![expect(unused, reason = "// TODO remove before release")]

use std::collections::HashMap;
use std::num::NonZeroU32;

use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::SignalValueType;
use serde::Deserialize;

use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntry;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntryKind;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchySignalType;

#[derive(Debug, Deserialize)]
struct Signal {
    decl: u32,
    name: Option<String>,
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
        type_kind: TypeKind,
        name: Option<String>,
    },
    Instance {
        id: u32,
    },
}

#[derive(Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
enum ValKind {
    Signal { id: u32 },
}

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
enum TypeKind {
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
}

#[derive(Debug, Deserialize, Clone)]
#[serde(rename_all = "snake_case")]
enum Dir {
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

/// Converts a GHDL TypeKind to a SignalValueType for the protocol.
fn type_kind_to_signal_value_type(type_kind: &TypeKind) -> SignalValueType {
    match type_kind {
        TypeKind::Bit { .. } => SignalValueType::Logic,
        TypeKind::Logic { .. } => SignalValueType::Logic,
        TypeKind::Discrete { .. } => SignalValueType::U8,
        TypeKind::Float { .. } => SignalValueType::F64,
    }
}

/// Builds a DesignHierarchyTreeEntry for an instance recursively.
fn build_instance_entry(
    instance_id: u32,
    instances: &HashMap<u32, Instance>,
    signal_types: &HashMap<u32, TypeKind>,
) -> DesignHierarchyEntry {
    let instance = instances.get(&instance_id);

    let name = instance
        .and_then(|i| i.name.clone())
        .unwrap_or_else(|| format!("instance_{instance_id}"));

    let mut entry = DesignHierarchyEntry::new(name, DesignHierarchyEntryKind::Module);

    if let Some(inst) = instance {
        for obj in &inst.objects {
            match obj {
                ObjectKind::Object {
                    val_kind: ValKind::Signal { id },
                    type_kind,
                    name,
                } => {
                    let signal_name = name.clone().unwrap_or_else(|| format!("signal_{id}"));

                    if let Some(non_zero_id) = NonZeroU32::new(*id) {
                        let signal_entry = DesignHierarchyEntry::new(
                            signal_name,
                            DesignHierarchyEntryKind::Signal(
                                SignalInstanceId(non_zero_id),
                                DesignHierarchySignalType::Scalar,
                                type_kind_to_signal_value_type(type_kind),
                            ),
                        );
                        entry.add_child(signal_entry);
                    }
                }
                ObjectKind::Instance { id } => {
                    let child_entry = build_instance_entry(*id, instances, signal_types);
                    entry.add_child(child_entry);
                }
            }
        }
    }

    entry
}

/// Registers the design hierarchy with the WebSocket server.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_register_design(
    ws_state: &crate::websocket_server::WebSocketState,
    root_instance: u32,
    instance_count: u32,
    signal_count: u32,
) {
    let mut buffer = Vec::with_capacity(4096);
    let mut signal_types: HashMap<u32, TypeKind> = HashMap::new();
    let mut instances: HashMap<u32, Instance> = HashMap::new();

    eprintln!(
        "Registering design: root_instance={root_instance}, instances={instance_count}, signals={signal_count}"
    );

    // First pass: collect all signals
    for signal_id in 1..=signal_count {
        buffer.clear();
        adapter_encode_signal(&mut buffer, signal_id);
        if let Ok(signal) = serde_json::from_slice::<Signal>(&buffer) {
            eprintln!("signal {signal_id}: {:?}", signal);
        }
    }

    // Second pass: collect all instances
    for instance_id in 1..=instance_count {
        buffer.clear();
        adapter_encode_instance(&mut buffer, instance_id);
        match serde_json::from_slice::<Instance>(&buffer) {
            Ok(instance) => {
                // Collect signal types from this instance
                for obj in &instance.objects {
                    if let ObjectKind::Object {
                        val_kind: ValKind::Signal { id },
                        type_kind,
                        ..
                    } = obj
                    {
                        signal_types.insert(*id, type_kind.clone());
                    }
                }
                eprintln!("instance {instance_id}: {:?}", instance.name);
                instances.insert(instance_id, instance);
            }
            Err(e) => {
                eprintln!("Error deserializing instance {instance_id}: {e}");
            }
        }
    }

    // Build the design hierarchy tree starting from the root instance
    let root_entry = build_instance_entry(root_instance, &instances, &signal_types);

    let tree = DesignHierarchy { root: root_entry };

    eprintln!("Design hierarchy tree built successfully");

    // Store the tree in the server state
    crate::websocket_server::set_design_hierarchy(ws_state, tree);
}

unsafe extern "C" {
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_signal(buffer: &mut Vec<u8>, signal_id: u32);
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_instance(buffer: &mut Vec<u8>, instance_id: u32);
}
