#![expect(unused, reason = "// TODO remove before release")]

use std::fmt;
use std::num::NonZeroU32;
use std::time::Duration;
use std::time::UNIX_EPOCH;

use compact_str::CompactString;
use compact_str::format_compact;
use ghdl_ast as ast;
use hdl_simulation_protocol::design_hierarchy as hierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalInstanceId;
use serde::Deserialize;
use tracing::debug;
use tracing::info;
use tracing::instrument;

#[derive(Debug, Deserialize)]
pub struct Signal {
    decl: Option<ast::GenericNodeId>,
    #[serde(rename = "type")]
    typ: Type,
}

#[derive(Debug, Deserialize)]
struct Instance {
    #[serde(deserialize_with = "ast::deserialize_optional_node_id")]
    stmt: Option<ast::GenericNodeId>,
    #[serde(deserialize_with = "ast::deserialize_optional_node_id")]
    source: Option<ast::GenericNodeId>,
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
    Enumeration {
        names: Vec<CompactString>,
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
    Record {
        fields: Vec<RecordField>,
    },
    #[serde(other)]
    Other,
}

#[derive(Debug, Deserialize, Clone)]
struct RecordField {
    typ: Type,
    net_offset: u32,
    mem_offset: u64,
    decl: Option<ast::GenericNodeId>,
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
            Type::Enumeration { names } => hierarchy::SignalType::Enumeration {
                names: names.clone(),
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
                let element_count = length * element_type.element_count();

                hierarchy::SignalType::Array {
                    left,
                    right,
                    direction,
                    element_count,
                    element_type,
                }
            },
            Type::Record { fields } => {
                let fields: Vec<_> = fields
                    .iter()
                    .map(|field| {
                        let name = if let Some(node_id) = field.decl {
                            let node = retrieve_ast_node(node_id).unwrap();
                            match node {
                                ast::Node::ElementDeclaration(element) => {
                                    element.identifier.into_original()
                                },
                                ast::Node::RecordElementConstraint(constraint) => {
                                    constraint.identifier.into_original()
                                },
                                _ => CompactString::const_new("<unknown>"),
                            }
                        } else {
                            CompactString::const_new("<unknown>")
                        };
                        hierarchy::RecordField {
                            name,
                            typ: (&field.typ).into(),
                            element_offset: field.net_offset,
                        }
                    })
                    .collect();

                let element_count = fields.iter().map(|field| field.typ.element_count()).sum();
                hierarchy::SignalType::Record {
                    fields,
                    element_count,
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
#[instrument(level = "debug", skip(state))]
#[unsafe(no_mangle)]
extern "C" fn adapter_register_design(
    state: &mut crate::sim_interface::AdapterState,
    root_instance: u32,
    instance_count: u32,
    signal_count: u32,
    name_str: *const u8,
    name_len: u64,
) {
    let signals = collect_signals(signal_count);
    let instances = collect_instances(instance_count);
    let root_module = build_module(root_instance, &instances, &signals);
    let root_modules = root_module.into_iter().collect();

    let name = unsafe { get_string_opt(name_str, name_len) };
    // TODO take time at program start, instead of after elaboration
    let start_time = UNIX_EPOCH.elapsed().unwrap_or(Duration::ZERO).as_secs_f64();
    let hierarchy = hierarchy::DesignHierarchy {
        simulation_id: crate::simulation_id(),
        name,
        start_time,
        root_modules,
    };
    state.set_design_hierarchy(hierarchy, signals);
}

unsafe fn get_string_opt(str: *const u8, len: u64) -> Option<CompactString> {
    if str.is_null() {
        return None;
    }
    let bytes = unsafe { std::slice::from_raw_parts(str, len as usize) };
    str::from_utf8(bytes).ok().map(CompactString::from)
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

fn get_signal_name(decl_id: ast::GenericNodeId) -> Option<CompactString> {
    let node = retrieve_ast_node(decl_id).ok()?;
    match node {
        ast::Node::SignalDeclaration(signal) => Some(signal.identifier.normalized.0),
        ast::Node::InterfaceSignalDeclaration(signal) => Some(signal.identifier.normalized.0),
        ast::Node::Attribute(attribute) => {
            let prefix = retrieve_ast_node(attribute.prefix.into()).ok()?;
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

fn retrieve_ast_node<T>(node_id: ast::NodeId<T>) -> Result<T, DecodingError>
where
    T: TryFrom<ast::Node>,
    <T as TryFrom<ast::Node>>::Error: fmt::Debug,
{
    fn get_node(node_id: NonZeroU32) -> Result<ast::Node, DecodingError> {
        let mut buffer = Vec::with_capacity(4096);
        adapter_encode_ast_node(&mut buffer, node_id);
        serde_json::from_slice::<ast::Node>(&buffer).map_err(|e| DecodingError {
            json_error: e,
            encoded: String::from_utf8_lossy(&buffer).to_string(),
        })
    }

    get_node(node_id.to_raw()).map(|node| node.try_into().expect("node should be of expected type"))
}

fn collect_signals(signal_count: u32) -> Vec<Signal> {
    // Signal IDs start at 1, so we put a dummy at index 0
    let mut signals: Vec<Signal> = Vec::with_capacity(1 + signal_count as usize);
    signals.push(Signal {
        decl: None,
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
            Ok(signal) => signals.push(signal),
            Err(e) => {
                let encoded = String::from_utf8_lossy(&buffer);
                panic!("Error deserializing signal {signal_id}: {e}; encoded: {encoded:?}");
            },
        }
    }

    signals
}

fn collect_instances(instance_count: u32) -> Vec<Instance> {
    // Collect all instances; instance IDs start at 1, so we put a dummy at index 0
    let mut instances: Vec<Instance> = Vec::with_capacity(1 + instance_count as usize);
    instances.push(Instance {
        stmt: None,
        source: None,
        objects: vec![],
    });

    let mut buffer = Vec::with_capacity(4096);
    for instance_id in 1..=instance_count {
        buffer.clear();
        adapter_encode_instance(&mut buffer, instance_id);
        match serde_json::from_slice::<Instance>(&buffer) {
            Ok(instance) => instances.push(instance),
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
) -> Option<hierarchy::Module> {
    let instance = &instances[instance_id as usize];
    let name;
    let kind;

    let stmt = instance
        .stmt
        .map(|stmt_id| retrieve_ast_node(stmt_id).unwrap());
    let source = retrieve_ast_node(instance.source?).ok()?;
    match source {
        ast::Node::ArchitectureBody(arch) => {
            let entity_name = retrieve_ast_node(arch.entity_name).unwrap();
            kind = hierarchy::ModuleKind::DesignEntity {
                entity: entity_name.identifier.into_original(),
                architecture: arch.identifier.into_original(),
            };

            match stmt {
                Some(ast::Node::ComponentInstantiationStatement(stmt)) => {
                    name = Some(stmt.label.into_original());
                },
                _ => name = None,
            }
        },
        ast::Node::ComponentDeclaration(_) => {
            if let &[ObjectKind::Instance { id: inner }] = &*instance.objects {
                return build_module(inner, instances, all_signals);
            } else {
                return None;
            }
        },
        _ => return None,
    }

    let mut submodules = vec![];
    let mut signals = vec![];

    for object in &instance.objects {
        match object {
            ObjectKind::Object {
                val_kind: ValKind::Signal { id: signal_id },
            } => {
                let signal = &all_signals[signal_id.get() as usize];
                let name = if let Some(node_id) = signal.decl
                    && let Some(name) = get_signal_name(node_id)
                {
                    name
                } else {
                    CompactString::const_new("?")
                };
                signals.push(hierarchy::Signal {
                    name,
                    id: SignalInstanceId(*signal_id),
                    typ: (&signal.typ).into(),
                });
            },
            ObjectKind::Object { .. } => {
                // TODO
            },
            ObjectKind::Instance { id } => {
                if let Some(child_module) = build_module(*id, instances, all_signals) {
                    submodules.push(child_module);
                }
            },
        }
    }

    Some(hierarchy::Module {
        name,
        kind,
        submodules,
        signals,
    })
}

unsafe extern "C" {
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_signal(buffer: &mut Vec<u8>, signal_id: u32);
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_instance(buffer: &mut Vec<u8>, instance_id: u32);
    #[expect(improper_ctypes, reason = "opaque pointer")]
    safe fn adapter_encode_ast_node(buffer: &mut Vec<u8>, node_id: NonZeroU32);
}
