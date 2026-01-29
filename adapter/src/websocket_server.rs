#![expect(unused, reason = "// TODO remove before release")]

use std::collections::HashSet;
use std::num::NonZeroU32;
use std::sync::OnceLock;
use std::thread;
use std::time::Duration;

use crossbeam_channel::{Receiver, Sender};
use futures_util::{SinkExt, StreamExt};
use hdl_simulation_protocol::Logic;
use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::SignalValueType;
use hdl_simulation_protocol::SimulationId;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntry;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchyEntryKind;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchySignalType;
use hdl_simulation_protocol::from_simulator::NewValues;
use hdl_simulation_protocol::from_simulator::NewValuesEnum;
use hdl_simulation_protocol::from_simulator::Notification;
use hdl_simulation_protocol::from_simulator::SignalValuesInRange;
use hdl_simulation_protocol::from_simulator::SimulationUpdate;
use hdl_simulation_protocol::time::{Delta, LogicalTime, PhysicalTime};
use hdl_simulation_protocol::to_simulator::Command;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::{mpsc, watch};
use tokio::time::interval;
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::tungstenite::Message;

/// Information about a signal in the mockup design.
#[derive(Debug, Clone)]
struct MockupSignal {
    id: SignalInstanceId,
    value_type: SignalValueType,
}

/// Creates a mockup design hierarchy tree for testing.
///
/// Returns the hierarchy tree and a list of all signals in the design.
fn create_mockup_design_hierarchy_tree() -> (DesignHierarchy, Vec<MockupSignal>) {
    // Build a simple hierarchy:
    // - TopModule
    //   - clk (signal, Logic)
    //   - reset (signal, Logic)
    //   - SubModule_A
    //     - counter (signal, U8)
    //     - data_out (signal, U8)
    //   - SubModule_B
    //     - enable (signal, Logic)
    //     - result (signal, F64)

    let mut signals = Vec::new();

    // Helper to create a signal entry and record it in the signals list
    let mut create_signal = |name: &str, id: u32, value_type: SignalValueType| {
        let signal_id = SignalInstanceId(NonZeroU32::new(id).unwrap());
        signals.push(MockupSignal {
            id: signal_id,
            value_type,
        });
        DesignHierarchyEntry::new(
            name.to_owned(),
            DesignHierarchyEntryKind::Signal(
                signal_id,
                DesignHierarchySignalType::Scalar,
                value_type,
            ),
        )
    };

    let mut root =
        DesignHierarchyEntry::new("TopModule".to_owned(), DesignHierarchyEntryKind::Module);

    // Top-level signals
    root.add_child(create_signal("clk", 1, SignalValueType::Logic));
    root.add_child(create_signal("reset", 2, SignalValueType::Logic));

    // SubModule_A with its signals
    let mut submodule_a =
        DesignHierarchyEntry::new("SubModule_A".to_owned(), DesignHierarchyEntryKind::Module);
    submodule_a.add_child(create_signal("counter", 3, SignalValueType::U8));
    submodule_a.add_child(create_signal("data_out", 4, SignalValueType::U8));
    root.add_child(submodule_a);

    // SubModule_B with its signals
    let mut submodule_b =
        DesignHierarchyEntry::new("SubModule_B".to_owned(), DesignHierarchyEntryKind::Module);
    submodule_b.add_child(create_signal("enable", 5, SignalValueType::Logic));
    submodule_b.add_child(create_signal("result", 6, SignalValueType::F64));
    root.add_child(submodule_b);

    (DesignHierarchy { root }, signals)
}

/// Commands that can be sent from the WebSocket thread to the main simulation thread
#[derive(Debug)]
enum SimulationCommand {
    Start,
    Stop,
}

/// Global state for the WebSocket server
struct WebSocketState {
    /// Receiver for simulation commands (used by FFI functions)
    command_rx: Receiver<SimulationCommand>,
}

static WEBSOCKET_STATE: OnceLock<WebSocketState> = OnceLock::new();

/// Encodes and sends a message over the WebSocket.
async fn send_message(
    ws_sender: &mut futures_util::stream::SplitSink<WebSocketStream<TcpStream>, Message>,
    message: &SimulationUpdate,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let encoded = postcard::to_allocvec(message)?;
    ws_sender.send(Message::Binary(encoded.into())).await?;
    Ok(())
}

/// Handles a single WebSocket connection.
async fn handle_connection(stream: TcpStream, command_tx: Sender<SimulationCommand>) {
    let ws_stream = match tokio_tungstenite::accept_async(stream).await {
        Ok(ws) => ws,
        Err(e) => {
            eprintln!("Error accepting WebSocket connection: {e}");
            return;
        }
    };

    eprintln!("New WebSocket connection established.");

    let (mut ws_sender, mut ws_receiver) = ws_stream.split();

    // Send design hierarchy immediately upon connection
    let (hierarchy, signals) = create_mockup_design_hierarchy_tree();
    eprintln!("Created mockup design with {} signals", signals.len());
    let hierarchy_message = SimulationUpdate {
        simulation_id: 0,
        message: Notification::DesignHierarchy(hierarchy),
    };
    if let Err(e) = send_message(&mut ws_sender, &hierarchy_message).await {
        eprintln!("Failed to send design hierarchy: {e}");
        return;
    }

    let mut simulation_id: SimulationId = 0;

    // Track which signals have active subscriptions
    let mut subscribed_signals: HashSet<SignalInstanceId> = HashSet::new();

    // Watch channel to share subscription state with the mockup generator
    let (subscription_tx, subscription_rx) = watch::channel::<Vec<MockupSignal>>(Vec::new());

    // Channel for sending messages from the mockup generator to the WebSocket sender
    let (mockup_tx, mut mockup_rx) = mpsc::channel::<SimulationUpdate>(32);

    // Spawn the mockup message generator task
    let mockup_handle = tokio::spawn(async move {
        run_mockup_message_generator(mockup_tx, subscription_rx).await;
    });

    loop {
        tokio::select! {
            // Handle incoming WebSocket messages
            Some(message) = ws_receiver.next() => {
                let text = match message {
                    Ok(Message::Text(text)) => text,
                    Ok(Message::Close(_)) => {
                        eprintln!("WebSocket connection closed by client.");
                        break;
                    }
                    Ok(_) => continue,
                    Err(e) => {
                        eprintln!("WebSocket error: {e}");
                        break;
                    }
                };

                let parsed: Result<Command, _> = serde_json::from_str(&text);
                let client_message = match parsed {
                    Ok(msg) => msg,
                    Err(e) => {
                        eprintln!("Failed to parse message: {e}");
                        continue;
                    }
                };

                let response = match client_message {
                    Command::StartSimulation => {
                        simulation_id += 1;
                        let _ = command_tx.send(SimulationCommand::Start);
                        Some(Notification::SimulationStarted)
                    }
                    Command::StopSimulation => {
                        let _ = command_tx.send(SimulationCommand::Stop);
                        Some(Notification::SimulationStopped)
                    }
                    Command::PauseSimulation => {
                        // Not implemented yet, just confirm
                        Some(Notification::SimulationPaused)
                    }
                    Command::ResumeSimulation => {
                        // Not implemented yet, just confirm
                        Some(Notification::SimulationResumed)
                    }
                    Command::RestartSimulation => {
                        simulation_id += 1;
                        let _ = command_tx.send(SimulationCommand::Start);
                        Some(Notification::SimulationStarted)
                    }
                    Command::TrackSignals(request) => {
                        // Update signal subscriptions based on the request
                        for signal_id in &request.signal_instance_ids {
                            if request.subscribe && request.enabled {
                                subscribed_signals.insert(*signal_id);
                                eprintln!("Subscribed to signal {signal_id}");
                            } else {
                                subscribed_signals.remove(signal_id);
                                eprintln!("Unsubscribed from signal {signal_id}");
                            }
                        }
                        eprintln!(
                            "Active subscriptions: {} signals",
                            subscribed_signals.len()
                        );

                        // Update the mockup generator with the current subscribed signals
                        let subscribed: Vec<MockupSignal> = signals
                            .iter()
                            .filter(|s| subscribed_signals.contains(&s.id))
                            .cloned()
                            .collect();
                        let _ = subscription_tx.send(subscribed);

                        None
                    }
                };

                if let Some(response) = response {
                    let response_meta = SimulationUpdate {
                        simulation_id,
                        message: response,
                    };
                    if let Err(e) = send_message(&mut ws_sender, &response_meta).await {
                        eprintln!("Failed to send response: {e}");
                        break;
                    }
                }
            }

            // Handle messages from the mockup generator
            Some(mockup_message) = mockup_rx.recv() => {
                if let Err(e) = send_message(&mut ws_sender, &mockup_message).await {
                    eprintln!("Failed to send mockup message: {e}");
                    break;
                }
            }

            // Exit if the receiver stream or mockup channel is exhausted
            else => break,
        }
    }

    // Abort the mockup generator when the connection closes
    mockup_handle.abort();
}

/// Generates 10 mockup values for a signal starting at the given time.
fn generate_signal_values(
    signal: &MockupSignal,
    start_time: u64,
    time_increment: u64,
    tick_counter: u64,
) -> NewValuesEnum {
    const VALUES_PER_TICK: usize = 10;
    let sub_increment = time_increment / VALUES_PER_TICK as u64;

    let mut timestamps = Vec::with_capacity(VALUES_PER_TICK);
    for i in 0..VALUES_PER_TICK {
        let time = start_time + (i as u64) * sub_increment;
        timestamps.push(LogicalTime::new(PhysicalTime(time), Delta::ZERO));
    }

    let time_range =
        timestamps[0]..LogicalTime::new(PhysicalTime(start_time + time_increment), Delta::ZERO);

    // Generate simple deterministic values based on tick counter and signal ID
    let signal_offset = signal.id.0.get() as u64;

    match signal.value_type {
        SignalValueType::Logic => {
            let values: Vec<Logic> = (0..VALUES_PER_TICK)
                .map(|i| {
                    // Alternate between Zero and One based on index and tick
                    if (tick_counter + i as u64 + signal_offset).is_multiple_of(2) {
                        Logic::Zero
                    } else {
                        Logic::One
                    }
                })
                .collect();
            NewValuesEnum::Logic(NewValues {
                time_range,
                signal_instance_id: signal.id,
                timestamps,
                values,
            })
        }
        SignalValueType::U8 => {
            let values: Vec<u8> = (0..VALUES_PER_TICK)
                .map(|i| {
                    // Simple incrementing pattern wrapping at 256
                    ((tick_counter * VALUES_PER_TICK as u64 + i as u64 + signal_offset) % 256) as u8
                })
                .collect();
            NewValuesEnum::U8(NewValues {
                time_range,
                signal_instance_id: signal.id,
                timestamps,
                values,
            })
        }
        SignalValueType::F64 => {
            let values: Vec<f64> = (0..VALUES_PER_TICK)
                .map(|i| {
                    // Simple sawtooth pattern
                    let step = tick_counter * VALUES_PER_TICK as u64 + i as u64 + signal_offset;
                    (step % 100) as f64 / 10.0
                })
                .collect();
            NewValuesEnum::F64(NewValues {
                time_range,
                signal_instance_id: signal.id,
                timestamps,
                values,
            })
        }
    }
}

/// Runs a mockup message generator that sends `NewSimulationTime` and signal values every 500ms.
async fn run_mockup_message_generator(
    tx: mpsc::Sender<SimulationUpdate>,
    subscription_rx: watch::Receiver<Vec<MockupSignal>>,
) {
    let mut interval = interval(Duration::from_millis(500));
    let mut current_time: u64 = 0;
    let mut tick_counter: u64 = 0;
    // Increment by 10ns (10_000_000 femtoseconds) each tick
    let time_increment: u64 = 10_000_000;

    loop {
        interval.tick().await;

        let logical_time = LogicalTime::new(PhysicalTime(current_time), Delta::ZERO);

        // Send NewSimulationTime message
        let time_message = SimulationUpdate {
            simulation_id: 1,
            message: Notification::NewSimulationTime(logical_time),
        };

        eprintln!("Sending NewSimulationTime: {logical_time}");

        if tx.send(time_message).await.is_err() {
            break;
        }

        // Generate and send signal values for subscribed signals
        let subscribed_signals = subscription_rx.borrow().clone();
        if !subscribed_signals.is_empty() {
            let values_in_range: Vec<NewValuesEnum> = subscribed_signals
                .iter()
                .map(|signal| {
                    generate_signal_values(signal, current_time, time_increment, tick_counter)
                })
                .collect();

            let values_message = SimulationUpdate {
                simulation_id: 1,
                message: Notification::SignalValuesInRange(SignalValuesInRange { values_in_range }),
            };

            eprintln!(
                "Sending signal values for {} signals",
                subscribed_signals.len()
            );

            if tx.send(values_message).await.is_err() {
                break;
            }
        }

        current_time += time_increment;
        tick_counter += 1;
    }
}

/// Runs the async WebSocket server.
async fn run_websocket_server(command_tx: Sender<SimulationCommand>) {
    let addr = "127.0.0.1:8080";
    let listener = match TcpListener::bind(addr).await {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Failed to bind WebSocket server to {addr}: {e}");
            return;
        }
    };

    eprintln!("WebSocket server listening on {addr}");

    loop {
        match listener.accept().await {
            Ok((stream, _)) => {
                let tx = command_tx.clone();
                tokio::spawn(async move {
                    handle_connection(stream, tx).await;
                });
            }
            Err(e) => {
                eprintln!("Failed to accept connection: {e}");
            }
        }
    }
}

/// Initializes the WebSocket server.
///
/// Must be called once before simulation starts.
/// Spawns a background thread running the WebSocket server with a single-threaded tokio runtime.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_init_websocket() {
    // Create channel for simulation commands
    let (tx, rx) = crossbeam_channel::unbounded::<SimulationCommand>();

    // Store the receiver in global state
    WEBSOCKET_STATE
        .set(WebSocketState { command_rx: rx })
        .ok()
        .expect("WebSocket should only be initialized once");

    // Spawn a thread to run the WebSocket server with a single-threaded tokio runtime
    thread::spawn(move || {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime");

        rt.block_on(run_websocket_server(tx));
    });

    eprintln!("WebSocket server thread started");
}

/// Blocks until a StartSimulation command is received from a WebSocket client.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_wait_for_start_simulation() {
    let state = WEBSOCKET_STATE
        .get()
        .expect("adapter_init_websocket should be called first");

    eprintln!("Waiting for start simulation command...");

    loop {
        match state.command_rx.recv() {
            Ok(SimulationCommand::Start) => {
                eprintln!("Received start simulation command");
                return;
            }
            Ok(SimulationCommand::Stop) => {
                // Ignore stop commands while waiting for start
                eprintln!("Ignoring stop command (waiting for start)");
            }
            Err(e) => {
                eprintln!("Channel error while waiting for start: {e}");
                return;
            }
        }
    }
}

/// Blocks until a StopSimulation command is received from a WebSocket client.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_wait_for_stop_simulation() {
    let state = WEBSOCKET_STATE
        .get()
        .expect("adapter_init_websocket should be called first");

    eprintln!("Waiting for stop simulation command...");

    loop {
        match state.command_rx.recv() {
            Ok(SimulationCommand::Stop) => {
                eprintln!("Received stop simulation command");
                return;
            }
            Ok(SimulationCommand::Start) => {
                // Ignore start commands while waiting for stop
                eprintln!("Ignoring start command (waiting for stop)");
            }
            Err(e) => {
                eprintln!("Channel error while waiting for stop: {e}");
                return;
            }
        }
    }
}
