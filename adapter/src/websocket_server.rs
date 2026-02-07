use std::collections::HashMap;
use std::collections::HashSet;
use std::pin::Pin;

use crossbeam_channel::Sender;
use futures_util::stream::SelectAll;
use futures_util::{SinkExt, Stream, StreamExt};
use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::SimulationId;
use hdl_simulation_protocol::from_simulator::Notification;
use hdl_simulation_protocol::from_simulator::SimulationUpdate as WsSimulationUpdate;
use hdl_simulation_protocol::to_simulator::Command;
use smallvec::SmallVec;
use tokio::net::{TcpListener, TcpStream};
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::tungstenite;
use tokio_tungstenite::tungstenite::Message;

use crate::SimulationCommand;
use crate::SimulationUpdate;
use crate::sim_interface::DesignHierarchyWithSignals;

type WsSink = futures_util::stream::SplitSink<WebSocketStream<TcpStream>, Message>;
type TaggedWsStream =
    Pin<Box<dyn Stream<Item = (u64, Result<Message, tungstenite::Error>)> + Send>>;
type SendError = Box<dyn std::error::Error + Send + Sync>;

/// State for a single WebSocket connection.
struct Connection {
    id: u64,
    sink: WsSink,
    subscribed_signals: HashSet<SignalInstanceId>,
    simulation_id: SimulationId,
}

impl Connection {
    /// Encodes and sends a protocol message over the WebSocket.
    async fn send(&mut self, message: &WsSimulationUpdate) -> Result<(), SendError> {
        let encoded = postcard::to_allocvec(message)?;
        self.sink.send(Message::Binary(encoded.into())).await?;
        Ok(())
    }

    /// Wraps a notification with this connection's simulation ID and sends it.
    async fn send_notification(&mut self, notification: Notification) -> Result<(), SendError> {
        let message = WsSimulationUpdate {
            simulation_id: self.simulation_id,
            message: notification,
        };
        self.send(&message).await
    }

    /// Processes a client command, forwarding simulation commands to the simulator
    /// thread and returning an optional response notification.
    fn handle_command(
        &mut self,
        command: Command,
        command_tx: &Sender<SimulationCommand>,
    ) -> Option<Notification> {
        match command {
            Command::StartSimulation => {
                self.simulation_id += 1;
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
                self.simulation_id += 1;
                let _ = command_tx.send(SimulationCommand::Start);
                Some(Notification::SimulationStarted)
            }
            Command::TrackSignals(request) => {
                let mut to_subscribe: SmallVec<[SignalInstanceId; 1]> = SmallVec::new();
                for &signal_id in &request.signal_instance_ids {
                    if request.subscribe && request.enabled {
                        self.subscribed_signals.insert(signal_id);
                        to_subscribe.push(signal_id);
                        eprintln!("Connection {}: Subscribed to signal {signal_id}", self.id);
                    } else {
                        self.subscribed_signals.remove(&signal_id);
                        eprintln!(
                            "Connection {}: Unsubscribed from signal {signal_id}",
                            self.id
                        );
                    }
                }
                if !to_subscribe.is_empty() {
                    command_tx
                        .send(SimulationCommand::Subscribe(to_subscribe))
                        .unwrap();
                }
                eprintln!(
                    "Connection {}: {} signals subscribed",
                    self.id,
                    self.subscribed_signals.len()
                );
                None
            }
        }
    }
}

/// Sends a notification to all connections, removing any that fail.
async fn broadcast(connections: &mut HashMap<u64, Connection>, notification: Notification) {
    let mut to_remove = Vec::new();
    for conn in connections.values_mut() {
        if let Err(e) = conn.send_notification(notification.clone()).await {
            eprintln!("Failed to send to connection {}: {e}", conn.id);
            to_remove.push(conn.id);
        }
    }
    for id in to_remove {
        connections.remove(&id);
    }
}

/// Runs the async WebSocket server with all connections handled in a single event loop.
pub(crate) async fn run_websocket_server(
    command_tx: Sender<SimulationCommand>,
    mut update_rx: tokio::sync::mpsc::UnboundedReceiver<SimulationUpdate>,
) {
    let addr = "127.0.0.1:8080";
    let listener = match TcpListener::bind(addr).await {
        Ok(l) => l,
        Err(e) => {
            eprintln!("Failed to bind WebSocket server to {addr}: {e}");
            return;
        }
    };

    eprintln!("WebSocket server listening on {addr}");

    // Connection state management
    let mut connections: HashMap<u64, Connection> = HashMap::new();
    type TaggedWsStream = std::pin::Pin<
        Box<
            dyn futures_util::Stream<
                    Item = (u64, Result<Message, tokio_tungstenite::tungstenite::Error>),
                > + Send,
        >,
    >;
    let mut ws_receivers: SelectAll<TaggedWsStream> = SelectAll::new();
    let mut next_connection_id: u64 = 0;

    // Stored design hierarchy for new connections
    let mut current_hierarchy: Option<DesignHierarchyWithSignals> = None;

    loop {
        tokio::select! {
            // Accept new connections
            result = listener.accept() => {
                let (stream, _) = match result {
                    Ok(pair) => pair,
                    Err(e) => {
                        eprintln!("Failed to accept connection: {e}");
                        continue;
                    }
                };
                let ws_stream = match tokio_tungstenite::accept_async(stream).await {
                    Ok(ws) => ws,
                    Err(e) => {
                        eprintln!("Error during WebSocket handshake: {e}");
                        continue;
                    }
                };

                let id = next_id;
                next_id += 1;
                eprintln!("New WebSocket connection {id} established.");

                let (sink, stream) = ws_stream.split();
                let mut conn = Connection {
                    id,
                    sink,
                    subscribed_signals: HashSet::new(),
                    simulation_id: 0,
                };

                // Send current design hierarchy to the newly connected client
                if let Some(ref data) = current_hierarchy {
                    let notification = Notification::DesignHierarchy(data.hierarchy.clone());
                    if let Err(e) = conn.send_notification(notification).await {
                        eprintln!("Failed to send design hierarchy to connection {id}: {e}");
                        continue;
                    }
                }

                connections.insert(id, conn);
                ws_receivers.push(Box::pin(stream.map(move |msg| (id, msg))));
            }

            // Handle messages from any connection
            Some((id, result)) = ws_receivers.next() => {
                let text = match result {
                    Ok(Message::Text(text)) => text,
                    Ok(Message::Close(_)) => {
                        eprintln!("Connection {id} closed by client.");
                        connections.remove(&id);
                        continue;
                    }
                    Ok(_) => continue,
                    Err(e) => {
                        eprintln!("WebSocket error on connection {id}: {e}");
                        connections.remove(&id);
                        continue;
                    }
                };

                let command: Command = match serde_json::from_str(&text) {
                    Ok(cmd) => cmd,
                    Err(e) => {
                        eprintln!("Failed to parse message from connection {id}: {e}");
                        continue;
                    }
                };

                let Some(conn) = connections.get_mut(&id) else { continue };
                let response = conn.handle_command(command, &command_tx);

                if let Some(notification) = response
                    && let Err(e) = conn.send_notification(notification).await {
                        eprintln!("Failed to send response to connection {id}: {e}");
                        connections.remove(&id);
                    }
            }

            // Handle simulation updates from the simulator thread
            Some(update) = update_rx.recv() => {
                match update {
                    SimulationUpdate::Design(data) => {
                        eprintln!(
                            "Broadcasting design hierarchy to {} connections",
                            connections.len()
                        );

                        // Store the hierarchy for new connections
                        current_hierarchy = Some(data.clone());

                        // Clear subscriptions since signal IDs may have changed
                        for conn in connections.values_mut() {
                            conn.subscribed_signals.clear();
                        }

                        broadcast(
                            &mut connections,
                            Notification::DesignHierarchy(data.hierarchy),
                        )
                        .await;
                    }
                    SimulationUpdate::SignalValuesInRange(values) => {
                        eprintln!(
                            "Broadcasting signal values to {} connections",
                            connections.len()
                        );

                        broadcast(
                            &mut connections,
                            Notification::SignalValuesInRange(values),
                        )
                        .await;
                    }
                }
            }
        }
    }
}
