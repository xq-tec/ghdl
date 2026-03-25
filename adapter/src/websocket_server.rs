use std::collections::HashSet;
use std::fs::File;
use std::future::pending;
use std::io;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Once;
use std::sync::OnceLock;
use std::time::Duration;

use crossbeam_channel::Sender;
use futures_util::SinkExt;
use futures_util::StreamExt;
use futures_util::stream::SplitStream;
use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalElementId;
use hdl_simulation_protocol::from_simulator::SimulationUpdate as WsSimulationUpdate;
use hdl_simulation_protocol::server_marker;
use hdl_simulation_protocol::to_simulator::Command;
use smallvec::SmallVec;
use tokio::net::TcpListener;
use tokio::net::TcpStream;
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::tungstenite;
use tokio_tungstenite::tungstenite::Message;
use tracing::debug;
use tracing::error;
use tracing::info;
use tracing::info_span;
use tracing::instrument;
use tracing::warn;

use crate::SimulationCommand;
use crate::SimulationUpdate;

/// Path for `libc::atexit` cleanup. Stale files may remain after `SIGKILL` or crash.
static SERVER_MARKER_PATH: OnceLock<PathBuf> = OnceLock::new();

static REGISTER_SERVER_MARKER_ATEXIT: Once = Once::new();

extern "C" fn remove_server_marker_atexit() {
    if let Some(path) = SERVER_MARKER_PATH.get() {
        let _ = std::fs::remove_file(path);
    }
}

/// Creates an empty `{port}-{simulation_id:014x}.server` file and registers a one-time `atexit` handler to remove it.
fn create_server_marker_and_register_cleanup(port: u16, simulation_id: u64) -> io::Result<()> {
    let dir = server_marker::markers_directory();
    std::fs::create_dir_all(&dir)?;
    let path = server_marker::marker_path(port, simulation_id);
    File::create(&path)?;
    let _ = SERVER_MARKER_PATH.set(path);
    REGISTER_SERVER_MARKER_ATEXIT.call_once(|| unsafe {
        libc::atexit(remove_server_marker_atexit);
    });
    Ok(())
}

/// Returns a uniform random 53-bit simulation instance identifier.
fn random_simulation_id() -> u64 {
    let mut bytes = [0u8; 8];
    if getrandom::fill(&mut bytes).is_err() {
        let time = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or(1u64, |d| d.as_nanos() as u64);
        bytes = time.to_ne_bytes();
    }
    u64::from_ne_bytes(bytes) & ((1u64 << 53) - 1)
}

type WsSink = futures_util::stream::SplitSink<WebSocketStream<TcpStream>, Message>;
type WsRecv = SplitStream<WebSocketStream<TcpStream>>;
type SendError = Box<dyn std::error::Error + Send + Sync>;

/// Send half and subscription state for the single allowed WebSocket client.
///
/// The receive half is stored separately so `tokio::select!` can await
/// `recv.next()` without holding a borrow of this struct across polls of
/// `listener.accept()`.
struct ClientSession {
    sink: WsSink,
    subscribed_signals: HashSet<SignalElementId>,
}

impl ClientSession {
    /// Encodes and sends a protocol message over the WebSocket.
    #[instrument(skip(self, message), level = "debug")]
    async fn send(&mut self, message: &WsSimulationUpdate) -> Result<(), SendError> {
        let encoded = postcard::to_allocvec(message)?;
        self.sink.send(Message::Binary(encoded.into())).await?;
        Ok(())
    }

    /// Processes a client command, forwarding simulation commands to the simulator
    /// thread and returning an optional response notification.
    #[instrument(skip(self, command_tx), level = "debug")]
    fn handle_command(
        &mut self,
        command: Command,
        command_tx: &Sender<SimulationCommand>,
    ) -> Option<WsSimulationUpdate> {
        match command {
            Command::StartSimulation => {
                let _ = command_tx.send(SimulationCommand::Start);
                Some(WsSimulationUpdate::SimulationStarted)
            },
            Command::StopSimulation => {
                let _ = command_tx.send(SimulationCommand::Stop);
                Some(WsSimulationUpdate::SimulationStopped)
            },
            Command::PauseSimulation => {
                // Not implemented yet, just confirm
                Some(WsSimulationUpdate::SimulationPaused)
            },
            Command::ResumeSimulation => {
                // Not implemented yet, just confirm
                Some(WsSimulationUpdate::SimulationResumed)
            },
            Command::RestartSimulation => {
                let _ = command_tx.send(SimulationCommand::Start);
                Some(WsSimulationUpdate::SimulationStarted)
            },
            Command::TrackSignals(request) => {
                let mut to_subscribe: SmallVec<[SignalElementId; 1]> = SmallVec::new();
                for &element_id in &request.signal_element_ids {
                    if request.subscribe && request.enabled {
                        self.subscribed_signals.insert(element_id);
                        to_subscribe.push(element_id);
                        debug!(?element_id, "subscribed to signal");
                    } else {
                        self.subscribed_signals.remove(&element_id);
                        debug!(?element_id, "unsubscribed from signal");
                    }
                }
                if !to_subscribe.is_empty() {
                    command_tx
                        .send(SimulationCommand::Subscribe(to_subscribe))
                        .unwrap();
                }
                debug!(
                    count = self.subscribed_signals.len(),
                    "signal subscription count updated",
                );
                None
            },
        }
    }
}

fn disconnect_client(session: &mut Option<ClientSession>, recv: &mut Option<WsRecv>) {
    *session = None;
    *recv = None;
}

/// Sends a notification to the connected client, clearing the slot on failure.
#[instrument(skip_all, level = "debug")]
async fn send_to_client(
    session: &mut Option<ClientSession>,
    recv: &mut Option<WsRecv>,
    update: &WsSimulationUpdate,
) {
    let Some(client) = session.as_mut() else {
        return;
    };
    if let Err(e) = client.send(update).await {
        warn!("failed to send to client: {e}");
        disconnect_client(session, recv);
    }
}

/// Runs the async WebSocket server with a single client handled in one event loop.
#[instrument(skip_all)]
pub(crate) async fn run_websocket_server(
    command_tx: Sender<SimulationCommand>,
    mut update_rx: tokio::sync::mpsc::UnboundedReceiver<SimulationUpdate>,
) {
    let bind_addr = "127.0.0.1:0";
    let listener = match TcpListener::bind(bind_addr).await {
        Ok(l) => l,
        Err(e) => {
            error!(addr = bind_addr, "failed to bind WebSocket server: {e}");
            return;
        },
    };

    let addr = match listener.local_addr() {
        Ok(a) => a,
        Err(e) => {
            error!("failed to read WebSocket bind address: {e}");
            return;
        },
    };

    let simulation_id = random_simulation_id();
    if let Err(e) = create_server_marker_and_register_cleanup(addr.port(), simulation_id) {
        error!(%addr, "failed to create server marker file: {e}");
        return;
    }

    info!(%addr, "WebSocket server listening");

    let mut client_session: Option<ClientSession> = None;
    let mut client_recv: Option<WsRecv> = None;
    let mut current_hierarchy: Option<DesignHierarchy> = None;
    let mut update_interval = tokio::time::interval(Duration::from_millis(100));

    loop {
        tokio::select! {
            // Periodically request a signal-value flush from the simulator
            _ = update_interval.tick() => {
                let _ = command_tx.send(SimulationCommand::SendUpdate);
            }

            // Accept new connections (only when no client is connected)
            accept_result = async {
                if client_session.is_none() {
                    listener.accept().await
                } else {
                    pending::<io::Result<(TcpStream, SocketAddr)>>().await
                }
            } => {
                let (stream, _) = match accept_result {
                    Ok(pair) => pair,
                    Err(e) => {
                        warn!("failed to accept connection: {e}");
                        continue;
                    }
                };
                let ws_stream = match tokio_tungstenite::accept_async(stream).await {
                    Ok(ws) => ws,
                    Err(e) => {
                        warn!("error during WebSocket handshake: {e}");
                        continue;
                    }
                };

                let connection_span = info_span!("client");
                let (sink, stream) = ws_stream.split();
                let mut session = ClientSession {
                    sink,
                    subscribed_signals: HashSet::new(),
                };

                {
                    let _enter = connection_span.enter();
                    info!("WebSocket connection established");

                    // Send current design hierarchy to the newly connected client
                    if let Some(ref hierarchy) = current_hierarchy {
                        let update = WsSimulationUpdate::DesignHierarchy(hierarchy.clone());
                        if let Err(e) = session.send(&update).await {
                            warn!("failed to send design hierarchy: {e}");
                            continue;
                        }
                    }
                }

                client_session = Some(session);
                client_recv = Some(stream);
            }

            // Handle messages from the connected client
            ws_item = async {
                match &mut client_recv {
                    Some(s) => s.next().await,
                    None => pending::<Option<Result<Message, tungstenite::Error>>>().await,
                }
            } => {
                let connection_span = info_span!("client");
                let _enter = connection_span.enter();

                let Some(result) = ws_item else {
                    info!("WebSocket stream ended");
                    disconnect_client(&mut client_session, &mut client_recv);
                    continue;
                };

                let text = match result {
                    Ok(Message::Text(text)) => text,
                    Ok(Message::Close(_)) => {
                        info!("connection closed by client");
                        disconnect_client(&mut client_session, &mut client_recv);
                        continue;
                    },
                    Ok(_) => continue,
                    Err(e) => {
                        warn!("WebSocket error: {e}");
                        disconnect_client(&mut client_session, &mut client_recv);
                        continue;
                    },
                };

                let command: Command = match serde_json::from_str(&text) {
                    Ok(cmd) => cmd,
                    Err(e) => {
                        warn!("failed to parse message: {e}");
                        continue;
                    },
                };

                let Some(session) = client_session.as_mut() else { continue };
                let response = session.handle_command(command, &command_tx);

                if let Some(update) = response
                    && let Err(e) = session.send(&update).await {
                        warn!("failed to send response: {e}");
                        disconnect_client(&mut client_session, &mut client_recv);
                    }
            }

            // Handle simulation updates from the simulator thread
            Some(update) = update_rx.recv() => {
                match update {
                    SimulationUpdate::Design(mut hierarchy) => {
                        hierarchy.simulation_id = simulation_id;
                        current_hierarchy = Some(hierarchy.clone());

                        // Clear subscriptions since signal IDs may have changed
                        if let Some(s) = client_session.as_mut() {
                            s.subscribed_signals.clear();
                        }

                        send_to_client(
                            &mut client_session,
                            &mut client_recv,
                            &WsSimulationUpdate::DesignHierarchy(hierarchy),
                        )
                        .await;
                    },
                    SimulationUpdate::Events(values) => {
                        send_to_client(
                            &mut client_session,
                            &mut client_recv,
                            &WsSimulationUpdate::Events(values),
                        )
                        .await;
                    },
                    SimulationUpdate::StatusChanged(status, ack_tx) => {
                        let update = match status {
                            SimulationStatus::Paused => WsSimulationUpdate::SimulationPaused,
                            SimulationStatus::Running => WsSimulationUpdate::SimulationResumed,
                            SimulationStatus::Stopped => WsSimulationUpdate::SimulationStopped,
                        };
                        send_to_client(&mut client_session, &mut client_recv, &update).await;
                        if let Some(tx) = ack_tx {
                            let _ = tx.send(());
                        }
                    },
                }
            }
        }
    }
}
