use std::fs::File;
use std::future::pending;
use std::io;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Once;
use std::sync::OnceLock;
use std::time::Duration;

use crossbeam_channel::Sender as SyncSender;
use futures_util::SinkExt;
use futures_util::StreamExt;
use hdl_simulation_protocol::SimulationId;
use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::from_simulator::SimulationUpdate as WsSimulationUpdate;
use hdl_simulation_protocol::server_marker;
use hdl_simulation_protocol::to_simulator::Command;
use tokio::net::TcpListener;
use tokio::net::TcpStream;
use tokio::sync::mpsc::Receiver as AsyncReceiver;
use tokio::time::MissedTickBehavior;
use tokio_tungstenite::WebSocketStream;
use tokio_tungstenite::tungstenite;
use tokio_tungstenite::tungstenite::Message;
use tracing::debug;
use tracing::error;
use tracing::info_span;
use tracing::instrument;
use tracing::warn;

use crate::SIMULATION_ID;
use crate::SimulationCommand;
use crate::SimulationUpdate;

/// The soft maximum on the number of events per update.
///
/// A WebSocket message is limited to 16MiB, and one event takes at most 24 bytes
/// (less in practice due to efficient encoding).
/// The threshold should be set well below `16 * 2**20 / 24 = 699050` for a safety margin.
pub const EVENTS_PER_UPDATE_THRESHOLD: usize = 500_000;

/// Path for `libc::atexit` cleanup. Stale files may remain after `SIGKILL` or crash.
static SERVER_MARKER_PATH: OnceLock<PathBuf> = OnceLock::new();

static REGISTER_SERVER_MARKER_ATEXIT: Once = Once::new();

extern "C" fn remove_server_marker_atexit() {
    if let Some(path) = SERVER_MARKER_PATH.get() {
        let _ = std::fs::remove_file(path);
    }
}

/// Creates an empty `{port}-{simulation_id:014x}.server` file and registers a one-time `atexit` handler to remove it.
fn create_server_marker_and_register_cleanup(
    port: u16,
    simulation_id: SimulationId,
) -> io::Result<()> {
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

/// Send half and subscription state for the single allowed WebSocket client.
struct ClientSession {
    stream: WebSocketStream<TcpStream>,
}

impl ClientSession {
    /// Encodes and sends a protocol message over the WebSocket.
    async fn send(
        &mut self,
        message: &WsSimulationUpdate,
    ) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        let encoded = postcard::to_allocvec(message)?;
        self.stream.send(Message::Binary(encoded.into())).await?;
        Ok(())
    }
}

/// Sends a notification to the connected client, clearing the slot on failure.
async fn send_to_client(session: &mut Option<ClientSession>, update: &WsSimulationUpdate) {
    let Some(client) = session.as_mut() else {
        return;
    };
    if let Err(error) = client.send(update).await {
        warn!(%error, "failed to send to client");
        *session = None;
    }
}

/// Runs the async WebSocket server with a single client handled in one event loop.
#[instrument(level = "debug", skip_all)]
pub(crate) async fn run_websocket_server(
    command_tx: SyncSender<SimulationCommand>,
    mut update_rx: AsyncReceiver<SimulationUpdate>,
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

    if let Err(e) = create_server_marker_and_register_cleanup(addr.port(), *SIMULATION_ID) {
        error!(%addr, "failed to create server marker file: {e}");
        return;
    }

    debug!(%addr, "WebSocket server listening");

    let mut client_session: Option<ClientSession> = None;
    let mut design_hierarchy: Option<DesignHierarchy> = None;
    let mut update_interval = tokio::time::interval(Duration::from_millis(100));
    update_interval.set_missed_tick_behavior(MissedTickBehavior::Delay);
    let mut simulation_status = None;

    loop {
        /// Stores the result of the select!, so that we can put the actual logic outside of the
        /// macro (rustfmt doesn't work in macros).
        enum SelectBranch {
            UpdateTick,
            AcceptConnection(io::Result<(TcpStream, SocketAddr)>),
            ClientRecv(Option<tungstenite::Result<Message>>),
            SimulationUpdate(SimulationUpdate),
        }

        let selected = tokio::select! {
            // Periodically request a signal-value flush from the simulator
            _ = update_interval.tick(),
                if client_session.is_some() && simulation_status == Some(SimulationStatus::Running) =>
            {
                SelectBranch::UpdateTick
            },

            // Accept new connections (only when no client is connected)
            accept_result = listener.accept(), if client_session.is_none() => {
                SelectBranch::AcceptConnection(accept_result)
            },

            // Handle messages from the connected client
            ws_item = async {
                match &mut client_session {
                    Some(session) => session.stream.next().await,
                    None => pending().await,
                }
            } => SelectBranch::ClientRecv(ws_item),

            // Handle simulation updates from the simulator thread
            Some(update) = update_rx.recv() => SelectBranch::SimulationUpdate(update),
        };

        match selected {
            SelectBranch::UpdateTick => {
                let _ = command_tx.send(SimulationCommand::SendUpdate);
            },

            SelectBranch::AcceptConnection(Ok((stream, _))) => {
                let stream = match tokio_tungstenite::accept_async(stream).await {
                    Ok(ws) => ws,
                    Err(error) => {
                        error!(%error, "error during WebSocket handshake");
                        continue;
                    },
                };

                let connection_span = info_span!("client");
                let mut session = ClientSession { stream };

                {
                    let _enter = connection_span.enter();
                    debug!("WebSocket connection established");

                    // Send design hierarchy to the newly connected client
                    if let Some(hierarchy) = &design_hierarchy {
                        let update = WsSimulationUpdate::DesignHierarchy(hierarchy.clone());
                        if let Err(error) = session.send(&update).await {
                            error!(%error, "failed to send design hierarchy");
                            continue;
                        }
                    }
                }

                client_session = Some(session);
            },
            SelectBranch::AcceptConnection(Err(error)) => {
                error!(%error, "failed to accept connection");
            },

            SelectBranch::ClientRecv(Some(Ok(message))) => {
                let client_recv_span = info_span!("client_recv");
                let _enter_span = client_recv_span.enter();

                let text = match message {
                    Message::Text(text) => text,
                    Message::Close(_) => {
                        debug!("connection closed by client");
                        client_session = None;
                        continue;
                    },
                    _ => continue,
                };

                let command: Command = match serde_json::from_str(&text) {
                    Ok(cmd) => cmd,
                    Err(error) => {
                        error!(%error, %text, "failed to parse message");
                        continue;
                    },
                };
                debug!(?command, "received command from client");

                let tx = match command {
                    Command::StartSimulation => SimulationCommand::Start,
                    Command::StopSimulation => SimulationCommand::Stop,
                    Command::PauseSimulation => SimulationCommand::Pause,
                    Command::ResumeSimulation => SimulationCommand::Resume,
                    Command::Subscribe(signals) => SimulationCommand::Subscribe(signals),
                    Command::Unsubscribe(signals) => SimulationCommand::Unsubscribe(signals),
                };
                let _ = command_tx.send(tx);
            },
            SelectBranch::ClientRecv(Some(Err(error))) => {
                error!(%error, "WebSocket error");
                client_session = None;
            },
            SelectBranch::ClientRecv(None) => {
                debug!("WebSocket stream ended");
                client_session = None;
            },

            SelectBranch::SimulationUpdate(SimulationUpdate::Design(hierarchy)) => {
                debug_assert!(design_hierarchy.is_none());
                let message = WsSimulationUpdate::DesignHierarchy(hierarchy);
                send_to_client(&mut client_session, &message).await;
                // Deconstruct the message to store the design hierarchy without cloning it
                if let WsSimulationUpdate::DesignHierarchy(hierarchy) = message {
                    design_hierarchy = Some(hierarchy);
                }
            },
            SelectBranch::SimulationUpdate(SimulationUpdate::Events(values)) => {
                send_to_client(&mut client_session, &WsSimulationUpdate::Events(values)).await;
            },
            SelectBranch::SimulationUpdate(SimulationUpdate::StatusChanged(status, ack_tx)) => {
                debug!(?status, "simulation status changed");
                let update = match status {
                    SimulationStatus::Paused => WsSimulationUpdate::SimulationPaused,
                    SimulationStatus::Running => {
                        if simulation_status.is_none() {
                            WsSimulationUpdate::SimulationStarted
                        } else {
                            WsSimulationUpdate::SimulationResumed
                        }
                    },
                    SimulationStatus::Stopped => WsSimulationUpdate::SimulationStopped,
                };
                simulation_status = Some(status);
                send_to_client(&mut client_session, &update).await;
                if let Some(tx) = ack_tx {
                    let _ = tx.send(());
                }
            },
        }
    }
}
