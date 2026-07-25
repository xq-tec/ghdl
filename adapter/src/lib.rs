use std::sync::OnceLock;

use crossbeam_channel::Sender as SyncSender;
use hdl_simulation_protocol::SimulationId;
use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalElementId;
use hdl_simulation_protocol::from_simulator::EventsUpdate;
use hdl_simulation_protocol::to_simulator::RunUntil;

mod design;
mod json_buffer;
mod logging;
mod sim_interface;
mod websocket_server;

/// The ID for this simulation instance.
///
/// Set from `--simulation-id` during adapter init when provided; otherwise randomly generated
/// on first access.
static SIMULATION_ID: OnceLock<SimulationId> = OnceLock::new();

fn simulation_id() -> SimulationId {
    *SIMULATION_ID.get_or_init(SimulationId::new_random)
}

/// Commands that can be sent from the WebSocket thread to the main simulation thread
#[derive(Debug)]
enum SimulationCommand {
    Run { until: RunUntil },
    Pause,
    Stop,

    Subscribe(Vec<SignalElementId>),
    Unsubscribe(Vec<SignalElementId>),
    SendUpdate,
}

enum SimulationUpdate {
    Events(EventsUpdate),
    Design(DesignHierarchy),
    /// Notifies all connected clients of a simulation status change.
    ///
    /// If the optional sender is present, the WebSocket thread sends an
    /// acknowledgment after the notification has been flushed to all connections.
    StatusChanged(SimulationStatus, Option<SyncSender<()>>),
}
