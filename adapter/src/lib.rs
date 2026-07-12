use std::sync::LazyLock;

use crossbeam_channel::Sender as SyncSender;
use hdl_simulation_protocol::SimulationId;
use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalElementId;
use hdl_simulation_protocol::from_simulator::EventsUpdate;
use hdl_simulation_protocol::to_simulator::RunUntil;

mod ada_ffi;
mod design;
mod design_export;
mod json_buffer;
mod logging;
mod sim_interface;
mod websocket_server;

/// The randomly generated ID for this simulation instance.
static SIMULATION_ID: LazyLock<SimulationId> = LazyLock::new(SimulationId::new_random);

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
