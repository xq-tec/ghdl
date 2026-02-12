use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::from_simulator::EventsUpdate;
use smallvec::SmallVec;

use crate::sim_interface::DesignHierarchyWithSignals;

mod design;
mod json_buffer;
mod sim_interface;
mod websocket_server;

/// Commands that can be sent from the WebSocket thread to the main simulation thread
#[derive(Debug)]
enum SimulationCommand {
    Start,
    Stop,

    Subscribe(SmallVec<[SignalInstanceId; 1]>),
    #[expect(unused, reason = "TODO WIP")]
    Unsubscribe(SmallVec<[SignalInstanceId; 1]>),
    SendUpdate,
}

enum SimulationUpdate {
    Events(EventsUpdate),
    Design(DesignHierarchyWithSignals),
    /// Notifies all connected clients of a simulation status change.
    ///
    /// If the optional sender is present, the WebSocket thread sends an
    /// acknowledgment after the notification has been flushed to all connections.
    StatusChanged(SimulationStatus, Option<std::sync::mpsc::SyncSender<()>>),
}
