use std::sync::mpsc::SyncSender;

use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalElementId;
use hdl_simulation_protocol::from_simulator::EventsUpdate;
use smallvec::SmallVec;

mod design;
mod json_buffer;
mod logging;
mod sim_interface;
mod websocket_server;

/// Commands that can be sent from the WebSocket thread to the main simulation thread
#[derive(Debug)]
enum SimulationCommand {
    Start,
    Stop,

    Subscribe(SmallVec<[SignalElementId; 1]>),
    #[expect(unused, reason = "TODO WIP")]
    Unsubscribe(SmallVec<[SignalElementId; 1]>),
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
