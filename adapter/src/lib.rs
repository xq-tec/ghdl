use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::from_simulator::SignalValuesInRange;
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
    Unsubscribe(SmallVec<[SignalInstanceId; 1]>),
    SendUpdate,
}

enum SimulationUpdate {
    SignalValuesInRange(SignalValuesInRange),
    Design(DesignHierarchyWithSignals),
}
