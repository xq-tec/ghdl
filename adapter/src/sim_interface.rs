use std::mem::replace;
use std::num::NonZeroU32;
use std::thread;

use crossbeam_channel::Receiver;
use hdl_simulation_protocol::Logic;
use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::from_simulator::{NewValuesEnum, SignalValuesInRange};
use hdl_simulation_protocol::time::{Delta, LogicalTime, PhysicalTime};
use rustc_hash::FxHashMap;

use crate::SimulationCommand;
use crate::SimulationUpdate;
use crate::design::Signal;
use crate::websocket_server::run_websocket_server;

unsafe extern "C" {
    /// Sets the Subscription field of a signal in GHDL's signal table.
    safe fn ghdl_set_signal_subscription(signal_id: NonZeroU32, sub_idx: u32);
}

/// The design hierarchy sent to clients, along with the extracted signal information.
#[derive(Clone)]
pub struct DesignHierarchyWithSignals {
    pub(crate) hierarchy: DesignHierarchy,
}

/// Constructs a [`LogicalTime`] from the FFI integer representation.
fn logical_time_from_ffi(physical_time: i64, delta_cycle: i64) -> LogicalTime {
    debug_assert!(physical_time >= 0);
    debug_assert!(delta_cycle >= 0);
    LogicalTime {
        physical: PhysicalTime(physical_time as u64),
        delta: Delta(delta_cycle as u64),
    }
}

/// Simulator-facing adapter state.
///
/// Created during initialization and passed to all `adapter_*` FFI functions
/// that are called from the GHDL simulation thread.
pub struct AdapterState {
    /// Receiver for simulation commands (from the WebSocket thread).
    command_rx: Receiver<SimulationCommand>,
    /// Sender for simulation updates (to the WebSocket thread).
    ///
    /// `UnboundedSender::send()` is synchronous, so it can be called from the
    /// non-async FFI context.
    update_tx: tokio::sync::mpsc::UnboundedSender<SimulationUpdate>,

    signals: Vec<Signal>,
    subscriptions: Vec<SignalInstanceId>,
    /// Maps the IDs of subscribed signals to their index in the
    /// [`subscriptions`](Self::subscriptions) list.
    signal_indices: FxHashMap<SignalInstanceId, usize>,

    events: SignalValuesInRange,
}

impl AdapterState {
    /// Flushes accumulated signal events up to `end_time` to the WebSocket thread.
    fn transmit_events(&mut self, end_time: LogicalTime) {
        debug_assert!(self.events.time_range.end <= end_time);
        self.events.time_range.end = end_time;
        if !self.events.time_range.is_empty() {
            let values_in_range = self
                .events
                .values_in_range
                .iter()
                .map(NewValuesEnum::clone_empty)
                .collect();
            let signal_values = replace(
                &mut self.events,
                SignalValuesInRange {
                    time_range: end_time..end_time,
                    values_in_range,
                },
            );
            self.update_tx
                .send(SimulationUpdate::SignalValuesInRange(signal_values))
                .expect("Failed to send simulation update"); // TODO handle error
        }
    }

    /// Subscribes to the given signals, flushing any pending events first.
    fn subscribe(&mut self, current_time: LogicalTime, signal_ids: &[SignalInstanceId]) {
        use std::collections::hash_map::Entry;

        self.transmit_events(current_time);

        let mut next_index = self.subscriptions.len();
        for &signal_id in signal_ids {
            eprintln!("Subscribing to signal {signal_id}");
            if let Entry::Vacant(entry) = self.signal_indices.entry(signal_id) {
                entry.insert(next_index);
                self.subscriptions.push(signal_id);
                ghdl_set_signal_subscription(signal_id.0, next_index as u32);
                let type_kind = &self.signals[signal_id.0.get() as usize].type_kind;
                let value_type = type_kind.to_value_type();
                self.events
                    .values_in_range
                    .push(NewValuesEnum::new(signal_id, value_type));

                next_index += 1;
            }
        }
    }

    /// Sets the design hierarchy to be sent to WebSocket clients.
    ///
    /// Sends the hierarchy to all currently connected clients and stores it
    /// for new connections.
    pub fn set_design_hierarchy(&mut self, hierarchy: DesignHierarchy, signals: Vec<Signal>) {
        eprintln!("Setting design hierarchy: {hierarchy:#?}");
        self.signals = signals;
        let data = DesignHierarchyWithSignals { hierarchy };

        if let Err(e) = self.update_tx.send(SimulationUpdate::Design(data)) {
            eprintln!("Failed to broadcast design hierarchy: {e}");
        }
    }
}

/// Initializes the adapter.
///
/// Must be called once before simulation starts. Spawns a background thread
/// running the WebSocket server with a single-threaded tokio runtime.
/// Returns a pointer to the adapter state that must be passed to other
/// `adapter_*` functions.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_init_websocket() -> *mut AdapterState {
    let (command_tx, command_rx) = crossbeam_channel::unbounded::<SimulationCommand>();
    let (update_tx, update_rx) = tokio::sync::mpsc::unbounded_channel::<SimulationUpdate>();

    thread::spawn(move || {
        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime");

        rt.block_on(run_websocket_server(command_tx, update_rx));
    });

    eprintln!("WebSocket server thread started");

    Box::into_raw(Box::new(AdapterState {
        command_rx,
        update_tx,
        subscriptions: Vec::new(),
        signals: Vec::new(),
        signal_indices: FxHashMap::default(),
        events: SignalValuesInRange {
            time_range: LogicalTime::ZERO..LogicalTime::ZERO,
            values_in_range: Vec::new(),
        },
    }))
}

/// Blocks until a StartSimulation command is received from a WebSocket client.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_wait_for_start_simulation(state: &mut AdapterState) {
    eprintln!("Waiting for start simulation command...");

    loop {
        match state.command_rx.recv() {
            Ok(SimulationCommand::Start) => {
                eprintln!("Received start simulation command");
                return;
            }
            Ok(SimulationCommand::Stop) => {
                eprintln!("Ignoring stop command (waiting for start)");
            }
            Ok(SimulationCommand::Subscribe(signal_ids)) => {
                state.subscribe(LogicalTime::ZERO, &signal_ids);
            }
            Ok(_) => todo!(),
            Err(e) => {
                eprintln!("Channel error while waiting for start: {e}");
                return;
            }
        }
    }
}

/// Blocks until a StopSimulation command is received from a WebSocket client.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_wait_for_stop_simulation(state: &AdapterState) {
    eprintln!("Waiting for stop simulation command...");

    loop {
        match state.command_rx.recv() {
            Ok(SimulationCommand::Stop) => {
                eprintln!("Received stop simulation command");
                return;
            }
            Ok(SimulationCommand::Start) => {
                eprintln!("Ignoring start command (waiting for stop)");
            }
            Ok(_) => todo!(),
            Err(e) => {
                eprintln!("Channel error while waiting for stop: {e}");
                return;
            }
        }
    }
}

/// Drains all pending commands from the WebSocket thread and processes them.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_handle_commands(
    state: &mut AdapterState,
    physical_time: i64,
    delta_cycle: i64,
) {
    let current_time = logical_time_from_ffi(physical_time, delta_cycle);
    while let Ok(command) = state.command_rx.try_recv() {
        match command {
            SimulationCommand::Subscribe(signal_ids) => {
                state.subscribe(current_time, &signal_ids);
            }
            SimulationCommand::Unsubscribe(_signal_ids) => {
                // TODO handle unsubscribe by one client, while others remain subscribed
                state.transmit_events(current_time);
                // TODO remove subscription from GHDL code
            }
            SimulationCommand::SendUpdate => {
                state.transmit_events(current_time);
            }
            SimulationCommand::Start | SimulationCommand::Stop => {
                // TODO
            }
        }
    }
}

/// Records a signal value change at the given simulation time.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_notify_signal_event(
    state: &mut AdapterState,
    physical_time: i64,
    delta_cycle: i64,
    subscription_index: u32,
    value: u64,
) {
    let subscription_index = subscription_index as usize;
    if subscription_index >= state.subscriptions.len() {
        // TODO this would be a bug
        panic!("Subscription index out of bounds: {subscription_index}");
    }
    let current_time = logical_time_from_ffi(physical_time, delta_cycle);
    match &mut state.events.values_in_range[subscription_index] {
        NewValuesEnum::F64(v) => {
            v.timestamps.push(current_time);
            eprintln!(
                "Notifying signal event: subscription index = {subscription_index}, value = {}",
                f64::from_ne_bytes(value.to_ne_bytes())
            );
            v.values.push(f64::from_ne_bytes(value.to_ne_bytes()));
        }
        NewValuesEnum::U8(v) => {
            v.timestamps.push(current_time);
            eprintln!(
                "Notifying signal event: subscription index = {subscription_index}, value = {value}",
            );
            v.values.push(value as u8);
        }
        NewValuesEnum::Logic(v) => {
            v.timestamps.push(current_time);
            eprintln!(
                "Notifying signal event: subscription index = {subscription_index}, value = {}",
                Logic::try_from(value as u8).unwrap()
            );
            v.values.push(Logic::try_from(value as u8).unwrap());
        }
    }
}
