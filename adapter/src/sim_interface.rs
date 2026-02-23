use std::mem::replace;
use std::num::NonZeroU32;
use std::sync::OnceLock;

use crossbeam_channel::Receiver;
use hdl_simulation_protocol::SignalInstanceId;
use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::from_simulator::Event;
use hdl_simulation_protocol::from_simulator::EventsUpdate;
use hdl_simulation_protocol::from_simulator::RawValue;
use hdl_simulation_protocol::from_simulator::SignalEvents;
use hdl_simulation_protocol::time::Delta;
use hdl_simulation_protocol::time::LogicalTime;
use hdl_simulation_protocol::time::PhysicalTime;
use rustc_hash::FxHashMap;
use tracing::debug;
use tracing::error;
use tracing::info;
use tracing::instrument;
use tracing::trace;
use tracing::warn;

use crate::SimulationCommand;
use crate::SimulationUpdate;
use crate::design::Signal;
use crate::websocket_server::run_websocket_server;

unsafe extern "C" {
    /// Sets the Subscription field of a signal in GHDL's signal table.
    safe fn ghdl_set_signal_subscription(signal_id: NonZeroU32, sub_idx: u32);
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

    time_for_events: LogicalTime,
    events: EventsUpdate,

    current_status: SimulationStatus,
    requested_status: SimulationStatus,
}

impl AdapterState {
    /// Flushes accumulated signal events up to `end_time` to the WebSocket thread.
    fn transmit_events(&mut self) {
        let end_time = self.events.time_range.end;
        if !self.events.time_range.is_empty() {
            let signals = self
                .events
                .signals
                .iter()
                .map(SignalEvents::clone_empty)
                .collect();
            let signal_values = replace(
                &mut self.events,
                EventsUpdate {
                    time_range: end_time..end_time,
                    signals,
                },
            );
            self.update_tx
                .send(SimulationUpdate::Events(signal_values))
                .expect("Failed to send simulation update"); // TODO handle error
        }
    }

    /// Subscribes to the given signals, flushing any pending events first.
    fn subscribe(&mut self, signal_ids: &[SignalInstanceId]) {
        use std::collections::hash_map::Entry;

        self.transmit_events();

        let mut next_index = self.subscriptions.len();
        for &signal_id in signal_ids {
            debug!(%signal_id, "subscribing to signal");
            if let Entry::Vacant(entry) = self.signal_indices.entry(signal_id) {
                entry.insert(next_index);
                self.subscriptions.push(signal_id);
                ghdl_set_signal_subscription(signal_id.0, next_index as u32);
                self.events.signals.push(SignalEvents::new(signal_id));

                next_index += 1;
            }
        }
    }

    /// Sets the design hierarchy to be sent to WebSocket clients.
    ///
    /// Sends the hierarchy to all currently connected clients and stores it
    /// for new connections.
    pub fn set_design_hierarchy(&mut self, hierarchy: DesignHierarchy, signals: Vec<Signal>) {
        self.signals = signals;

        if let Err(e) = self.update_tx.send(SimulationUpdate::Design(hierarchy)) {
            error!("failed to broadcast design hierarchy: {e}");
        }
    }
}

static RUNTIME: OnceLock<tokio::runtime::Runtime> = OnceLock::new();

/// Initializes the adapter.
///
/// Must be called once before simulation starts. Sets up the OpenTelemetry
/// tracing subscriber and spawns the WebSocket server on a shared tokio
/// runtime.
/// Returns a pointer to the adapter state that must be passed to other
/// `adapter_*` functions.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_init_websocket() -> *mut AdapterState {
    let rt = RUNTIME.get_or_init(|| {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .expect("failed to create tokio runtime")
    });
    let _rt_guard = rt.enter();

    crate::logging::init_logging();

    let (command_tx, command_rx) = crossbeam_channel::unbounded::<SimulationCommand>();
    let (update_tx, update_rx) = tokio::sync::mpsc::unbounded_channel::<SimulationUpdate>();

    rt.spawn(run_websocket_server(command_tx, update_rx));

    info!("WebSocket server thread started");

    Box::into_raw(Box::new(AdapterState {
        command_rx,
        update_tx,
        subscriptions: Vec::new(),
        signals: Vec::new(),
        signal_indices: FxHashMap::default(),
        time_for_events: LogicalTime::ZERO,
        events: EventsUpdate {
            time_range: LogicalTime::ZERO..LogicalTime::ZERO,
            signals: Vec::new(),
        },
        current_status: SimulationStatus::Paused,
        requested_status: SimulationStatus::Paused,
    }))
}

/// Processes commands from the WebSocket thread.
///
/// When `block` is non-zero, blocks until at least one command is received.
/// When `block` is zero, returns immediately if no commands are pending.
#[instrument(skip(state), level = "debug")]
#[unsafe(no_mangle)]
pub extern "C" fn adapter_process_commands(state: &mut AdapterState, block: bool) {
    if block {
        match state.command_rx.recv() {
            Ok(cmd) => process_command(state, cmd),
            Err(e) => {
                error!("channel error in process_commands: {e}");
                return;
            },
        }
    };

    while let Ok(command) = state.command_rx.try_recv() {
        process_command(state, command);
    }
}

/// Processes a single simulation command and returns the updated request code.
fn process_command(state: &mut AdapterState, command: SimulationCommand) {
    match command {
        SimulationCommand::Start => {
            info!("received Start command");
            state.requested_status = SimulationStatus::Running;
        },
        SimulationCommand::Stop => {
            info!("received Stop command");
            state.requested_status = SimulationStatus::Stopped;
        },
        SimulationCommand::Subscribe(signal_ids) => {
            state.subscribe(&signal_ids);
        },
        SimulationCommand::Unsubscribe(_signal_ids) => {
            state.transmit_events();
            // TODO remove subscription from GHDL data structures
        },
        SimulationCommand::SendUpdate => {
            state.transmit_events();
        },
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_requested_simulation_status(state: &AdapterState) -> SimulationStatus {
    state.requested_status
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_set_next_event_time(
    state: &mut AdapterState,
    physical_time: i64,
    delta_cycle: i64,
) {
    debug_assert!(physical_time >= 0);
    debug_assert!(delta_cycle >= 0);
    state.time_for_events = LogicalTime {
        physical: PhysicalTime(physical_time as u64),
        delta: Delta(delta_cycle as u64),
    };
    trace!(%state.time_for_events, "set next event time");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_update_simulation_time(state: &mut AdapterState) {
    state.events.time_range.end = state.time_for_events;
    trace!(%state.events.time_range.end, "updated simulation time");
}

/// Sends a status update to all connected clients if the status has changed.
///
/// When the status is [`SimulationStatus::Stopped`], blocks until the
/// notification has been flushed on all WebSocket connections (with a
/// two-second timeout).
#[instrument(skip(state))]
#[unsafe(no_mangle)]
pub extern "C" fn adapter_notify_simulation_status(
    state: &mut AdapterState,
    status: SimulationStatus,
) {
    if state.current_status == status {
        return;
    }
    info!(
        previous = ?state.current_status,
        new = ?status,
        "simulation status changed",
    );
    state.current_status = status;

    let (ack_tx, ack_rx) = if status == SimulationStatus::Stopped {
        state.transmit_events();
        let (tx, rx) = std::sync::mpsc::sync_channel(0);
        (Some(tx), Some(rx))
    } else {
        (None, None)
    };
    if let Err(e) = state
        .update_tx
        .send(SimulationUpdate::StatusChanged(status, ack_tx))
    {
        error!("failed to send simulation status update: {e}");
        return;
    }

    if let Some(rx) = ack_rx {
        match rx.recv_timeout(std::time::Duration::from_secs(2)) {
            Ok(()) => {
                debug!("stopped notification acknowledged by WebSocket thread");
            },
            Err(std::sync::mpsc::RecvTimeoutError::Timeout) => {
                warn!("timed out waiting for stopped notification acknowledgment");
            },
            Err(std::sync::mpsc::RecvTimeoutError::Disconnected) => {
                warn!("WebSocket thread dropped the acknowledgment channel");
            },
        }
    }
}

/// Records a signal value change at the given simulation time.
#[instrument(level = "trace", skip(state))]
#[unsafe(no_mangle)]
pub extern "C" fn adapter_notify_signal_event(
    state: &mut AdapterState,
    subscription_index: u32,
    value: u64,
) {
    let subscription_index = subscription_index as usize;
    if subscription_index >= state.subscriptions.len() {
        // TODO this would be a bug
        panic!("Subscription index out of bounds: {subscription_index}");
    }
    state.events.signals[subscription_index].events.push(Event {
        time: state.time_for_events,
        value: RawValue(value),
    });
}
