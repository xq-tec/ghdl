use std::mem::replace;
use std::num::NonZeroU32;
use std::sync::OnceLock;
use std::time::Duration;

use crossbeam_channel::Receiver as SyncReceiver;
use crossbeam_channel::bounded as sync_bounded;
use crossbeam_channel::unbounded as sync_unbounded;
use hdl_simulation_protocol::SimulationStatus;
use hdl_simulation_protocol::design_hierarchy::DesignHierarchy;
use hdl_simulation_protocol::design_hierarchy::SignalElementId;
use hdl_simulation_protocol::from_simulator::Event;
use hdl_simulation_protocol::from_simulator::EventsUpdate;
use hdl_simulation_protocol::from_simulator::RawValue;
use hdl_simulation_protocol::from_simulator::SignalEvents;
use hdl_simulation_protocol::time::Delta;
use hdl_simulation_protocol::time::LogicalTime;
use hdl_simulation_protocol::time::PhysicalTime;
use rustc_hash::FxHashMap;
use tokio::sync::mpsc::Sender as AsyncSender;
use tokio::sync::mpsc::channel as async_bounded;
use tracing::debug;
use tracing::error;
use tracing::info;
use tracing::instrument;
use tracing::trace;
use tracing::warn;

use crate::SimulationCommand;
use crate::SimulationUpdate;
use crate::design::Signal;
use crate::websocket_server::EVENTS_PER_UPDATE_THRESHOLD;
use crate::websocket_server::run_websocket_server;

unsafe extern "C" {
    /// Sets the `Subscription` field of a signal in GHDL's signal table.
    safe fn ghdl_set_signal_subscription(
        signal_id: NonZeroU32,
        element_index: u32,
        subscription_index: SubscriptionIndex,
    ) -> u64;
}

#[derive(Clone, Copy, Debug)]
#[repr(C)]
struct SubscriptionIndex(u32);

/// Simulator-facing adapter state.
///
/// Created during initialization and passed to all `adapter_*` FFI functions
/// that are called from the GHDL simulation thread.
pub struct AdapterState {
    /// Receiver for simulation commands (from the WebSocket thread).
    command_rx: SyncReceiver<SimulationCommand>,
    /// Sender for simulation updates (to the WebSocket thread).
    ///
    /// This is a bounded channel to put backpressure on the simulation thread when the
    /// WebSocket thread is getting behind in transmitting updates.
    update_tx: AsyncSender<SimulationUpdate>,

    signals: Vec<Signal>,
    subscriptions: SubscriptionTracker,
    time_for_events: LogicalTime,

    current_status: SimulationStatus,
    requested_status: SimulationStatus,
}

impl AdapterState {
    /// Flushes accumulated signal events to the WebSocket thread.
    fn transmit_events(&mut self) {
        // Transmit an update if there are new events or if the time range has changed.
        if let Some(events_update) = self.subscriptions.extract_events() {
            let _ignore = self
                .update_tx
                .blocking_send(SimulationUpdate::Events(events_update));
        }
    }

    /// Subscribes to the given signals, flushing any pending events first.
    fn subscribe(&mut self, element_ids: &[SignalElementId]) {
        self.transmit_events();
        self.subscriptions.subscribe(element_ids);
    }

    /// Sets the design hierarchy to be sent to WebSocket clients.
    ///
    /// Sends the hierarchy to all currently connected clients and stores it
    /// for new connections.
    ///
    /// # Panics
    ///
    /// Panics if the design hierarchy has already been set.
    pub fn set_design_hierarchy(&mut self, hierarchy: DesignHierarchy, signals: Vec<Signal>) {
        assert!(self.signals.is_empty(), "design hierarchy already set");

        self.signals = signals;
        if let Err(e) = self
            .update_tx
            .blocking_send(SimulationUpdate::Design(hierarchy))
        {
            error!("failed to broadcast design hierarchy: {e}");
        }
    }
}

struct SubscriptionTracker {
    /// The list of currently subscribed signal elements.
    subscriptions: Vec<SignalElementId>,
    /// Maps the IDs of subscribed signal elements to their index in the
    /// [`subscriptions`](Self::subscriptions) list.
    element_indices: FxHashMap<SignalElementId, SubscriptionIndex>,

    events: EventsUpdate,
    event_count: usize,
}

impl SubscriptionTracker {
    fn new() -> Self {
        Self {
            subscriptions: Vec::new(),
            element_indices: FxHashMap::default(),
            events: EventsUpdate {
                time_range: LogicalTime::ZERO..LogicalTime::ZERO,
                signals: Vec::new(),
            },
            event_count: 0,
        }
    }

    /// Subscribes to the given signal elements.
    fn subscribe(&mut self, element_ids: &[SignalElementId]) {
        use std::collections::hash_map::Entry;

        let mut next_index = self.subscriptions.len();
        for &element_id in element_ids {
            debug!(?element_id, "subscribing to signal");
            if let Entry::Vacant(entry) = self.element_indices.entry(element_id) {
                let subscription_index = SubscriptionIndex(next_index as u32);
                entry.insert(subscription_index);
                self.subscriptions.push(element_id);
                // TODO ensure that signal_id and element_index are in bounds
                let initial_value = ghdl_set_signal_subscription(
                    element_id.signal_id.0,
                    element_id.element_index,
                    subscription_index,
                );
                let mut signal_events = SignalEvents::new(element_id);
                signal_events.events.push(Event {
                    time: self.events.time_range.end,
                    value: RawValue(initial_value),
                });
                self.events.signals.push(signal_events);
                self.event_count += 1;

                next_index += 1;
            }
        }
    }

    fn update_time_range(&mut self, time: LogicalTime) {
        self.events.time_range.end = time;
    }

    fn notify_signal_event(
        &mut self,
        subscription_index: SubscriptionIndex,
        time: LogicalTime,
        value: u64,
    ) {
        let index = subscription_index.0 as usize;
        // get_mut(index) should never fail, this would be a bug. But just in case we do have a bug,
        // we ignore this error so that the simulation can continue.
        if let Some(signal) = self.events.signals.get_mut(index) {
            signal.events.push(Event {
                time,
                value: RawValue(value),
            });
            self.event_count += 1;
        }
    }

    /// Extracts and returns any accumulated events.
    ///
    /// Returns `None` if there are no events **and** the time range is empty.
    fn extract_events(&mut self) -> Option<EventsUpdate> {
        if self.events.time_range.is_empty() {
            return None;
        }

        let end_time = self.events.time_range.end;
        let signals = self
            .events
            .signals
            .iter()
            .map(SignalEvents::clone_empty)
            .collect();
        let events = replace(
            &mut self.events,
            EventsUpdate {
                time_range: end_time..end_time,
                signals,
            },
        );
        self.event_count = 0;
        Some(events)
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
extern "C" fn adapter_init_websocket(wait_for_gui: bool) -> *mut AdapterState {
    let rt = RUNTIME.get_or_init(|| {
        tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .worker_threads(2)
            .build()
            .expect("failed to create tokio runtime")
    });
    let _rt_guard = rt.enter();

    // The looger uses the tokio runtime
    crate::logging::init_logging();

    let (command_tx, command_rx) = sync_unbounded::<SimulationCommand>();
    // At a 10 Hz update rate, this buffer size allows for ~3 seconds of buffering.
    let (update_tx, update_rx) = async_bounded::<SimulationUpdate>(30);

    rt.spawn(run_websocket_server(command_tx, update_rx));

    Box::into_raw(Box::new(AdapterState {
        command_rx,
        update_tx,
        signals: Vec::new(),
        subscriptions: SubscriptionTracker::new(),
        time_for_events: LogicalTime::ZERO,
        current_status: SimulationStatus::Paused,
        requested_status: if wait_for_gui {
            SimulationStatus::Paused
        } else {
            SimulationStatus::Running
        },
    }))
}

/// Processes commands from the WebSocket thread.
///
/// When `block` is non-zero, blocks until at least one command is received.
/// When `block` is zero, returns immediately if no commands are pending.
#[instrument(skip(state), level = "debug")]
#[unsafe(no_mangle)]
extern "C" fn adapter_process_commands(state: &mut AdapterState, block: bool) {
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
extern "C" fn adapter_requested_simulation_status(state: &AdapterState) -> SimulationStatus {
    state.requested_status
}

#[unsafe(no_mangle)]
extern "C" fn adapter_set_next_event_time(
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

/// Notifies the adapter that the current simulation cycle (one iteration of the simulation loop)
/// has finished.
#[unsafe(no_mangle)]
extern "C" fn adapter_update_simulation_time(state: &mut AdapterState) {
    state.subscriptions.update_time_range(state.time_for_events);
    if state.subscriptions.event_count >= EVENTS_PER_UPDATE_THRESHOLD {
        state.transmit_events();
    }
    trace!(%state.time_for_events, "updated simulation time");
}

/// Sends a status update to the WebSocket client if the status has changed.
///
/// When the status is [`SimulationStatus::Stopped`], blocks until the
/// notification has been flushed to the client or skipped (no client), with a
/// two-second timeout.
#[instrument(skip(state), level = "debug")]
#[unsafe(no_mangle)]
extern "C" fn adapter_notify_simulation_status(state: &mut AdapterState, status: SimulationStatus) {
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
        // Tell the WebSocket thread to acknowledge the transmission of the stop notification.
        let (tx, rx) = sync_bounded(0);
        (Some(tx), Some(rx))
    } else {
        (None, None)
    };
    let _ignore = state
        .update_tx
        .send(SimulationUpdate::StatusChanged(status, ack_tx));

    if let Some(rx) = ack_rx {
        // If the simulation has stopped, the simulator process will exit. We until the WebSocket
        // thread acknowledges that it sent the stop notification, otherwise it would get lost.
        match rx.recv_timeout(Duration::from_secs(2)) {
            Ok(()) => {
                debug!("stopped notification acknowledged by WebSocket thread");
            },
            Err(_) => {
                warn!("timed out waiting for stopped notification acknowledgment");
            },
        }
    }
}

/// Records a signal value change at the given simulation time.
#[instrument(level = "trace", skip(state))]
#[unsafe(no_mangle)]
extern "C" fn adapter_notify_signal_event(
    state: &mut AdapterState,
    subscription_index: SubscriptionIndex,
    value: u64,
) {
    state
        .subscriptions
        .notify_signal_event(subscription_index, state.time_for_events, value);
}
