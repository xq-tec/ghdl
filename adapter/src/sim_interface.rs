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
use hdl_simulation_protocol::from_simulator::Report;
use hdl_simulation_protocol::from_simulator::Severity;
use hdl_simulation_protocol::from_simulator::SignalEvents;
use hdl_simulation_protocol::time::Delta;
use hdl_simulation_protocol::time::LogicalTime;
use hdl_simulation_protocol::time::PhysicalTime;
use hdl_simulation_protocol::to_simulator::RunUntil;
use rustc_hash::FxHashMap;
use tokio::sync::mpsc::Sender as AsyncSender;
use tokio::sync::mpsc::channel as async_bounded;
use tracing::debug;
use tracing::error;
use tracing::info;
use tracing::instrument;
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

impl SubscriptionIndex {
    const INVALID: Self = Self(u32::MAX);
}

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
    requested_end_time: Option<PhysicalTime>,
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

    /// Unsubscribes from the given signals, flushing any pending events first.
    fn unsubscribe(&mut self, element_ids: &[SignalElementId]) {
        self.transmit_events();
        self.subscriptions.unsubscribe(element_ids);
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

    /// Sets the physical time limit for the current run.
    fn set_requested_end_time(&mut self, until: RunUntil) {
        self.requested_end_time = match until {
            RunUntil::UntilEnd => None,
            RunUntil::UntilTime { deadline } => Some(deadline),
            RunUntil::ForTime { duration } => Some(self.time_for_events.physical + duration),
        };
    }

    /// Pauses the simulation and notifies connected clients.
    fn pause_simulation(&mut self) {
        self.requested_end_time = None;
        self.transmit_events();
        self.set_simulation_status(SimulationStatus::Paused);
    }

    /// Processes a single simulation command.
    fn process_command(&mut self, command: SimulationCommand) {
        match command {
            SimulationCommand::Run { until } => {
                debug!(?until, "received Run command");
                self.set_requested_end_time(until);
                self.set_simulation_status(SimulationStatus::Running);
            },
            SimulationCommand::Pause => {
                debug!("received Pause command");
                self.requested_end_time = None;
                self.set_simulation_status(SimulationStatus::Paused);
            },
            SimulationCommand::Stop => {
                debug!("received Stop command");
                self.set_simulation_status(SimulationStatus::Stopped);
            },
            SimulationCommand::Subscribe(signal_ids) => {
                self.subscribe(&signal_ids);
            },
            SimulationCommand::Unsubscribe(signal_ids) => {
                self.unsubscribe(&signal_ids);
            },
            SimulationCommand::SendUpdate => {
                self.transmit_events();
            },
        }
    }

    fn set_simulation_status(&mut self, status: SimulationStatus) {
        if self.current_status == status {
            return;
        }
        self.current_status = status;
        match status {
            SimulationStatus::Paused => info!("simulation paused"),
            SimulationStatus::Running => info!("simulation running"),
            SimulationStatus::Stopped => info!("simulation stopped"),
        };

        let (ack_tx, ack_rx) = if status == SimulationStatus::Stopped {
            self.transmit_events();
            // Tell the WebSocket thread to acknowledge the transmission of the stop notification.
            let (tx, rx) = sync_bounded(0);
            (Some(tx), Some(rx))
        } else {
            (None, None)
        };
        let _ignore = self
            .update_tx
            .blocking_send(SimulationUpdate::StatusChanged(status, ack_tx));

        if let Some(rx) = ack_rx {
            // If the simulation has stopped, the simulator process will exit. We until the WebSocket
            // thread acknowledges that it sent the stop notification, otherwise it would get lost.
            match rx.recv_timeout(Duration::from_secs(2)) {
                Ok(()) => {
                    debug!("notification acknowledged by WebSocket thread");
                },
                Err(_) => {
                    warn!("timed out waiting for notification acknowledgment");
                },
            }
        }
    }

    fn end_time_to_ghdl_time(&self) -> i64 {
        let Some(time) = self.requested_end_time else {
            return i64::MAX;
        };
        i64::try_from(time.0).unwrap_or(i64::MAX)
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
                reports: Vec::new(),
            },
            event_count: 0,
        }
    }

    /// Subscribes to the given signal elements.
    fn subscribe(&mut self, element_ids: &[SignalElementId]) {
        use std::collections::hash_map::Entry;

        let mut next_index = self.subscriptions.len();
        for &element_id in element_ids {
            if let Entry::Vacant(entry) = self.element_indices.entry(element_id) {
                debug!(?element_id, "subscribing to signal");
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

    /// Unsubscribes from the given signal elements.
    fn unsubscribe(&mut self, element_ids: &[SignalElementId]) {
        for element_id in element_ids {
            if let Some(SubscriptionIndex(index)) = self.element_indices.remove(element_id) {
                debug!(?element_id, "unsubscribing from signal");

                let index = index as usize;
                // Mark signal as unsubscribed in GHDL.
                let _ignore = ghdl_set_signal_subscription(
                    element_id.signal_id.0,
                    element_id.element_index,
                    SubscriptionIndex::INVALID,
                );

                self.subscriptions.swap_remove(index);
                self.events.signals.swap_remove(index);

                // If we removed from the middle, the former tail element moved into this slot.
                // Update both our index map and subscription index in GHDL accordingly.
                if let Some(&moved_element_id) = self.subscriptions.get(index) {
                    let moved_index = SubscriptionIndex(index as u32);
                    self.element_indices.insert(moved_element_id, moved_index);
                    let _ignore = ghdl_set_signal_subscription(
                        moved_element_id.signal_id.0,
                        moved_element_id.element_index,
                        moved_index,
                    );
                }
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

    fn notify_report(
        &mut self,
        time: LogicalTime,
        message: String,
        severity: Severity,
        file: String,
        line: u32,
        column: u32,
    ) {
        self.events.reports.push(Report {
            time,
            message,
            severity,
            file,
            line,
            column,
        });
        // TODO change this to a better accounting method
        self.event_count += 1;
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
                reports: Vec::new(),
            },
        );
        self.event_count = 0;
        Some(events)
    }
}

/// Decodes a VHDL Latin-1 string into a Rust `String`.
fn latin1_bytes_to_string(bytes: &[u8]) -> String {
    let mut string = String::with_capacity(bytes.len());
    string.extend(bytes.iter().map(|&byte| char::from(byte)));
    string
}

/// Decodes a null-terminated Latin-1 C string into a Rust `String`.
///
/// # Safety
///
/// - The memory pointed to by `ptr` must contain a valid nul terminator at the end of the string.
/// - `ptr` must be [valid] for reads of bytes up to and including the nul terminator.
/// - The nul terminator must be within `isize::MAX` from `ptr`
unsafe fn latin1_c_string_to_string(ptr: *const u8) -> String {
    if ptr.is_null() {
        String::new()
    } else {
        // SAFETY: Valid as per function preconditions, plus the memory won't be mutated.
        let string = unsafe { std::ffi::CStr::from_ptr(ptr.cast()) };
        latin1_bytes_to_string(string.to_bytes())
    }
}

/// Maps a GHDL severity level to the protocol severity enum.
fn severity_from_u8(severity: u8) -> Severity {
    match severity {
        0 => Severity::Note,
        1 => Severity::Warning,
        2 => Severity::Error,
        _ => Severity::Failure,
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
extern "C" fn adapter_init_websocket() -> *mut AdapterState {
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
        current_status: SimulationStatus::Running,
        requested_end_time: None,
    }))
}

/// Sets the initial state of the simulation: paused if interactive, running otherwise.
#[unsafe(no_mangle)]
extern "C" fn adapter_set_interactive(state: &mut AdapterState, is_interactive: bool) {
    state.current_status = if is_interactive {
        SimulationStatus::Paused
    } else {
        SimulationStatus::Running
    };
}

/// Processes commands from the WebSocket thread.
///
/// When `block` is non-zero, blocks until at least one command is received.
/// When `block` is zero, returns immediately if no commands are pending.
///
/// Writes the current run end time to `run_end_time`.
#[instrument(level = "debug", skip(state))]
#[unsafe(no_mangle)]
extern "C" fn adapter_process_commands(
    state: &mut AdapterState,
    run_end_time: &mut i64,
) -> SimulationStatus {
    let block = state.current_status == SimulationStatus::Paused;
    if block {
        match state.command_rx.recv() {
            Ok(cmd) => state.process_command(cmd),
            Err(_) => {
                error!("channel from WebSocket thread disconnected");
                *run_end_time = state.end_time_to_ghdl_time();
                return SimulationStatus::Stopped;
            },
        }
    };

    loop {
        use crossbeam_channel::TryRecvError::*;
        match state.command_rx.try_recv() {
            Ok(command) => state.process_command(command),
            Err(Empty) => break,
            Err(Disconnected) => {
                error!("channel from WebSocket thread disconnected");
                *run_end_time = state.end_time_to_ghdl_time();
                return SimulationStatus::Stopped;
            },
        }
    }

    *run_end_time = state.end_time_to_ghdl_time();
    state.current_status
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
}

/// Notifies the adapter that the current simulation cycle (one iteration of the simulation loop)
/// has finished.
#[unsafe(no_mangle)]
extern "C" fn adapter_update_simulation_time(state: &mut AdapterState) {
    state.subscriptions.update_time_range(state.time_for_events);
    if state.subscriptions.event_count >= EVENTS_PER_UPDATE_THRESHOLD {
        state.transmit_events();
    }
}

/// Notifies the adapter that the simulation is ready.
#[instrument(level = "debug", skip(state))]
#[unsafe(no_mangle)]
extern "C" fn adapter_notify_simulation_ready(state: &mut AdapterState) {
    if state.current_status == SimulationStatus::Paused {
        eprintln!("Simulation ready; waiting for start command from frontend");
    }
    let _ignore = state
        .update_tx
        .blocking_send(SimulationUpdate::StatusChanged(state.current_status, None));
}

/// Notifies the adapter that the simulation has stopped.
#[instrument(level = "debug", skip(state))]
#[unsafe(no_mangle)]
extern "C" fn adapter_notify_simulation_stopped(state: &mut AdapterState) {
    state.set_simulation_status(SimulationStatus::Stopped);
}

/// Pauses the simulation after a requested end time has been reached.
#[unsafe(no_mangle)]
extern "C" fn adapter_notify_simulation_paused(state: &mut AdapterState) {
    state.pause_simulation();
}

/// Records a signal value change at the given simulation time.
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

/// Records a report or assertion message at the current simulation time.
///
/// # Safety
///
/// - When `!msg_ptr.is_null() && msg_len > 0`, `msg_ptr` must point to a memory area
///   of size `msg_len` bytes that is valid for reads.
/// - When `!file_ptr.is_null()`, `file_ptr` must point to a valid null-terminated C string.
#[unsafe(no_mangle)]
extern "C" fn adapter_notify_report(
    state: &mut AdapterState,
    msg_ptr: *const u8,
    msg_len: u64,
    severity: u8,
    file_ptr: *const u8,
    line: u32,
    column: u32,
) {
    let message = if msg_ptr.is_null() || msg_len == 0 {
        String::new()
    } else {
        let len = usize::try_from(msg_len).unwrap_or(usize::MAX);
        // SAFETY: valid byte slice as per function precondition.
        let bytes = unsafe { std::slice::from_raw_parts(msg_ptr, len) };
        latin1_bytes_to_string(bytes)
    };
    // SAFETY: Valid as per function precondition.
    let file = unsafe { latin1_c_string_to_string(file_ptr) };
    state.subscriptions.notify_report(
        state.time_for_events,
        message,
        severity_from_u8(severity),
        file,
        line,
        column,
    );
}
