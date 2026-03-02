# GHDL Adapter

- [1. Overview](#1-overview)
- [2. Architecture](#2-architecture)
  - [2.1. Threading Model](#21-threading-model)
  - [2.2. Communication Channels](#22-communication-channels)
- [3. Key Components](#3-key-components)
  - [3.1. `AdapterState`](#31-adapterstate)
  - [3.2. WebSocket Server (`websocket_server.rs`)](#32-websocket-server-websocket_serverrs)
  - [3.3. Design Hierarchy Builder (`design.rs`)](#33-design-hierarchy-builder-designrs)
  - [3.4. Simulation Interface (`sim_interface.rs`)](#34-simulation-interface-sim_interfacers)
    - [3.4.1. `adapter_init_websocket(wait_for_gui: bool) -> *mut AdapterState`](#341-adapter_init_websocketwait_for_gui-bool---mut-adapterstate)
    - [3.4.2. `adapter_process_commands(state: &mut AdapterState, block: bool)`](#342-adapter_process_commandsstate-mut-adapterstate-block-bool)
    - [3.4.3. `adapter_register_design(state, root_instance, instance_count, signal_count)`](#343-adapter_register_designstate-root_instance-instance_count-signal_count)
    - [3.4.4. `adapter_set_next_event_time(state, physical_time, delta_cycle)`](#344-adapter_set_next_event_timestate-physical_time-delta_cycle)
    - [3.4.5. `adapter_update_simulation_time(state)`](#345-adapter_update_simulation_timestate)
    - [3.4.6. `adapter_notify_signal_event(state, subscription_index, value)`](#346-adapter_notify_signal_eventstate-subscription_index-value)
    - [3.4.7. `adapter_notify_simulation_status(state, status)`](#347-adapter_notify_simulation_statusstate-status)
- [4. Signal Subscription Mechanism](#4-signal-subscription-mechanism)
- [5. Event Batching](#5-event-batching)
- [6. Type Conversions](#6-type-conversions)
- [7. Error Handling](#7-error-handling)
- [8. Dependencies](#8-dependencies)
- [9. Limitations and TODOs](#9-limitations-and-todos)
- [10. Build Configuration](#10-build-configuration)

## 1. Overview

The GHDL Adapter is a Rust/C bridge layer that interfaces between the GHDL simulator (written in Ada) and simulator frontend services, enabling WebSocket-based communication and protocol translation.
This adapter is compiled as a static library (`staticlib`) that is linked into the GHDL executable. It provides C FFI functions that GHDL calls during simulation, and in turn makes FFI calls back into GHDL to control the simulator.

## 2. Architecture

### 2.1. Threading Model

The adapter operates with two main threads:

1. **Simulation Thread** (GHDL's main thread)
   - Runs the VHDL simulation
   - Calls adapter FFI functions to report events
   - Processes commands from the WebSocket thread via channels

2. **WebSocket Thread** (Tokio async runtime)
   - Manages WebSocket server and client connections
   - Handles bidirectional communication with frontend
   - Forwards commands to the simulation thread
   - Broadcasts updates to all connected clients

### 2.2. Communication Channels

- **Command Channel** (`crossbeam_channel`): WebSocket → Simulation
  - Carries simulation control commands (start, stop)
  - Delivers signal subscription requests

- **Update Channel** (`tokio::sync::mpsc`): Simulation → WebSocket
  - Streams signal events to clients
  - Broadcasts design hierarchy
  - Notifies status changes

## 3. Key Components

### 3.1. `AdapterState`

The central state structure passed to all FFI functions from GHDL:

- **Signal Management**: Tracks subscribed signals and their indices
- **Event Buffering**: Accumulates signal events before transmission
- **Status Tracking**: Maintains current and requested simulation status
- **Channel Endpoints**: Interfaces for inter-thread communication

### 3.2. WebSocket Server (`websocket_server.rs`)

- Listens on `127.0.0.1:8080`
- Uses `tokio-tungstenite` for WebSocket connections
- Implements the `hdl-simulation-protocol` (binary postcard encoding)
- Supports multiple simultaneous client connections
- Automatically sends design hierarchy to new clients
- 100ms periodic update interval for signal values

### 3.3. Design Hierarchy Builder (`design.rs`)

Extracts and converts GHDL's internal design representation:

- Queries GHDL for instances and signals via FFI
- Parses JSON-encoded GHDL data structures
- Retrieves signal names from GHDL's AST
- Converts VHDL types to protocol types (Bit, Logic, Integer, Real, Array)
- Builds hierarchical module tree recursively

### 3.4. Simulation Interface (`sim_interface.rs`)

FFI functions called by GHDL during simulation:

#### 3.4.1. `adapter_init_websocket(wait_for_gui: bool) -> *mut AdapterState`

Initializes the adapter:

- Creates Tokio runtime
- Sets up OpenTelemetry logging/tracing
- Spawns WebSocket server
- Returns adapter state pointer

#### 3.4.2. `adapter_process_commands(state: &mut AdapterState, block: bool)`

Processes commands from WebSocket clients:

- When `block=true`, waits for at least one command
- When `block=false`, processes all pending commands and returns immediately
- GHDL calls this periodically to check for control commands

#### 3.4.3. `adapter_register_design(state, root_instance, instance_count, signal_count)`

Called after elaboration to register the design hierarchy:

- Collects all signals and instances from GHDL
- Builds module hierarchy tree
- Broadcasts design to all connected clients

#### 3.4.4. `adapter_set_next_event_time(state, physical_time, delta_cycle)`

Sets the timestamp for the next batch of events.

#### 3.4.5. `adapter_update_simulation_time(state)`

Advances the event buffer's time range to the current simulation time.

#### 3.4.6. `adapter_notify_signal_event(state, subscription_index, value)`

Records a signal value change:

- Called by GHDL for each subscribed signal event
- Accumulates events in buffer for batch transmission
- Uses raw 64-bit value encoding

#### 3.4.7. `adapter_notify_simulation_status(state, status)`

Notifies status changes (Paused/Running/Stopped):

- Broadcasts to all WebSocket clients
- When stopping, blocks until acknowledgment (2s timeout) to ensure clients receive final events

## 4. Signal Subscription Mechanism

1. Client sends `TrackSignals` command via WebSocket
2. WebSocket thread forwards `Subscribe` command to simulation thread
3. Simulation thread calls `ghdl_set_signal_subscription(signal_id, index)` FFI function
4. GHDL marks the signal for tracking and calls `adapter_notify_signal_event` on changes
5. Events are buffered and periodically flushed to clients

## 5. Event Batching

Events are transmitted in batches to improve efficiency:

- Events accumulate in `EventsUpdate` buffer within a time range
- Flushed on:
  - Explicit `SendUpdate` command (triggered every 100ms)
  - New signal subscription
  - Status change to Stopped
- Each flush sends all accumulated events for all subscribed signals

## 6. Type Conversions

The adapter converts between GHDL's internal types and protocol types:

- **Bit/Logic**: Single-bit or bit-vector types
- **Discrete**: Integer ranges (converted to min/max bounds)
- **Float**: Floating-point ranges (with special hex deserialization from GHDL)
- **Array**: Multi-dimensional arrays (recursive element types)

## 7. Error Handling

- Panics on `abort` (configured in Cargo.toml profiles)
- Channel send failures are logged but generally expected during shutdown
- WebSocket errors cause connection removal
- Invalid subscription indices indicate bugs and cause panic

## 8. Dependencies

Key dependencies:

- `tokio`: Async runtime for WebSocket server
- `tokio-tungstenite`: WebSocket protocol implementation
- `crossbeam-channel`: MPSC channels for inter-thread communication
- `postcard`: Binary serialization for WebSocket messages
- `serde`/`serde_json`: JSON parsing for GHDL data structures
- `opentelemetry`: Distributed tracing support
- `hdl-simulation-protocol`: Protocol definitions
- `ghdl-ast`: GHDL AST type definitions

## 9. Limitations and TODOs

- **Unsubscribe**: Not yet implemented in GHDL data structures
- **Pause/Resume**: Acknowledged but not fully implemented
- **Entity/Architecture names**: Currently filled with "TODO" placeholders
- **Message re-encoding**: Currently re-encodes messages for each connection (optimization opportunity)
- **Some object kinds**: Not all GHDL object types are handled yet

## 10. Build Configuration

Built as a `staticlib` crate type for linking into GHDL. Requires Rust 2024 edition.
