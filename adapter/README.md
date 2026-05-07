# GHDL Adapter

- [1. Overview](#1-overview)
- [2. Architecture](#2-architecture)
- [3. Signal Subscription Mechanism](#3-signal-subscription-mechanism)
- [4. Error Handling](#4-error-handling)
- [5. Limitations and TODOs](#5-limitations-and-todos)

## 1. Overview

The GHDL adapter serves as a command interface for controlling a simulation through a WebSocket connection.
This adapter is compiled as a static library and linked into the GHDL executable.

## 2. Architecture

The adapter operates in two threads:

1. **Simulation Thread** (GHDL's main thread)
   - Runs the VHDL simulation
   - Calls adapter FFI functions to report events
   - Processes commands from the WebSocket thread via channels

2. **WebSocket Thread** (Tokio async runtime)
   - Manages WebSocket server and a single client connection
   - Handles bidirectional communication with frontend
   - Forwards commands to the simulation thread
   - Sends updates to the connected client, if any

## 3. Signal Subscription Mechanism

1. Client sends `TrackSignals` command via WebSocket
2. WebSocket thread forwards `Subscribe` command to simulation thread
3. Simulation thread calls `ghdl_set_signal_subscription(signal_id, index)` FFI function
4. GHDL marks the signal for tracking and calls `adapter_notify_signal_event` on changes
5. Events are buffered and periodically flushed to the client when connected

## 4. Error Handling

Any panic triggers an abort of the process, to avoid UB from panics propagating across FFI boundaries.

## 5. Limitations and TODOs

- **Some object kinds**: Not all GHDL object types are handled yet
