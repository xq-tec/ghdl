--  Adapter for Rust-based JSON/buffer handling.
--  Copyright (C) 2026 xq-Tec GmbH
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.

with Interfaces; use Interfaces;
with System;

with Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;

package Adapter is
   pragma Warnings (Off, "the size of enums in C is implementation-defined");

   type RustBool is (False, True) with Size => 8;
   for RustBool use (False => 0, True => 1);
   pragma Convention (C, RustBool);

   type Simulation_Status is (Paused, Running, Stopped) with Size => 8;
   for Simulation_Status use (Paused => 0, Running => 1, Stopped => 2);
   pragma Convention (C, Simulation_Status);

   pragma Warnings (On, "the size of enums in C is implementation-defined");

   function Create_Buffer (Size : Unsigned_32) return System.Address
      with Inline;
   procedure Free_Buffer (Buffer : System.Address)
      with Inline;
   procedure Append (Buffer: System.Address; C : Character)
      with Inline;
   procedure Append (Buffer: System.Address; Str: String)
      with Inline;
   procedure Append (Buffer: System.Address; Value: Unsigned_32)
      with Inline;
   procedure Append (Buffer: System.Address; Value: Integer_32)
      with Inline;
   procedure Append (Buffer: System.Address; Value: Integer_64)
      with Inline;
   procedure Append (Buffer: System.Address; Value: IEEE_Float_64)
      with Inline;
   procedure Append (Buffer: System.Address; Value: Types.Direction_Type)
      with Inline;
   procedure Append_Escaped (Buffer: System.Address; Str: String)
      with Inline;
   procedure Flush (Buffer : System.Address)
      with Inline;

   procedure Init_Websocket (Wait_For_Gui : Boolean);

   --  Processes all commands in the queue from the WebSocket.
   --  When Block is True, blocks until at least one command has been received.
   --  When Block is False, returns immediately if no command is pending.
   procedure Process_Commands (Block : Boolean);
   function Requested_Simulation_Status return Simulation_Status;
   procedure Set_Next_Event_Time (Physical_Time : Std_Time; Delta_Cycle : Integer);
   procedure Update_Simulation_Time;
   procedure Notify_Simulation_Status (Status : Simulation_Status);

   function Get_Adapter_State return System.Address;

end Adapter;
