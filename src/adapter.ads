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

package Adapter is

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

   procedure Init_Websocket;
   procedure Wait_For_Start_Simulation;
   procedure Wait_For_Stop_Simulation;

end Adapter;
