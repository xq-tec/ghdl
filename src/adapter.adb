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

package body Adapter is

   function Create_Buffer (Capacity : Unsigned_32) return System.Address is
      function Adapter_Create_Buffer (Capacity : Unsigned_32) return System.Address;
      pragma Import (C, Adapter_Create_Buffer, "adapter_create_buffer");
   begin
      return Adapter_Create_Buffer (Capacity);
   end Create_Buffer;

   procedure Free_Buffer (Buffer : System.Address) is
      procedure Adapter_Free_Buffer (Buffer : System.Address);
      pragma Import (C, Adapter_Free_Buffer, "adapter_free_buffer");
   begin
      Adapter_Free_Buffer (Buffer);
   end Free_Buffer;

   procedure Append (Buffer: System.Address; C : Character) is
      procedure Adapter_Append_Char (Buffer : System.Address; C : Unsigned_8);
      pragma Import (C, Adapter_Append_Char, "adapter_append_char");
   begin
      Adapter_Append_Char (Buffer, Unsigned_8 (Character'Pos (C)));
   end Append;

   procedure Append (Buffer: System.Address; Str: String) is
      procedure Adapter_Append_Str (Buffer : System.Address; Str : AdaString);
      pragma Import (C, Adapter_Append_Str, "adapter_append_str");

      FFI_Str : constant AdaString := (Ptr => Str'Address, Len => Unsigned_64 (Str'Length));
   begin
      Adapter_Append_Str (Buffer, FFI_Str);
   end Append;

   procedure Append (Buffer: System.Address; Value: Boolean) is
      procedure Adapter_Append_Bool (Buffer : System.Address; Value : RustBool);
      pragma Import (C, Adapter_Append_Bool, "adapter_append_bool");
   begin
      Adapter_Append_Bool (Buffer, (if Value then True else False));
   end Append;

   procedure Append (Buffer: System.Address; Value: Unsigned_32) is
      procedure Adapter_Append_U32 (Buffer : System.Address; Value : Unsigned_32);
      pragma Import (C, Adapter_Append_U32, "adapter_append_u32");
   begin
      Adapter_Append_U32 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: Integer_32) is
      procedure Adapter_Append_I32 (Buffer : System.Address; Value : Integer_32);
      pragma Import (C, Adapter_Append_I32, "adapter_append_i32");
   begin
      Adapter_Append_I32 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: Integer_64) is
      procedure Adapter_Append_I64 (Buffer : System.Address; Value : Integer_64);
      pragma Import (C, Adapter_Append_I64, "adapter_append_i64");
   begin
      Adapter_Append_I64 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: IEEE_Float_64) is
      procedure Adapter_Append_F64 (Buffer : System.Address; Value : IEEE_Float_64);
      pragma Import (C, Adapter_Append_F64, "adapter_append_f64");
   begin
      Adapter_Append_F64 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: Types.Direction_Type) is
   begin
      case Value is
         when Types.Dir_To =>
            Append (Buffer, """to""");
         when Types.Dir_Downto =>
            Append (Buffer, """downto""");
      end case;
   end Append;

   procedure Append_Escaped (Buffer: System.Address; Str: String) is
      procedure Adapter_Append_Escaped (Buffer : System.Address; Str : AdaString);
      pragma Import (C, Adapter_Append_Escaped, "adapter_append_escaped");

      FFI_Str : constant AdaString := (Ptr => Str'Address, Len => Unsigned_64 (Str'Length));
   begin
      Adapter_Append_Escaped (Buffer, FFI_Str);
   end Append_Escaped;

   procedure Append_Attribute (Buffer: System.Address;
                               Attr: String; Value: Boolean)
   is
      procedure Adapter_Append_Bool_Attribute (Buffer : System.Address;
                                               Attr : AdaString;
                                               Value: RustBool);
      pragma Import (C, Adapter_Append_Bool_Attribute, "adapter_append_bool_attribute");

      FFI_Attr : constant AdaString := (Ptr => Attr'Address, Len => Unsigned_64 (Attr'Length));
      FFI_Value : constant RustBool := (if Value then True else False);
   begin
      Adapter_Append_Bool_Attribute (Buffer, FFI_Attr, FFI_Value);
   end Append_Attribute;

   procedure Append_Attribute (Buffer: System.Address;
                               Attr: String; Value: Unsigned_32)
   is
      procedure Adapter_Append_U32_Attribute (Buffer : System.Address;
                                              Attr : AdaString;
                                              Value: Unsigned_32);
      pragma Import (C, Adapter_Append_U32_Attribute, "adapter_append_u32_attribute");

      FFI_Attr : constant AdaString := (Ptr => Attr'Address, Len => Unsigned_64 (Attr'Length));
   begin
      Adapter_Append_U32_Attribute (Buffer, FFI_Attr, Value);
   end Append_Attribute;

   procedure Append_Attribute (Buffer: System.Address;
                               Attr: String; Value: Integer_32)
   is
      procedure Adapter_Append_I32_Attribute (Buffer : System.Address;
                                              Attr : AdaString;
                                              Value: Integer_32);
      pragma Import (C, Adapter_Append_I32_Attribute, "adapter_append_i32_attribute");

      FFI_Attr : constant AdaString := (Ptr => Attr'Address, Len => Unsigned_64 (Attr'Length));
   begin
      Adapter_Append_I32_Attribute (Buffer, FFI_Attr, Value);
   end Append_Attribute;

   procedure Append_Attribute (Buffer: System.Address;
                               Attr: String; Value: Integer_64)
   is
      procedure Adapter_Append_I64_Attribute (Buffer : System.Address;
                                              Attr : AdaString;
                                              Value: Integer_64);
      pragma Import (C, Adapter_Append_I64_Attribute, "adapter_append_i64_attribute");

      FFI_Attr : constant AdaString := (Ptr => Attr'Address, Len => Unsigned_64 (Attr'Length));
   begin
      Adapter_Append_I64_Attribute (Buffer, FFI_Attr, Value);
   end Append_Attribute;

   procedure Append_Attribute (Buffer: System.Address;
                               Attr: String; Value: IEEE_Float_64)
   is
      procedure Adapter_Append_F64_Attribute (Buffer : System.Address;
                                              Attr : AdaString;
                                              Value: IEEE_Float_64);
      pragma Import (C, Adapter_Append_F64_Attribute, "adapter_append_f64_attribute");

      FFI_Attr : constant AdaString := (Ptr => Attr'Address, Len => Unsigned_64 (Attr'Length));
   begin
      Adapter_Append_F64_Attribute (Buffer, FFI_Attr, Value);
   end Append_Attribute;

   procedure Append_Attribute (Buffer: System.Address;
                               Attr: String; Value: String)
   is
      procedure Adapter_Append_String_Attribute (Buffer : System.Address;
                                                 Attr : AdaString;
                                                 Value : AdaString);
      pragma Import (C, Adapter_Append_String_Attribute, "adapter_append_string_attribute");

      FFI_Attr : constant AdaString := (Ptr => Attr'Address, Len => Unsigned_64 (Attr'Length));
      FFI_Value : constant AdaString := (Ptr => Value'Address, Len => Unsigned_64 (Value'Length));
   begin
      Adapter_Append_String_Attribute (Buffer, FFI_Attr, FFI_Value);
   end Append_Attribute;

   procedure Flush (Buffer : System.Address) is
      procedure Adapter_Flush (Buffer : System.Address);
      pragma Import (C, Adapter_Flush, "adapter_flush");
   begin
      Adapter_Flush (Buffer);
   end Flush;

   Adapter_State : System.Address := System.Null_Address;

   procedure Init_Websocket is
      function Adapter_Init_Websocket return System.Address;
      pragma Import (C, Adapter_Init_Websocket, "adapter_init_websocket");
   begin
      Adapter_State := Adapter_Init_Websocket;
   end Init_Websocket;

   procedure Set_Interactive (Is_Interactive : Boolean) is
      procedure Adapter_Set_Interactive (Adapter_State : System.Address; Is_Interactive : RustBool);
      pragma Import (C, Adapter_Set_Interactive, "adapter_set_interactive");
   begin
      Adapter_Set_Interactive (Adapter_State, (if Is_Interactive then True else False));
   end Set_Interactive;

   function Process_Commands (Run_End_Time : access Std_Time) return Simulation_Status
   is
      function Adapter_Process_Commands
        (Adapter_State : System.Address;
         Run_End_Time : access Std_Time)
        return Simulation_Status;
      pragma Import (C, Adapter_Process_Commands, "adapter_process_commands");
   begin
      return Adapter_Process_Commands (Adapter_State, Run_End_Time);
   end Process_Commands;

   procedure Set_Next_Event_Time (Physical_Time : Std_Time; Delta_Cycle : Integer) is
      procedure Adapter_Set_Next_Event_Time (Adapter_State : System.Address;
         Physical_Time : Integer_64;
         Delta_Cycle : Integer_64);
      pragma Import (C, Adapter_Set_Next_Event_Time, "adapter_set_next_event_time");
   begin
      Adapter_Set_Next_Event_Time (
         Adapter_State,
         Integer_64 (Physical_Time), Integer_64 (Delta_Cycle));
   end Set_Next_Event_Time;

   procedure Update_Simulation_Time is
      procedure Adapter_Update_Simulation_Time (Adapter_State : System.Address);
      pragma Import (C, Adapter_Update_Simulation_Time, "adapter_update_simulation_time");
   begin
      Adapter_Update_Simulation_Time (Adapter_State);
   end Update_Simulation_Time;

   procedure Notify_Simulation_Ready is
      procedure Adapter_Notify_Simulation_Ready (Adapter_State : System.Address);
      pragma Import (C, Adapter_Notify_Simulation_Ready, "adapter_notify_simulation_ready");
   begin
      Adapter_Notify_Simulation_Ready (Adapter_State);
   end Notify_Simulation_Ready;

   procedure Notify_Simulation_Stopped is
      procedure Adapter_Notify_Simulation_Stopped (Adapter_State : System.Address);
      pragma Import (C, Adapter_Notify_Simulation_Stopped, "adapter_notify_simulation_stopped");
   begin
      Adapter_Notify_Simulation_Stopped (Adapter_State);
   end Notify_Simulation_Stopped;

   procedure Notify_Simulation_Paused is
      procedure Adapter_Notify_Simulation_Paused (Adapter_State : System.Address);
      pragma Import (C, Adapter_Notify_Simulation_Paused, "adapter_notify_simulation_paused");
   begin
      Adapter_Notify_Simulation_Paused (Adapter_State);
   end Notify_Simulation_Paused;

   function Get_Adapter_State return System.Address is
   begin
      return Adapter_State;
   end Get_Adapter_State;

end Adapter;
