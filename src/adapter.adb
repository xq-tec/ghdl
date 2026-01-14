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

   function Create_Buffer (Size : Uns32) return System.Address is
      function Adapter_Create_Buffer (Size : Uns32) return System.Address;
      pragma Import (C, Adapter_Create_Buffer, "adapter_create_buffer");
   begin
      return Adapter_Create_Buffer (Size);
   end Create_Buffer;

   procedure Free_Buffer (Buffer : System.Address) is
      procedure Adapter_Free_Buffer (Buffer : System.Address);
      pragma Import (C, Adapter_Free_Buffer, "adapter_free_buffer");
   begin
      Adapter_Free_Buffer (Buffer);
   end Free_Buffer;

   procedure Append (Buffer: System.Address; C : Character) is
      procedure Adapter_Append_Char (Buffer : System.Address; C : Uns8);
      pragma Import (C, Adapter_Append_Char, "adapter_append_char");
   begin
      Adapter_Append_Char (Buffer, Uns8 (Character'Pos (C)));
   end Append;

   procedure Append (Buffer: System.Address; Str: String) is
      procedure Adapter_Append_Str (Buffer : System.Address; Str : System.Address; Len : Uns64);
      pragma Import (C, Adapter_Append_Str, "adapter_append_str");
   begin
      Adapter_Append_Str (Buffer, Str (Str'First)'Address, Uns64 (Str'Length));
   end Append;

   procedure Append (Buffer: System.Address; Value: Uns32) is
      procedure Adapter_Append_U32 (Buffer : System.Address; Value : Uns32);
      pragma Import (C, Adapter_Append_U32, "adapter_append_u32");
   begin
      Adapter_Append_U32 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: Int32) is
      procedure Adapter_Append_I32 (Buffer : System.Address; Value : Int32);
      pragma Import (C, Adapter_Append_I32, "adapter_append_i32");
   begin
      Adapter_Append_I32 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: Int64) is
      procedure Adapter_Append_I64 (Buffer : System.Address; Value : Int64);
      pragma Import (C, Adapter_Append_I64, "adapter_append_i64");
   begin
      Adapter_Append_I64 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: Fp64) is
      procedure Adapter_Append_F64 (Buffer : System.Address; Value : Fp64);
      pragma Import (C, Adapter_Append_F64, "adapter_append_f64");
   begin
      Adapter_Append_F64 (Buffer, Value);
   end Append;

   procedure Append (Buffer: System.Address; Value: Direction_Type) is
   begin
      case Value is
         when Dir_To =>
            Append (Buffer, """to""");
         when Dir_Downto =>
            Append (Buffer, """downto""");
      end case;
   end Append;

   procedure Append_Escaped (Buffer: System.Address; Str: String) is
      procedure Adapter_Append_Escaped (Buffer : System.Address; Str : System.Address; Len : Uns64);
      pragma Import (C, Adapter_Append_Escaped, "adapter_append_escaped");
   begin
      Adapter_Append_Escaped (Buffer, Str (Str'First)'Address, Uns64 (Str'Length));
   end Append_Escaped;

   procedure Flush (Buffer : System.Address) is
      procedure Adapter_Flush (Buffer : System.Address);
      pragma Import (C, Adapter_Flush, "adapter_flush");
   begin
      Adapter_Flush (Buffer);
   end Flush;

end Adapter;
