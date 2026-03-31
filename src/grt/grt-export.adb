--  GHDL Run Time (GRT) - Design Export.
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

--  This package provides Ada bindings for Rust adapter functions.

with System;

with Adapter; use Adapter;
with Elab.Vhdl_Annotations; use Elab.Vhdl_Annotations;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Insts;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Grt.Options;
with Grt.Signals; use Grt.Signals;
with Simul.Vhdl_Elab; use Simul.Vhdl_Elab;
with Simul.Vhdl_Simul; use Simul.Vhdl_Simul;
with Types;

package body Grt.Export is

   procedure Encode_Type (Buffer : System.Address; Typ : Type_Acc) is
   begin
      case Typ.Kind is
         when Type_Bit =>
            Append (Buffer, "{""bit"":{");
         when Type_Logic =>
            Append (Buffer, "{""logic"":{");
         when Type_Discrete =>
            Append (Buffer, "{""discrete"":{");
         when Type_Float =>
            Append (Buffer, "{""float"":{");
         when Type_Slice =>
            Append (Buffer, """slice""");
         when Type_Unbounded_Vector =>
            Append (Buffer, """unbounded_vector""");
         when Type_Array
            | Type_Array_Unbounded
            | Type_Vector =>
            Append (Buffer, "{""array"":{");
         when Type_Unbounded_Array =>
            Append (Buffer, """unbounded_array""");
         when Type_Unbounded_Record =>
            Append (Buffer, """unbounded_record""");
         when Type_Record =>
            Append (Buffer, """record""");
         when Type_Access =>
            Append (Buffer, """access""");
         when Type_File =>
            Append (Buffer, """file""");
         when Type_Protected =>
            Append (Buffer, """protected""");
      end case;

      case Typ.Kind is
         when Type_Bit
            | Type_Logic
            | Type_Discrete =>
            Append (Buffer, """left"":");
            Append (Buffer, Integer_64 (Typ.Drange.Left));
            Append (Buffer, ",""right"":");
            Append (Buffer, Integer_64 (Typ.Drange.Right));
            Append (Buffer, ",""dir"":");
            Append (Buffer, Typ.Drange.Dir);
            Append (Buffer, "}}");

         when Type_Float =>
            Append (Buffer, """left"":");
            Append (Buffer, IEEE_Float_64 (Typ.Frange.Left));
            Append (Buffer, ",""right"":");
            Append (Buffer, IEEE_Float_64 (Typ.Frange.Right));
            Append (Buffer, ",""dir"":");
            Append (Buffer, Typ.Frange.Dir);
            Append (Buffer, "}}");

         when Type_Array
            | Type_Array_Unbounded
            | Type_Vector =>
            Append (Buffer, """left"":");
            Append (Buffer, Integer_32 (Typ.Abound.Left));
            Append (Buffer, ",""right"":");
            Append (Buffer, Integer_32 (Typ.Abound.Right));
            Append (Buffer, ",""dir"":");
            Append (Buffer, Typ.Abound.Dir);
            Append (Buffer, ",""W"":");
            Append (Buffer, Unsigned_32 (Typ.W));
            Append (Buffer, ",""Sz"":");
            Append (Buffer, Integer_64 (Typ.Sz));
            Append (Buffer, ",""is_last"":");
            Append (Buffer, Typ.Alast);
            Append (Buffer, ",""element_type"":");
            Encode_Type (Buffer, Typ.Arr_El);
            Append (Buffer, "}}");

         when others =>
            null;
         --  when Type_Slice =>
         --     Slice_Base : Type_Acc;
         --     Slice_Len : Unsigned_32;
         --     Slice_El : Type_Acc;
         --  when Type_Array
         --     | Type_Array_Unbounded
         --     | Type_Vector =>
         --     Abound : Bound_Type;
         --     Alast : Boolean;  --  True for the last dimension
         --     Arr_El : Type_Acc;
         --  when Type_Unbounded_Array
         --     | Type_Unbounded_Vector =>
         --     Uarr_El : Type_Acc;
         --     Ulast : Boolean;
         --     Uarr_Idx : Type_Acc;
         --  when Type_Record
         --     | Type_Unbounded_Record =>
         --     --  The base type, used to have compatible layouts.
         --     Rec_Base : Type_Acc;
         --     --  The first elements is in the LSBs of the net.
         --     Rec : Rec_El_Array_Acc;
         --  when Type_Access =>
         --     Acc_Acc : Type_Acc;
         --     --  Memory size to store the type and its bounds.
         --     Acc_Type_Sz : Size_Type;
         --     Acc_Bnd_Sz : Size_Type;
         --  when Type_File =>
         --     File_Typ  : Type_Acc;
         --     File_Signature : String_Acc;
         --  when Type_Protected =>
         --     null;
      end case;
   end Encode_Type;

   procedure Encode_Signal (Buffer : System.Address; Signal_Id : Unsigned_32);
   pragma Export (C, Encode_Signal, "adapter_encode_signal");

   procedure Encode_Signal (Buffer : System.Address; Signal_Id : Unsigned_32) is
      Signal : Signal_Entry renames Signals_Table.Table (Signal_Index_Type (Signal_Id));
   begin
      Append (Buffer, "{""decl"":");
      Append (Buffer, Unsigned_32 (Signal.Decl));

      Append (Buffer, ",""width"":");
      Append (Buffer, Unsigned_32 (Signal.Typ.W));
      Append (Buffer, ",""type"":");
      Encode_Type (Buffer, Signal.Typ);

      Append (Buffer, '}');
   end Encode_Signal;

   procedure Encode_Instance (Buffer : System.Address; Instance_Id : Unsigned_32);
   pragma Export (C, Encode_Instance, "adapter_encode_instance");

   procedure Encode_Instance (Buffer : System.Address; Instance_Id : Unsigned_32) is
      Is_First : Boolean := True;
      procedure Append_Comma is
      begin
         if Is_First then
            Is_First := False;
         else
            Append (Buffer, ',');
         end if;
      end Append_Comma;

      Instance : constant Synth_Instance_Acc :=
         Get_Instance_By_Id ( Instance_Id_Type (Instance_Id));
      Object_Count : constant Object_Slot_Type := Get_Instance_Max_Objs (Instance);
      Obj : Obj_Type;
   begin
      Append (Buffer, "{""stmt"":");
      Append (Buffer, Unsigned_32 (Get_Statement_Scope (Instance)));
      Append (Buffer, ",""source"":");
      Append (Buffer, Unsigned_32 (Get_Source_Scope (Instance)));
      Append (Buffer, ",""objects"":[");
      for I in 1 .. Object_Count loop
         Obj := Get_Instance_Obj (Instance, I);
         case Obj.Kind is
            when Obj_Object =>
               Append_Comma;
               Append (Buffer, "{""object"":{""val_kind"":");
               case Obj.Obj.Val.Kind is
                  when Value_Net =>
                     Append (Buffer, "{""net"":{""n"":");
                     Append (Buffer, Unsigned_32 (Obj.Obj.Val.N));
                     Append (Buffer, "}}");
                  when Value_Wire =>
                     Append (Buffer, "{""wire"":{""n"":");
                     Append (Buffer, Unsigned_32 (Obj.Obj.Val.N));
                     Append (Buffer, "}}");
                  when Value_Signal =>
                     Append (Buffer, "{""signal"":{""id"":");
                     Append (Buffer, Unsigned_32 (Obj.Obj.Val.S));
                     Append (Buffer, "}}");
                  when Value_Memory =>
                     Append (Buffer, """memory""");
                  when Value_File =>
                     Append (Buffer, """file""");
                  when Value_Quantity =>
                     Append (Buffer, """quantity""");
                  when Value_Terminal =>
                     Append (Buffer, """terminal""");
                  when Value_Const =>
                     Append (Buffer, """const""");
                  when Value_Alias =>
                     Append (Buffer, """alias""");
                  when Value_Dyn_Alias =>
                     Append (Buffer, """dyn_alias""");
                  when Value_Sig_Val =>
                     Append (Buffer, """sig_val""");
               end case;
               Append (Buffer, "}}");

            when Obj_Instance =>
               if Obj.I_Inst /= null then
                  Append_Comma;
                  Append (Buffer, "{""instance"":{""id"":");
                  Append (Buffer, Unsigned_32 (Get_Instance_Id (Obj.I_Inst)));
                  Append (Buffer, "}}");
               end if;

            when Obj_None | Obj_Subtype | Obj_Marker | Obj_Subprg =>
               null;
         end case;
      end loop;
      Append (Buffer, "]}");
   end Encode_Instance;

   procedure Register_Design is
      procedure Adapter_Register_Design (
         Adapter_State : System.Address;
         Root_Instance, Instance_Count, Signal_Count : Unsigned_32;
         Name_Str : System.Address; Name_Len : Unsigned_64);
      pragma Import (C, Adapter_Register_Design, "adapter_register_design");

      Name_Str : System.Address := System.Null_Address;
      Name_Len : Unsigned_64 := 0;
   begin
      if Grt.Options.Sim_Name_Valid then
         Name_Str := Grt.Options.Sim_Name.all'Address;
         Name_Len := Unsigned_64 (Grt.Options.Sim_Name'Length);
      end if;

      Adapter_Register_Design (
         Get_Adapter_State,
         Unsigned_32 (Get_Instance_Id (Elab.Vhdl_Insts.Top_Instance)),
         Unsigned_32 (Get_Instance_Count),
         Unsigned_32 (Signals_Table.Last),
         Name_Str, Name_Len);
   end Register_Design;

   procedure Set_Signal_Subscription (Signal_Id, Element_Index : Unsigned_32;
                                      Subscription : Subscription_Index) is
      -- This is the Signal_Entry type in simul-vhdl_elab.ads
      Signal : Signal_Entry renames Signals_Table.Table (Signal_Index_Type (Signal_Id));
      Sig_Ptr : constant Ghdl_Signal_Ptr :=
         Read_Sig (Sig_Index (Signal.Sig, Types.Uns32 (Element_Index)));
   begin
      Sig_Ptr.Subscription := Subscription;
   end Set_Signal_Subscription;

   procedure Notify_Signal_Event (Sig_Idx : Subscription_Index; Value : Unsigned_64) is
      procedure Adapter_Notify_Signal_Event (Adapter_State : System.Address;
                                             Sig_Idx : Unsigned_32;
                                             Value : Unsigned_64);
      pragma Import (C, Adapter_Notify_Signal_Event, "adapter_notify_signal_event");
   begin
      Adapter_Notify_Signal_Event (Get_Adapter_State, Unsigned_32 (Sig_Idx), Value);
   end Notify_Signal_Event;

end Grt.Export;
