--  JSON-encoded elaborated design export.
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
-- AI NOTICE: Mostly generated, partially reviewed.

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;

with Adapter; use Adapter;
with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Annotations; use Elab.Vhdl_Annotations;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;
with Elab.Vhdl_Insts; use Elab.Vhdl_Insts;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Errorout; use Errorout;
with Grt.Options;
with Simul.Vhdl_Elab; use Simul.Vhdl_Elab;
with Types; use Types;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Nodes_Meta; use Vhdl.Nodes_Meta;

package body Design_Export is
   Design_Schema_Version : constant := 2;

   function To_Type_Acc is new Ada.Unchecked_Conversion
     (System.Address, Type_Acc);

   function To_Value_Acc is new Ada.Unchecked_Conversion
     (System.Address, Value_Acc);

   function Nbr_Sources_Element_Size return Natural is
   begin
      return Natural (Nbr_Sources_Type'Object_Size / Storage_Unit);
   end Nbr_Sources_Element_Size;

   function Nbr_Sources_Element_At (Base : System.Address;
                                    Index : Unsigned_32)
     return Nbr_Sources_Type
   is
      type Nbr_Sources_El_Ptr is access Nbr_Sources_Type;
      function To_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Nbr_Sources_El_Ptr);
      El_Size : constant Natural := Nbr_Sources_Element_Size;
      Offset : constant Address :=
        Base + Storage_Offset (El_Size * Natural (Index - 1));
   begin
      return To_Ptr (Offset).all;
   end Nbr_Sources_Element_At;

   Export_Kind_Type : constant Unsigned_32 := 0;
   Export_Kind_Value : constant Unsigned_32 := 1;
   Export_Kind_Memory : constant Unsigned_32 := 2;
   Export_Kind_Nbr_Sources : constant Unsigned_32 := 3;
   Export_Kind_Rec_El_Array : constant Unsigned_32 := 4;

   Intern_Ctx : System.Address;

   procedure Design_Export_Create is
      function Adapter_Design_Export_Create return System.Address;
      pragma Import (C, Adapter_Design_Export_Create,
                     "adapter_design_export_create");
   begin
      Intern_Ctx := Adapter_Design_Export_Create;
   end Design_Export_Create;

   procedure Design_Export_Free is
      procedure Adapter_Design_Export_Free (Ctx : System.Address);
      pragma Import (C, Adapter_Design_Export_Free,
                     "adapter_design_export_free");
   begin
      if Intern_Ctx /= System.Null_Address then
         Adapter_Design_Export_Free (Intern_Ctx);
         Intern_Ctx := System.Null_Address;
      end if;
   end Design_Export_Free;

   procedure Intern_Object (
      Kind : Unsigned_32;
      Ptr : System.Address;
      Size : Unsigned_32;
      Id : out Unsigned_32;
      Was_New : out Boolean
   ) is
      procedure Adapter_Design_Export_Intern (
         Ctx : System.Address;
         Kind : Unsigned_32;
         Ptr : System.Address;
         Size : Unsigned_32;
         Id : out Unsigned_32;
         Was_New : out RustBool
      );
      pragma Import (C, Adapter_Design_Export_Intern, "adapter_design_export_intern");
      FFI_Was_New : RustBool;
   begin
      if Ptr = System.Null_Address then
         Id := 0;
         Was_New := False;
      else
         Adapter_Design_Export_Intern
           (Intern_Ctx, Kind, Ptr, Size, Id, FFI_Was_New);
         Was_New := FFI_Was_New = True;
      end if;
   end Intern_Object;

   function Intern_Object (Kind : Unsigned_32;
                           Ptr : System.Address;
                           Size : Unsigned_32) return Unsigned_32 is
      Id : Unsigned_32;
      Was_New : Boolean;
   begin
      Intern_Object (Kind, Ptr, Size, Id, Was_New);
      return Id;
   end Intern_Object;

   procedure Intern_Object (Kind : Unsigned_32;
                            Ptr : System.Address;
                            Size : Unsigned_32) is
      Id : Unsigned_32;
      Was_New : Boolean;
   begin
      Intern_Object (Kind, Ptr, Size, Id, Was_New);
   end Intern_Object;

   procedure Intern_Object (Kind : Unsigned_32;
                            Ptr : System.Address;
                            Size : Unsigned_32;
                            Was_New : out Boolean) is
      Id : Unsigned_32;
   begin
      Intern_Object (Kind, Ptr, Size, Id, Was_New);
   end Intern_Object;

   procedure Design_Export_Get_Entry (Ctx : System.Address;
                                      Kind : Unsigned_32;
                                      Id : Unsigned_32;
                                      Ptr : out System.Address;
                                      Size : out Unsigned_32);
   pragma Import (C, Design_Export_Get_Entry,
                  "adapter_design_export_get_entry");

   function Export_Count (Kind : Unsigned_32) return Unsigned_32 is
      function Adapter_Design_Export_Count
        (Ctx : System.Address; Kind : Unsigned_32) return Unsigned_32;
      pragma Import (C, Adapter_Design_Export_Count,
                     "adapter_design_export_count");
   begin
      return Adapter_Design_Export_Count (Intern_Ctx, Kind);
   end Export_Count;

   procedure Append_Memory_Hex (Buffer : System.Address;
                                Kind : Unsigned_32;
                                Id : Unsigned_32) is
      procedure Adapter_Design_Export_Append_Memory_Hex
        (Buffer : System.Address;
         Ctx : System.Address;
         Kind : Unsigned_32;
         Id : Unsigned_32);
      pragma Import (C, Adapter_Design_Export_Append_Memory_Hex,
                     "adapter_design_export_append_memory_hex");
   begin
      Adapter_Design_Export_Append_Memory_Hex
        (Buffer, Intern_Ctx, Kind, Id);
   end Append_Memory_Hex;

   Line_Counter : Natural := 0;

   procedure Append_Line_End (Buffer : System.Address) is
   begin
      Append (Buffer, ASCII.LF);
      Line_Counter := Line_Counter + 1;
      if Line_Counter >= 256 then
         Flush (Buffer);
         Line_Counter := 0;
      end if;
   end Append_Line_End;

   procedure Append_Iir (Buffer : System.Address; N : Node) is
   begin
      if Is_Valid (N) then
         Append (Buffer, Unsigned_32 (N));
      else
         Append (Buffer, "0");
      end if;
   end Append_Iir;

   procedure Append_Instance_Ref (Buffer : System.Address;
                                  Inst : Synth_Instance_Acc) is
   begin
      if Inst = null then
         Append (Buffer, "0");
      else
         Append (Buffer, Unsigned_32 (Get_Instance_Id (Inst)));
      end if;
   end Append_Instance_Ref;

   procedure Append_Direction (Buffer : System.Address; Dir : Direction_Type) is
   begin
      case Dir is
         when Dir_To =>
            Append (Buffer, """to""");
         when Dir_Downto =>
            Append (Buffer, """downto""");
      end case;
   end Append_Direction;

   procedure Append_Offsets (Buffer : System.Address; Offs : Value_Offsets) is
   begin
      Append (Buffer, "{""net_off"":");
      Append (Buffer, Unsigned_32 (Offs.Net_Off));
      Append (Buffer, ",""mem_off"":");
      Append (Buffer, Integer_64 (Offs.Mem_Off));
      Append (Buffer, '}');
   end Append_Offsets;

   procedure Append_Tag_Open (Buffer : System.Address; Tag : String) is
   begin
      Append (Buffer, "{""");
      Append (Buffer, Tag);
      Append (Buffer, """:{");
   end Append_Tag_Open;

   procedure Append_Tag_Close (Buffer : System.Address) is
   begin
      Append (Buffer, "}}");
   end Append_Tag_Close;

   procedure Append_Sub_Signal (Buffer : System.Address; Sig : Sub_Signal_Type);
   procedure Discover_Type (T : Type_Acc);
   procedure Discover_Value (V : Value_Acc; Sz : Size_Type := 0);
   procedure Discover_Memory (Mem : Memory_Ptr; Sz : Size_Type);
   procedure Discover_Valtyp (Vt : Valtyp);

   function Size_To_U32 (Sz : Size_Type) return Unsigned_32 is
   begin
      if Sz > Size_Type (Unsigned_32'Last) then
         return Unsigned_32'Last;
      else
         return Unsigned_32 (Sz);
      end if;
   end Size_To_U32;

   procedure Discover_Memory (Mem : Memory_Ptr; Sz : Size_Type) is
   begin
      if Mem = null or else Sz = 0 then
         return;
      end if;
      Intern_Object (Export_Kind_Memory,
                     To_Address (Mem),
                     Size_To_U32 (Sz));
   end Discover_Memory;

   procedure Discover_Type (T : Type_Acc) is
      Was_New : Boolean;
   begin
      if T = null then
         return;
      end if;

      Intern_Object (Export_Kind_Type, T.all'Address, 0, Was_New);
      if not Was_New then
         return;
      end if;

      case T.Kind is
         when Type_Slice =>
            Discover_Type (T.Slice_Base);
            Discover_Type (T.Slice_El);
         when Type_Array
           | Type_Array_Unbounded
           | Type_Vector =>
            Discover_Type (T.Arr_El);
         when Type_Unbounded_Array | Type_Unbounded_Vector =>
            Discover_Type (T.Uarr_El);
            Discover_Type (T.Uarr_Idx);
         when Type_Record | Type_Unbounded_Record =>
            Discover_Type (T.Rec_Base);
            if T.Rec /= null then
               Intern_Object (Export_Kind_Rec_El_Array,
                              T.Rec.E'Address,
                              Unsigned_32 (T.Rec.E'Size / 8));
               for I in T.Rec.E'Range loop
                  Discover_Type (T.Rec.E (I).Typ);
               end loop;
            end if;
         when Type_Access =>
            Discover_Type (T.Acc_Acc);
         when Type_File =>
            Discover_Type (T.File_Typ);
         when others =>
            null;
      end case;
   end Discover_Type;

   procedure Discover_Value (V : Value_Acc; Sz : Size_Type := 0) is
   begin
      if V = null then
         return;
      end if;

      declare
         Was_New : Boolean;
         Mem_Sz : constant Size_Type := Sz;
      begin
         Intern_Object (Export_Kind_Value, V.all'Address, 0, Was_New);
         if not Was_New then
            return;
         end if;

         case V.Kind is
            when Value_Signal =>
               Discover_Value (V.Init, Sz);
            when Value_Const =>
               Discover_Value (V.C_Val, Sz);
            when Value_Alias =>
               Discover_Value (V.A_Obj, Sz);
               Discover_Type (V.A_Typ);
            when Value_Dyn_Alias =>
               Discover_Value (V.D_Obj, Sz);
               Discover_Type (V.D_Ptyp);
            when Value_Memory =>
               if V.Mem /= null and then Mem_Sz > 0 then
                  Discover_Memory (V.Mem, Mem_Sz);
               end if;
            when Value_Sig_Val =>
               if V.I_Sigs /= null then
                  Discover_Memory (V.I_Sigs, Mem_Sz);
               end if;
               if V.I_Vals /= null then
                  Discover_Memory (V.I_Vals, Mem_Sz);
               end if;
            when others =>
               null;
         end case;
      end;
   end Discover_Value;

   procedure Discover_Valtyp (Vt : Valtyp) is
      Sz : Size_Type := 0;
   begin
      Discover_Type (Vt.Typ);
      if Vt.Typ /= null then
         Sz := Vt.Typ.Sz;
      end if;
      Discover_Value (Vt.Val, Sz);
   end Discover_Valtyp;

   procedure Append_Sub_Signal (Buffer : System.Address; Sig : Sub_Signal_Type) is
   begin
      Append (Buffer, "{""base"":");
      Append (Buffer, Unsigned_32 (Sig.Base));
      Append (Buffer, ",""offs"":");
      Append_Offsets (Buffer, Sig.Offs);
      if Sig.Typ /= null then
         Append (Buffer, ",""type"":");
         Append (Buffer, Intern_Object (Export_Kind_Type, Sig.Typ.all'Address, 0));
      end if;
      Append (Buffer, '}');
   end Append_Sub_Signal;

   procedure Append_Signal_Kind (Buffer : System.Address; Kind : Signal_Kind) is
   begin
      case Kind is
         when Signal_User =>
            Append (Buffer, """user""");
         when Signal_Quiet =>
            Append (Buffer, """quiet""");
         when Signal_Stable =>
            Append (Buffer, """stable""");
         when Signal_Transaction =>
            Append (Buffer, """transaction""");
         when Signal_Delayed =>
            Append (Buffer, """delayed""");
         when Signal_Above =>
            Append (Buffer, """above""");
         when Signal_Guard =>
            Append (Buffer, """guard""");
         when Signal_None =>
            Append (Buffer, """none""");
      end case;
   end Append_Signal_Kind;

   procedure Append_Type_Kind (Buffer : System.Address; Kind : Type_Kind) is
   begin
      case Kind is
         when Type_Bit => Append (Buffer, """bit""");
         when Type_Logic => Append (Buffer, """logic""");
         when Type_Discrete => Append (Buffer, """discrete""");
         when Type_Float => Append (Buffer, """float""");
         when Type_Slice => Append (Buffer, """slice""");
         when Type_Vector => Append (Buffer, """vector""");
         when Type_Unbounded_Vector => Append (Buffer, """unbounded_vector""");
         when Type_Array => Append (Buffer, """array""");
         when Type_Array_Unbounded => Append (Buffer, """array_unbounded""");
         when Type_Unbounded_Array => Append (Buffer, """unbounded_array""");
         when Type_Unbounded_Record => Append (Buffer, """unbounded_record""");
         when Type_Record => Append (Buffer, """record""");
         when Type_Access => Append (Buffer, """access""");
         when Type_File => Append (Buffer, """file""");
         when Type_Protected => Append (Buffer, """protected""");
      end case;
   end Append_Type_Kind;

   procedure Append_Wkind (Buffer : System.Address; Wkind : Wkind_Type) is
   begin
      case Wkind is
         when Wkind_Undef => Append (Buffer, """undef""");
         when Wkind_Net => Append (Buffer, """net""");
         when Wkind_Sim => Append (Buffer, """sim""");
      end case;
   end Append_Wkind;

   procedure Append_Obj_Kind (Buffer : System.Address; Kind : Obj_Kind) is
   begin
      case Kind is
         when Obj_None => Append (Buffer, """none""");
         when Obj_Object => Append (Buffer, """object""");
         when Obj_Subtype => Append (Buffer, """subtype""");
         when Obj_Subprg => Append (Buffer, """subprg""");
         when Obj_Instance => Append (Buffer, """instance""");
         when Obj_Marker => Append (Buffer, """marker""");
      end case;
   end Append_Obj_Kind;

   procedure Append_Value_Kind (Buffer : System.Address; Kind : Value_Kind) is
   begin
      case Kind is
         when Value_Net => Append (Buffer, """net""");
         when Value_Wire => Append (Buffer, """wire""");
         when Value_Signal => Append (Buffer, """signal""");
         when Value_Memory => Append (Buffer, """memory""");
         when Value_File => Append (Buffer, """file""");
         when Value_Quantity => Append (Buffer, """quantity""");
         when Value_Terminal => Append (Buffer, """terminal""");
         when Value_Const => Append (Buffer, """const""");
         when Value_Alias => Append (Buffer, """alias""");
         when Value_Dyn_Alias => Append (Buffer, """dyn_alias""");
         when Value_Sig_Val => Append (Buffer, """sig_val""");
      end case;
   end Append_Value_Kind;

   procedure Encode_Type_Record (Buffer : System.Address; T : Type_Acc) is
      function Type_Id return Unsigned_32 is
      begin
         return Intern_Object (Export_Kind_Type, T.all'Address, 0);
      end Type_Id;
   begin
      Append_Tag_Open (Buffer, "type");
      Append (Buffer, """id"":");
      Append (Buffer, Type_Id);
      Append (Buffer, ",""type_kind"":");
      Append_Type_Kind (Buffer, T.Kind);
      Append (Buffer, ",""wkind"":");
      Append_Wkind (Buffer, T.Wkind);
      Append (Buffer, ",""align"":");
      Append (Buffer, Unsigned_32 (T.Al));
      Append_Attribute (Buffer, "is_global", T.Is_Global);
      Append_Attribute (Buffer, "is_static", T.Is_Static);
      Append_Attribute (Buffer, "is_bnd_static", T.Is_Bnd_Static);
      Append (Buffer, ",""sz"":");
      Append (Buffer, Integer_64 (T.Sz));
      Append (Buffer, ",""w"":");
      Append (Buffer, Unsigned_32 (T.W));

      case T.Kind is
         when Type_Bit | Type_Logic | Type_Discrete =>
            Append (Buffer, ",""left"":");
            Append (Buffer, Integer_64 (T.Drange.Left));
            Append (Buffer, ",""right"":");
            Append (Buffer, Integer_64 (T.Drange.Right));
            Append (Buffer, ",""dir"":");
            Append_Direction (Buffer, T.Drange.Dir);
            Append_Attribute (Buffer, "is_signed", T.Drange.Is_Signed);

         when Type_Float =>
            Append (Buffer, ",""left"":");
            Append (Buffer, IEEE_Float_64 (T.Frange.Left));
            Append (Buffer, ",""right"":");
            Append (Buffer, IEEE_Float_64 (T.Frange.Right));
            Append (Buffer, ",""dir"":");
            Append_Direction (Buffer, T.Frange.Dir);

         when Type_Slice =>
            Append (Buffer, ",""base_type"":");
            if T.Slice_Base = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.Slice_Base.all'Address, 0));
            end if;
            Append (Buffer, ",""len"":");
            Append (Buffer, Unsigned_32 (T.Slice_Len));
            Append (Buffer, ",""element_type"":");
            if T.Slice_El = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.Slice_El.all'Address, 0));
            end if;

         when Type_Array | Type_Array_Unbounded | Type_Vector =>
            Append (Buffer, ",""left"":");
            Append (Buffer, Integer_32 (T.Abound.Left));
            Append (Buffer, ",""right"":");
            Append (Buffer, Integer_32 (T.Abound.Right));
            Append (Buffer, ",""dir"":");
            Append_Direction (Buffer, T.Abound.Dir);
            Append (Buffer, ",""len"":");
            Append (Buffer, Unsigned_32 (T.Abound.Len));
            Append_Attribute (Buffer, "is_last", T.Alast);
            Append (Buffer, ",""element_type"":");
            if T.Arr_El = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.Arr_El.all'Address, 0));
            end if;

         when Type_Unbounded_Array | Type_Unbounded_Vector =>
            Append_Attribute (Buffer, "is_last", T.Ulast);
            Append (Buffer, ",""element_type"":");
            if T.Uarr_El = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.Uarr_El.all'Address, 0));
            end if;
            Append (Buffer, ",""index_type"":");
            if T.Uarr_Idx = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.Uarr_Idx.all'Address, 0));
            end if;

         when Type_Record | Type_Unbounded_Record =>
            Append (Buffer, ",""base_type"":");
            if T.Rec_Base = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.Rec_Base.all'Address, 0));
            end if;
            Append (Buffer, ",""fields"":[");
            if T.Rec /= null then
               declare
                  Is_First : Boolean := True;
               begin
                  for I in T.Rec.E'Range loop
                     if Is_First then
                        Is_First := False;
                     else
                        Append (Buffer, ',');
                     end if;
                     Append (Buffer, "{""type"":");
                     if T.Rec.E (I).Typ = null then
                        Append (Buffer, "0");
                     else
                        Append (Buffer, Intern_Object
                                  (Export_Kind_Type,
                                   T.Rec.E (I).Typ.all'Address, 0));
                     end if;
                     Append (Buffer, ",""offs"":");
                     Append_Offsets (Buffer, T.Rec.E (I).Offs);
                     Append (Buffer, ",""decl"":");
                     Append_Iir (Buffer, T.Rec.E (I).Decl);
                     Append (Buffer, '}');
                  end loop;
               end;
            end if;
            Append (Buffer, ']');

         when Type_Access =>
            Append (Buffer, ",""designated_type"":");
            if T.Acc_Acc = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.Acc_Acc.all'Address, 0));
            end if;
            Append (Buffer, ",""acc_type_sz"":");
            Append (Buffer, Integer_64 (T.Acc_Type_Sz));
            Append (Buffer, ",""acc_bnd_sz"":");
            Append (Buffer, Integer_64 (T.Acc_Bnd_Sz));

         when Type_File =>
            Append (Buffer, ",""file_type"":");
            if T.File_Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, T.File_Typ.all'Address, 0));
            end if;
            Append (Buffer, ",""signature"":");
            if T.File_Signature = null then
               Append (Buffer, "null");
            else
               Append (Buffer, '"');
               Append_Escaped (Buffer, T.File_Signature.all);
               Append (Buffer, '"');
            end if;

         when Type_Protected =>
            null;
      end case;

      Append_Tag_Close (Buffer);
   end Encode_Type_Record;

   procedure Encode_Value_Record (Buffer : System.Address; V : Value_Acc) is
      function Value_Id return Unsigned_32 is
      begin
         return Intern_Object (Export_Kind_Value, V.all'Address, 0);
      end Value_Id;
   begin
      Append_Tag_Open (Buffer, "value");
      Append (Buffer, """id"":");
      Append (Buffer, Value_Id);
      Append (Buffer, ",""val_kind"":");
      Append_Value_Kind (Buffer, V.Kind);

      case V.Kind is
         when Value_Net | Value_Wire =>
            Append (Buffer, ",""n"":");
            Append (Buffer, Unsigned_32 (V.N));
         when Value_Signal =>
            Append (Buffer, ",""signal"":");
            Append (Buffer, Unsigned_32 (V.S));
            Append (Buffer, ",""init"":");
            if V.Init = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Value, V.Init.all'Address, 0));
            end if;
         when Value_Memory =>
            Append (Buffer, ",""memory"":");
            if V.Mem = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Memory,
                          To_Address (V.Mem),
                          0));
            end if;
         when Value_File =>
            Append (Buffer, ",""file"":");
            Append (Buffer, Unsigned_32 (V.File));
         when Value_Quantity =>
            Append (Buffer, ",""quantity"":");
            Append (Buffer, Unsigned_32 (V.Q));
         when Value_Terminal =>
            Append (Buffer, ",""terminal"":");
            Append (Buffer, Unsigned_32 (V.T));
         when Value_Const =>
            Append (Buffer, ",""value"":");
            if V.C_Val = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Value, V.C_Val.all'Address, 0));
            end if;
            Append (Buffer, ",""loc"":");
            Append_Iir (Buffer, V.C_Loc);
            Append (Buffer, ",""net"":");
            Append (Buffer, Unsigned_32 (V.C_Net));
         when Value_Alias =>
            Append (Buffer, ",""obj"":");
            if V.A_Obj = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Value, V.A_Obj.all'Address, 0));
            end if;
            Append (Buffer, ",""type"":");
            if V.A_Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, V.A_Typ.all'Address, 0));
            end if;
            Append (Buffer, ",""offs"":");
            Append_Offsets (Buffer, V.A_Off);
         when Value_Dyn_Alias =>
            Append (Buffer, ",""obj"":");
            if V.D_Obj = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Value, V.D_Obj.all'Address, 0));
            end if;
            Append (Buffer, ",""poff"":");
            Append (Buffer, Unsigned_32 (V.D_Poff));
            Append (Buffer, ",""ptype"":");
            if V.D_Ptyp = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, V.D_Ptyp.all'Address, 0));
            end if;
            Append (Buffer, ",""voff"":");
            Append (Buffer, Unsigned_32 (V.D_Voff));
            Append (Buffer, ",""eoff"":");
            Append (Buffer, Unsigned_32 (V.D_Eoff));
         when Value_Sig_Val =>
            Append (Buffer, ",""sigs"":");
            if V.I_Sigs = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Memory,
                          To_Address (V.I_Sigs),
                          0));
            end if;
            Append (Buffer, ",""vals"":");
            if V.I_Vals = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Memory,
                          To_Address (V.I_Vals),
                          0));
            end if;
      end case;

      Append_Tag_Close (Buffer);
   end Encode_Value_Record;

   procedure Encode_Memory_Record (Buffer : System.Address; Id : Unsigned_32) is
      function Adapter_Design_Export_Get_Size
        (Ctx : System.Address; Kind : Unsigned_32; Id : Unsigned_32)
        return Unsigned_32;
      pragma Import (C, Adapter_Design_Export_Get_Size,
                     "adapter_design_export_get_size");
      Sz : Unsigned_32;
   begin
      Sz := Adapter_Design_Export_Get_Size
        (Intern_Ctx, Export_Kind_Memory, Id);
      Append_Tag_Open (Buffer, "memory");
      Append (Buffer, """id"":");
      Append (Buffer, Id);
      Append (Buffer, ",""size"":");
      Append (Buffer, Sz);
      Append (Buffer, ",""data"":");
      Append_Memory_Hex (Buffer, Export_Kind_Memory, Id);
      Append_Tag_Close (Buffer);
   end Encode_Memory_Record;

   procedure Discover_Nbr_Sources (Arr : Nbr_Sources_Arr_Acc) is
   begin
      if Arr = null then
         return;
      end if;
      Intern_Object (Export_Kind_Nbr_Sources,
                     Arr.all'Address,
                     Unsigned_32
                       (Arr'Length * Nbr_Sources_Element_Size));
   end Discover_Nbr_Sources;

   procedure Encode_Nbr_Sources_Record (Buffer : System.Address;
                                        Id : Unsigned_32;
                                        Base : System.Address;
                                        Byte_Size : Unsigned_32) is
      Count : Unsigned_32 := 0;
      Is_First : Boolean := True;
      El_Size : constant Natural := Nbr_Sources_Element_Size;
   begin
      if El_Size > 0 then
         Count := Byte_Size / Unsigned_32 (El_Size);
      end if;

      Append_Tag_Open (Buffer, "nbr_sources");
      Append (Buffer, """id"":");
      Append (Buffer, Id);
      Append (Buffer, ",""entries"":[");
      if Base /= System.Null_Address then
         for I in 1 .. Count loop
            declare
               Entry_Item : constant Nbr_Sources_Type :=
                 Nbr_Sources_Element_At (Base, I);
            begin
               if Is_First then
                  Is_First := False;
               else
                  Append (Buffer, ',');
               end if;
               Append (Buffer, "{""nbr_drivers"":");
               Append (Buffer, Unsigned_32 (Entry_Item.Nbr_Drivers));
               Append (Buffer, ",""nbr_conns"":");
               Append (Buffer, Unsigned_32 (Entry_Item.Nbr_Conns));
               Append (Buffer, ",""total"":");
               Append (Buffer, Unsigned_32 (Entry_Item.Total));
               Append (Buffer, ",""last_proc"":");
               Append (Buffer, Unsigned_32 (Entry_Item.Last_Proc));
               Append (Buffer, '}');
            end;
         end loop;
      end if;
      Append (Buffer, "]}}");
   end Encode_Nbr_Sources_Record;

   procedure Discover_Snapshot is
      Inst : Synth_Instance_Acc;
      Obj : Obj_Type;
   begin
      for Id in First_Instance_Id .. Get_Instance_Count loop
         Inst := Get_Instance_By_Id (Id);
         if Inst /= null then
            for Slot in 1 .. Get_Instance_Max_Objs (Inst) loop
               Obj := Get_Instance_Obj (Inst, Slot);
               case Obj.Kind is
                  when Obj_Object =>
                     Discover_Valtyp (Obj.Obj);
                  when Obj_Subtype =>
                     Discover_Type (Obj.T_Typ);
                  when Obj_Instance =>
                     null;
                  when others =>
                     null;
               end case;
            end loop;
         end if;
      end loop;

      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            Signal : Signal_Entry renames Signals_Table.Table (I);
         begin
            Discover_Type (Signal.Typ);
            if Signal.Val_Init /= null then
               Discover_Memory (Signal.Val_Init, Signal.Typ.Sz);
            end if;
            if Signal.Val /= null then
               Discover_Memory (Signal.Val, Signal.Typ.Sz);
            end if;
            if Signal.Kind = Signal_User and then Signal.Nbr_Sources /= null then
               Discover_Nbr_Sources (Signal.Nbr_Sources);
            end if;
         end;
      end loop;

      for I in Quantity_Table.First .. Quantity_Table.Last loop
         declare
            Item : Quantity_Entry renames Quantity_Table.Table (I);
         begin
            Discover_Type (Item.Typ);
            if Item.Val /= null then
               Discover_Memory (Item.Val, Item.Typ.Sz);
            end if;
         end;
      end loop;

      for I in Terminal_Table.First .. Terminal_Table.Last loop
         declare
            Item : Terminal_Entry renames Terminal_Table.Table (I);
         begin
            Discover_Type (Item.Across_Typ);
            Discover_Type (Item.Through_Typ);
            if Item.Ref_Val /= null and then Item.Across_Typ /= null then
               Discover_Memory (Item.Ref_Val, Item.Across_Typ.Sz);
            end if;
         end;
      end loop;
   end Discover_Snapshot;

   procedure Encode_Metadata (Buffer : System.Address) is
      Name : AdaString := (Ptr => System.Null_Address, Len => 0);
   begin
      if Grt.Options.Sim_Name_Valid then
         Name := (Ptr => Grt.Options.Sim_Name.all'Address,
                  Len => Unsigned_64 (Grt.Options.Sim_Name'Length));
      end if;

      Append (Buffer, "{""schema"":");
      Append (Buffer, Unsigned_32 (Design_Schema_Version));
      Append (Buffer, ",""root_instance"":");
      if Elab.Vhdl_Insts.Top_Instance = null then
         Append (Buffer, "0");
      else
         Append (Buffer, Unsigned_32
           (Get_Instance_Id (Elab.Vhdl_Insts.Top_Instance)));
      end if;
      Append (Buffer, ",""counts"":{");
      Append (Buffer, """elab_unit"":");
      Append (Buffer, Unsigned_32 (Elab_Units.Last));
      Append (Buffer, ",""instance"":");
      Append (Buffer, Unsigned_32 (Get_Instance_Count));
      Append (Buffer, ",""signal"":");
      Append (Buffer, Unsigned_32 (Signals_Table.Last));
      Append (Buffer, ",""process"":");
      Append (Buffer, Unsigned_32 (Processes_Table.Last));
      Append (Buffer, ",""driver"":");
      Append (Buffer, Unsigned_32 (Drivers_Table.Last));
      Append (Buffer, ",""sensitivity"":");
      Append (Buffer, Unsigned_32 (Sensitivity_Table.Last));
      Append (Buffer, ",""connection"":");
      Append (Buffer, Unsigned_32 (Connect_Table.Last));
      Append (Buffer, ",""disconnect"":");
      Append (Buffer, Unsigned_32 (Disconnect_Table.Last));
      Append (Buffer, ",""quantity"":");
      Append (Buffer, Unsigned_32 (Quantity_Table.Last));
      Append (Buffer, ",""terminal"":");
      Append (Buffer, Unsigned_32 (Terminal_Table.Last));
      Append (Buffer, ",""simultaneous"":");
      Append (Buffer, Unsigned_32 (Simultaneous_Table.Last));
      Append (Buffer, ",""complex_simultaneous"":");
      Append (Buffer, Unsigned_32 (Complex_Simultaneous_Table.Last));
      Append (Buffer, ",""type"":");
      Append (Buffer, Export_Count (Export_Kind_Type));
      Append (Buffer, ",""value"":");
      Append (Buffer, Export_Count (Export_Kind_Value));
      Append (Buffer, ",""memory"":");
      Append (Buffer, Export_Count (Export_Kind_Memory));
      Append (Buffer, ",""nbr_sources"":");
      Append (Buffer, Export_Count (Export_Kind_Nbr_Sources));
      Append (Buffer, "},""name"":");
      if Name.Len = 0 then
         Append (Buffer, "null");
      else
         Append (Buffer, '"');
         Append_Escaped (Buffer, Grt.Options.Sim_Name.all);
         Append (Buffer, '"');
      end if;
      Append (Buffer, '}');
      Append_Line_End (Buffer);
   end Encode_Metadata;

   procedure Emit_Elab_Units (Buffer : System.Address) is
   begin
      for I in Elab_Units.First .. Elab_Units.Last loop
         Append_Tag_Open (Buffer, "elab_unit");
         Append (Buffer, """id"":");
         Append (Buffer, Unsigned_32 (I));
         Append (Buffer, ",""unit"":");
         Append_Iir (Buffer, Elab_Units.Table (I));
         Append_Tag_Close (Buffer);
         Append_Line_End (Buffer);
      end loop;
   end Emit_Elab_Units;

   procedure Emit_Instances (Buffer : System.Address) is
      Inst : Synth_Instance_Acc;
   begin
      for Id in First_Instance_Id .. Get_Instance_Count loop
         Inst := Get_Instance_By_Id (Id);
         exit when Inst = null;
         Append_Tag_Open (Buffer, "instance");
         Append (Buffer, """id"":");
         Append (Buffer, Unsigned_32 (Id));
         Append (Buffer, ",""stmt"":");
         Append_Iir (Buffer, Get_Statement_Scope (Inst));
         Append (Buffer, ",""source"":");
         Append_Iir (Buffer, Get_Source_Scope (Inst));
         Append (Buffer, ",""parent"":");
         Append_Instance_Ref (Buffer, Get_Instance_Parent (Inst));
         Append (Buffer, ",""config"":");
         Append_Iir (Buffer, Get_Instance_Config (Inst));
         Append (Buffer, ",""caller"":");
         Append_Instance_Ref (Buffer, Get_Caller_Instance (Inst));
         Append (Buffer, ",""extra"":");
         Append_Instance_Ref (Buffer, Get_First_Extra_Instance (Inst));
         Append (Buffer, ",""block"":");
         Append_Iir (Buffer, Get_Instance_Block_Ref (Inst));
         Append (Buffer, ",""uninst"":");
         Append_Iir (Buffer, Get_Instance_Uninst_Ref (Inst));
         Append_Attribute (Buffer, "is_const", Get_Instance_Const (Inst));
         Append_Attribute (Buffer, "is_error", Is_Error (Inst));
         Append_Attribute
            (Buffer, "flag1", Get_Indiv_Signal_Assoc_Flag (Inst));
         Append_Attribute
            (Buffer, "flag2", Get_Indiv_Signal_Assoc_Parent_Flag (Inst));
         Append (Buffer, ",""foreign"":");
         Append (Buffer, Integer_32 (Get_Instance_Foreign (Inst)));
         Append (Buffer, ",""elab_objects"":");
         Append (Buffer, Unsigned_32 (Get_Instance_Elab_Objects (Inst)));
         Append (Buffer, ",""max_objects"":");
         Append (Buffer, Unsigned_32 (Get_Instance_Max_Objs (Inst)));
         Append_Tag_Close (Buffer);
         Append_Line_End (Buffer);
      end loop;
   end Emit_Instances;

   procedure Emit_Object_Slots (Buffer : System.Address) is
      Inst : Synth_Instance_Acc;
      Obj : Obj_Type;
      Decl : Node;
      Ann : Sim_Info_Acc;
   begin
      for Id in First_Instance_Id .. Get_Instance_Count loop
         Inst := Get_Instance_By_Id (Id);
         if Inst /= null then
            declare
               type Slot_Decl_Array is
                 array (Object_Slot_Type range <>) of Node;
               Slot_Decls : Slot_Decl_Array (1 .. Get_Instance_Max_Objs (Inst));
               Source : constant Node := Get_Source_Scope (Inst);
            begin
               for S in Slot_Decls'Range loop
                  Slot_Decls (S) := Null_Node;
               end loop;
               if Is_Valid (Source)
                 and then Get_Kind (Source) /= Iir_Kind_Foreign_Module
               then
                  if Has_Declaration_Chain (Get_Kind (Source)) then
                     Decl := Get_Declaration_Chain (Source);
                     while Is_Valid (Decl) loop
                        Ann := Get_Ann (Decl);
                        if Ann /= null
                          and then Ann.Slot /= Invalid_Object_Slot
                        then
                           Slot_Decls (Ann.Slot) := Decl;
                        end if;
                        Decl := Get_Chain (Decl);
                     end loop;
                  end if;
               end if;

               for Slot in 1 .. Get_Instance_Max_Objs (Inst) loop
                  Obj := Get_Instance_Obj (Inst, Slot);
                  if Obj.Kind /= Obj_None then
                     Append_Tag_Open (Buffer, "object_slot");
                     Append (Buffer, """instance"":");
                     Append (Buffer, Unsigned_32 (Id));
                     Append (Buffer, ",""slot"":");
                     Append (Buffer, Unsigned_32 (Slot));
                     Append (Buffer, ",""decl"":");
                     Append_Iir (Buffer, Slot_Decls (Slot));
                     Append (Buffer, ",""obj_kind"":");
                     Append_Obj_Kind (Buffer, Obj.Kind);

                     case Obj.Kind is
                        when Obj_Object =>
                           Append (Buffer, ",""type"":");
                           if Obj.Obj.Typ = null then
                              Append (Buffer, "0");
                           else
                              Append (Buffer, Intern_Object
                                        (Export_Kind_Type,
                                         Obj.Obj.Typ.all'Address, 0));
                           end if;
                           Append (Buffer, ",""value"":");
                           if Obj.Obj.Val = null then
                              Append (Buffer, "0");
                           else
                              Append (Buffer, Intern_Object
                                        (Export_Kind_Value,
                                         Obj.Obj.Val.all'Address, 0));
                           end if;
                        when Obj_Subtype =>
                           Append (Buffer, ",""type"":");
                           if Obj.T_Typ = null then
                              Append (Buffer, "0");
                           else
                              Append (Buffer, Intern_Object
                                        (Export_Kind_Type,
                                         Obj.T_Typ.all'Address, 0));
                           end if;
                           Append (Buffer, ",""def"":");
                           Append_Iir (Buffer, Obj.T_Def);
                        when Obj_Subprg =>
                           Append (Buffer, ",""decl"":");
                           Append_Iir (Buffer, Obj.S_Decl);
                        when Obj_Instance =>
                           Append (Buffer, ",""target_instance"":");
                           Append_Instance_Ref (Buffer, Obj.I_Inst);
                        when Obj_Marker =>
                           Append (Buffer, ",""mark"":null");
                        when others =>
                           null;
                     end case;

                     Append_Tag_Close (Buffer);
                     Append_Line_End (Buffer);
                  end if;
               end loop;
            end;
         end if;
      end loop;
   end Emit_Object_Slots;

   procedure Emit_Signals (Buffer : System.Address) is
   begin
      for I in Signals_Table.First .. Signals_Table.Last loop
         declare
            Signal : Signal_Entry renames Signals_Table.Table (I);
            Nbr_Id : Unsigned_32 := 0;
         begin
            if Signal.Kind = Signal_User and then Signal.Nbr_Sources /= null then
               Nbr_Id := Intern_Object
                 (Export_Kind_Nbr_Sources,
                  Signal.Nbr_Sources.all'Address,
                  Unsigned_32
                    (Signal.Nbr_Sources'Length * Nbr_Sources_Element_Size));
            end if;

            Append_Tag_Open (Buffer, "signal");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""signal_kind"":");
            Append_Signal_Kind (Buffer, Signal.Kind);
            Append (Buffer, ",""decl"":");
            Append_Iir (Buffer, Signal.Decl);
            Append (Buffer, ",""instance"":");
            Append_Instance_Ref (Buffer, Signal.Inst);
            Append (Buffer, ",""type"":");
            if Signal.Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, Signal.Typ.all'Address, 0));
            end if;
            Append (Buffer, ",""val_init"":");
            if Signal.Val_Init = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Memory,
                          To_Address (Signal.Val_Init),
                          Size_To_U32 (Signal.Typ.Sz)));
            end if;
            Append (Buffer, ",""val"":");
            if Signal.Val = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Memory,
                          To_Address (Signal.Val),
                          Size_To_U32 (Signal.Typ.Sz)));
            end if;
            Append (Buffer, ",""sensitivity"":");
            Append (Buffer, Unsigned_32 (Signal.Sensitivity));
            Append (Buffer, ",""connect"":");
            Append (Buffer, Unsigned_32 (Signal.Connect));
            Append (Buffer, ",""collapsed_by"":");
            Append (Buffer, Unsigned_32 (Signal.Collapsed_By));
            Append (Buffer, ",""collapsed_offs"":");
            Append_Offsets (Buffer, Signal.Collapsed_Offs);
            Append_Attribute (Buffer, "has_active", Signal.Has_Active);

            case Signal.Kind is
               when Signal_User =>
                  Append (Buffer, ",""drivers"":");
                  Append (Buffer, Unsigned_32 (Signal.Drivers));
                  Append (Buffer, ",""disconnect"":");
                  Append (Buffer, Unsigned_32 (Signal.Disconnect));
                  Append (Buffer, ",""nbr_sources"":");
                  Append (Buffer, Nbr_Id);
               when Signal_Quiet | Signal_Stable | Signal_Delayed
                 | Signal_Transaction =>
                  Append (Buffer, ",""time"":");
                  Append (Buffer, Integer_64 (Signal.Time));
                  Append (Buffer, ",""pfx"":");
                  Append_Sub_Signal (Buffer, Signal.Pfx);
               when others =>
                  null;
            end case;

            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Signals;

   procedure Emit_Processes (Buffer : System.Address) is
   begin
      for I in Processes_Table.First .. Processes_Table.Last loop
         declare
            Proc : Proc_Record_Type renames Processes_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "process");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""proc"":");
            Append_Iir (Buffer, Proc.Proc);
            Append (Buffer, ",""instance"":");
            Append_Instance_Ref (Buffer, Proc.Inst);
            Append (Buffer, ",""drivers"":");
            Append (Buffer, Unsigned_32 (Proc.Drivers));
            Append (Buffer, ",""sensitivity"":");
            Append (Buffer, Unsigned_32 (Proc.Sensitivity));
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Processes;

   procedure Emit_Drivers (Buffer : System.Address) is
   begin
      for I in Drivers_Table.First .. Drivers_Table.Last loop
         declare
            Drv : Driver_Entry renames Drivers_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "driver");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""sig"":");
            Append_Sub_Signal (Buffer, Drv.Sig);
            Append (Buffer, ",""proc"":");
            Append (Buffer, Unsigned_32 (Drv.Proc));
            Append (Buffer, ",""prev_sig"":");
            Append (Buffer, Unsigned_32 (Drv.Prev_Sig));
            Append (Buffer, ",""prev_proc"":");
            Append (Buffer, Unsigned_32 (Drv.Prev_Proc));
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Drivers;

   procedure Emit_Sensitivity (Buffer : System.Address) is
   begin
      for I in Sensitivity_Table.First .. Sensitivity_Table.Last loop
         declare
            Sens : Sensitivity_Entry renames Sensitivity_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "sensitivity");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""sig"":");
            Append_Sub_Signal (Buffer, Sens.Sig);
            Append (Buffer, ",""proc"":");
            Append (Buffer, Unsigned_32 (Sens.Proc));
            Append (Buffer, ",""prev_sig"":");
            Append (Buffer, Unsigned_32 (Sens.Prev_Sig));
            Append (Buffer, ",""prev_proc"":");
            Append (Buffer, Unsigned_32 (Sens.Prev_Proc));
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Sensitivity;

   procedure Emit_Connections (Buffer : System.Address) is
   begin
      for I in Connect_Table.First .. Connect_Table.Last loop
         declare
            Conn : Connect_Entry renames Connect_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "connection");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""formal"":");
            Append_Sub_Signal (Buffer, Conn.Formal);
            Append (Buffer, ",""actual"":");
            Append_Sub_Signal (Buffer, Conn.Actual);
            Append (Buffer, ",""formal_link"":");
            Append (Buffer, Unsigned_32 (Conn.Formal_Link));
            Append (Buffer, ",""actual_link"":");
            Append (Buffer, Unsigned_32 (Conn.Actual_Link));
            Append_Attribute (Buffer, "collapsed", Conn.Collapsed);
            Append (Buffer, ",""assoc"":");
            Append_Iir (Buffer, Conn.Assoc);
            Append (Buffer, ",""assoc_inst"":");
            Append_Instance_Ref (Buffer, Conn.Assoc_Inst);
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Connections;

   procedure Emit_Disconnects (Buffer : System.Address) is
   begin
      for I in Disconnect_Table.First .. Disconnect_Table.Last loop
         declare
            Disc : Disconnect_Entry renames Disconnect_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "disconnect");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""sig"":");
            Append_Sub_Signal (Buffer, Disc.Sig);
            Append (Buffer, ",""prev"":");
            Append (Buffer, Unsigned_32 (Disc.Prev));
            Append (Buffer, ",""val"":");
            Append (Buffer, Integer_64 (Disc.Val));
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Disconnects;

   procedure Emit_Quantities (Buffer : System.Address) is
   begin
      for I in Quantity_Table.First .. Quantity_Table.Last loop
         declare
            Item : Quantity_Entry renames Quantity_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "quantity");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""decl"":");
            Append_Iir (Buffer, Item.Decl);
            Append (Buffer, ",""instance"":");
            Append_Instance_Ref (Buffer, Item.Inst);
            Append (Buffer, ",""type"":");
            if Item.Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, Item.Typ.all'Address, 0));
            end if;
            Append (Buffer, ",""val"":");
            if Item.Val = null or else Item.Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Memory,
                          To_Address (Item.Val),
                          Size_To_U32 (Item.Typ.Sz)));
            end if;
            Append (Buffer, ",""sq_idx"":");
            Append (Buffer, Unsigned_32 (Item.Sq_Idx));
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Quantities;

   procedure Emit_Terminals (Buffer : System.Address) is
   begin
      for I in Terminal_Table.First .. Terminal_Table.Last loop
         declare
            Item : Terminal_Entry renames Terminal_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "terminal");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""decl"":");
            Append_Iir (Buffer, Item.Decl);
            Append (Buffer, ",""instance"":");
            Append_Instance_Ref (Buffer, Item.Inst);
            Append (Buffer, ",""across_type"":");
            if Item.Across_Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, Item.Across_Typ.all'Address, 0));
            end if;
            Append (Buffer, ",""through_type"":");
            if Item.Through_Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Type, Item.Through_Typ.all'Address, 0));
            end if;
            Append (Buffer, ",""ref_val"":");
            if Item.Ref_Val = null or else Item.Across_Typ = null then
               Append (Buffer, "0");
            else
               Append (Buffer, Intern_Object
                         (Export_Kind_Memory,
                          To_Address (Item.Ref_Val),
                          Size_To_U32 (Item.Across_Typ.Sz)));
            end if;
            Append (Buffer, ",""ref_idx"":");
            Append (Buffer, Unsigned_32 (Item.Ref_Idx));
            Append (Buffer, ",""term_idx"":");
            Append (Buffer, Unsigned_32 (Item.Term_Idx));
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Terminals;

   procedure Emit_Simultaneous (Buffer : System.Address) is
   begin
      for I in Simultaneous_Table.First .. Simultaneous_Table.Last loop
         declare
            Item : Simultaneous_Record renames Simultaneous_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "simultaneous");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""stmt"":");
            Append_Iir (Buffer, Item.Stmt);
            Append (Buffer, ",""instance"":");
            Append_Instance_Ref (Buffer, Item.Inst);
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;

      for I in Complex_Simultaneous_Table.First .. Complex_Simultaneous_Table.Last
      loop
         declare
            Item : Simultaneous_Record renames
              Complex_Simultaneous_Table.Table (I);
         begin
            Append_Tag_Open (Buffer, "complex_simultaneous");
            Append (Buffer, """id"":");
            Append (Buffer, Unsigned_32 (I));
            Append (Buffer, ",""stmt"":");
            Append_Iir (Buffer, Item.Stmt);
            Append (Buffer, ",""instance"":");
            Append_Instance_Ref (Buffer, Item.Inst);
            Append_Tag_Close (Buffer);
            Append_Line_End (Buffer);
         end;
      end loop;
   end Emit_Simultaneous;

   procedure Emit_Interned_Objects (Buffer : System.Address) is
      Entry_Ptr : System.Address;
      Entry_Size : Unsigned_32;

      procedure Emit_Kind (Kind : Unsigned_32) is
      begin
         for Id in 1 .. Export_Count (Kind) loop
            Design_Export_Get_Entry
              (Intern_Ctx, Kind, Id, Entry_Ptr, Entry_Size);
            case Kind is
               when Export_Kind_Type =>
                  if Entry_Ptr /= System.Null_Address then
                     Encode_Type_Record (Buffer, To_Type_Acc (Entry_Ptr));
                     Append_Line_End (Buffer);
                  end if;
               when Export_Kind_Value =>
                  if Entry_Ptr /= System.Null_Address then
                     Encode_Value_Record (Buffer, To_Value_Acc (Entry_Ptr));
                     Append_Line_End (Buffer);
                  end if;
               when Export_Kind_Memory =>
                  Encode_Memory_Record (Buffer, Id);
                  Append_Line_End (Buffer);
               when Export_Kind_Nbr_Sources =>
                  Encode_Nbr_Sources_Record
                    (Buffer, Id, Entry_Ptr, Entry_Size);
                  Append_Line_End (Buffer);
               when others =>
                  null;
            end case;
         end loop;
      end Emit_Kind;
   begin
      Emit_Kind (Export_Kind_Type);
      Emit_Kind (Export_Kind_Value);
      Emit_Kind (Export_Kind_Memory);
      Emit_Kind (Export_Kind_Nbr_Sources);
   end Emit_Interned_Objects;

   procedure Dump_Design is
      Buffer : System.Address;
   begin
      if Elab.Vhdl_Insts.Top_Instance = null then
         Error_Msg_Option ("interpreted elaboration snapshot is not available");
         return;
      end if;

      Design_Export_Create;

      Discover_Snapshot;

      Buffer := Create_Buffer (64 * 1024);
      Line_Counter := 0;
      Encode_Metadata (Buffer);
      Emit_Elab_Units (Buffer);
      Emit_Instances (Buffer);
      Emit_Object_Slots (Buffer);
      Emit_Signals (Buffer);
      Emit_Processes (Buffer);
      Emit_Drivers (Buffer);
      Emit_Sensitivity (Buffer);
      Emit_Connections (Buffer);
      Emit_Disconnects (Buffer);
      Emit_Quantities (Buffer);
      Emit_Terminals (Buffer);
      Emit_Simultaneous (Buffer);
      Emit_Interned_Objects (Buffer);
      -- Empty line to indicate end of data
      Append_Line_End (Buffer);

      Flush (Buffer);
      Free_Buffer (Buffer);
      Design_Export_Free;
   end Dump_Design;

end Design_Export;
