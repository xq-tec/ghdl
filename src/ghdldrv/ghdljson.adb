--  GHDL driver - json commands
--  Copyright (C) 2016 Tristan Gingold
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

with Types; use Types;
with Flags;
with Name_Table; use Name_Table;
with Vhdl.Nodes_Meta; use Vhdl.Nodes_Meta;
with Files_Map;
with Vhdl.Disp_Tree; use Vhdl.Disp_Tree;
with Libraries;
with Errorout; use Errorout;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with Adapter; use Adapter;

package body Ghdljson is

   procedure Append_Quoted_Attribute (Buffer : System.Address; Attr : String; Value : String) is
   begin
      Append (Buffer, ",""");
      Append (Buffer, Attr);
      Append (Buffer, """:""");
      Append (Buffer, Value);
      Append (Buffer, """");
   end Append_Quoted_Attribute;

   procedure Append_Attribute (Buffer : System.Address; Attr : String; Value : Uns32) is
   begin
      Append (Buffer, ",""");
      Append (Buffer, Attr);
      Append (Buffer, """:");
      Append (Buffer, Unsigned_32 (Value));
   end Append_Attribute;

   procedure Append_Attribute (Buffer : System.Address; Attr : String; Value : Int32) is
   begin
      Append (Buffer, ",""");
      Append (Buffer, Attr);
      Append (Buffer, """:");
      Append (Buffer, Integer_32 (Value));
   end Append_Attribute;

   procedure Append_Attribute (Buffer : System.Address; Attr : String; Value : Int64) is
   begin
      Append (Buffer, ",""");
      Append (Buffer, Attr);
      Append (Buffer, """:");
      Append (Buffer, Integer_64 (Value));
   end Append_Attribute;

   procedure Append_Attribute (Buffer : System.Address; Attr : String; Value : Fp64) is
      -- Strip leading blank from result of 'Image
      function Strip (S : String) return String is
      begin
         if S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Strip;
      -- Use 17 digits for printing, to avoid rounding errors
      type Print_Fp64 is digits 17;
   begin
      Append (Buffer, ",""");
      Append (Buffer, Attr);
      Append (Buffer, """:");
      -- TODO use binary representation
      Append (Buffer, Strip (Print_Fp64'Image (Print_Fp64 (Value))));
   end Append_Attribute;

   procedure Append_Attribute (Buffer : System.Address; Attr : String; Value : Boolean) is
   begin
      Append (Buffer, ",""");
      Append (Buffer, Attr);
      if Value then
         Append (Buffer, """:true");
      else
         Append (Buffer, """:false");
      end if;
   end Append_Attribute;

   procedure Append_Iir_Chain (Buffer : System.Address; Id : String; N : Iir) is
      El : Iir;
      Is_First_Item : Boolean := True;
   begin
      Append (Buffer, ",""");
      Append (Buffer, Id);
      Append (Buffer, """:[");

      El := N;
      while Is_Valid (El) loop
         if Is_First_Item then
            Is_First_Item := False;
         else
            Append (Buffer, ',');
         end if;
         Append (Buffer, Unsigned_32 (El));
         El := Get_Chain (El);
      end loop;

      Append (Buffer, ']');
   end Append_Iir_Chain;

   procedure Append_Iir_List (Buffer : System.Address; Id : String; L : Iir_List) is
      El : Iir;
      It : List_Iterator;
      Is_First_Item : Boolean := True;
   begin
      Append (Buffer, ",""");
      Append (Buffer, Id);
      Append (Buffer, """:");

      case L is
         when Iir_List_All =>
            Append (Buffer, """all""");

         when others =>
            Append (Buffer, '[');
            It := List_Iterate (L);
            while Is_Valid (It) loop
               El := Get_Element (It);
               if Is_First_Item then
                  Is_First_Item := False;
               else
                  Append (Buffer, ',');
               end if;
               Append (Buffer, Unsigned_32 (El));
               Next (It);
            end loop;
            Append (Buffer, ']');
      end case;
   end Append_Iir_List;

   procedure Append_Iir_Flist (Buffer : System.Address; Id : String; L : Iir_Flist) is
      El : Iir;
      Is_First_Item : Boolean := True;
   begin
      Append (Buffer, ",""");
      Append (Buffer, Id);
      Append (Buffer, """:");

      case L is
         when Iir_Flist_All =>
            Append (Buffer, """all""");

         when Iir_Flist_Others =>
            Append (Buffer, """others""");

         when others =>
            Append (Buffer, '[');
            for I in Flist_First .. Flist_Last (L) loop
               El := Get_Nth_Element (L, I);
               if Is_First_Item then
                  Is_First_Item := False;
               else
                  Append (Buffer, ',');
               end if;
               Append (Buffer, Unsigned_32 (El));
            end loop;
            Append (Buffer, ']');
      end case;
   end Append_Iir_Flist;

   function Get_Operator_Kind (K : Iir_Kind) return String is
   begin
      case K is
         when Iir_Kind_Identity_Operator => return """+""";
         when Iir_Kind_Negation_Operator => return """-""";
         when Iir_Kind_Absolute_Operator => return """abs""";
         when Iir_Kind_Not_Operator => return """not""";
         when Iir_Kind_Implicit_Condition_Operator => return """??""";
         when Iir_Kind_Condition_Operator => return """??""";
         when Iir_Kind_Reduction_And_Operator => return """and""";
         when Iir_Kind_Reduction_Or_Operator => return """or""";
         when Iir_Kind_Reduction_Nand_Operator => return """nand""";
         when Iir_Kind_Reduction_Nor_Operator => return """nor""";
         when Iir_Kind_Reduction_Xor_Operator => return """xor""";
         when Iir_Kind_Reduction_Xnor_Operator => return """xnor""";
         when Iir_Kind_And_Operator => return """and""";
         when Iir_Kind_Or_Operator => return """or""";
         when Iir_Kind_Nand_Operator => return """nand""";
         when Iir_Kind_Nor_Operator => return """nor""";
         when Iir_Kind_Xor_Operator => return """xor""";
         when Iir_Kind_Xnor_Operator => return """xnor""";
         when Iir_Kind_Equality_Operator => return """=""";
         when Iir_Kind_Inequality_Operator => return """/=""";
         when Iir_Kind_Less_Than_Operator => return """<""";
         when Iir_Kind_Less_Than_Or_Equal_Operator => return """<=""";
         when Iir_Kind_Greater_Than_Operator => return """>""";
         when Iir_Kind_Greater_Than_Or_Equal_Operator => return """>=""";
         when Iir_Kind_Match_Equality_Operator => return """?=""";
         when Iir_Kind_Match_Inequality_Operator => return """?/=""";
         when Iir_Kind_Match_Less_Than_Operator => return """?<""";
         when Iir_Kind_Match_Less_Than_Or_Equal_Operator => return """?<=""";
         when Iir_Kind_Match_Greater_Than_Operator => return """?>""";
         when Iir_Kind_Match_Greater_Than_Or_Equal_Operator => return """?>=""";
         when Iir_Kind_Sll_Operator => return """sll""";
         when Iir_Kind_Sla_Operator => return """sla""";
         when Iir_Kind_Srl_Operator => return """srl""";
         when Iir_Kind_Sra_Operator => return """sra""";
         when Iir_Kind_Rol_Operator => return """rol""";
         when Iir_Kind_Ror_Operator => return """ror""";
         when Iir_Kind_Addition_Operator => return """+""";
         when Iir_Kind_Substraction_Operator => return """-""";
         when Iir_Kind_Concatenation_Operator => return """&""";
         when Iir_Kind_Multiplication_Operator => return """*""";
         when Iir_Kind_Division_Operator => return """/""";
         when Iir_Kind_Modulus_Operator => return """mod""";
         when Iir_Kind_Remainder_Operator => return """rem""";
         when Iir_Kind_Exponentiation_Operator => return """**""";
         when others => return "";
      end case;
   end Get_Operator_Kind;

   function Get_Attribute_Kind (K : Iir_Kind) return String is
   begin
      case K is
         when Iir_Kind_Base_Attribute => return """base""";
         when Iir_Kind_Subtype_Attribute => return """subtype""";
         when Iir_Kind_Element_Attribute => return """element""";
         when Iir_Kind_Across_Attribute => return """across""";
         when Iir_Kind_Through_Attribute => return """through""";
         when Iir_Kind_Nature_Reference_Attribute => return """nature_reference""";
         when Iir_Kind_Left_Type_Attribute => return """left_type""";
         when Iir_Kind_Right_Type_Attribute => return """right_type""";
         when Iir_Kind_High_Type_Attribute => return """high_type""";
         when Iir_Kind_Low_Type_Attribute => return """low_type""";
         when Iir_Kind_Ascending_Type_Attribute => return """ascending_type""";
         when Iir_Kind_Image_Attribute => return """image""";
         when Iir_Kind_Value_Attribute => return """value""";
         when Iir_Kind_Pos_Attribute => return """pos""";
         when Iir_Kind_Val_Attribute => return """val""";
         when Iir_Kind_Succ_Attribute => return """succ""";
         when Iir_Kind_Pred_Attribute => return """pred""";
         when Iir_Kind_Leftof_Attribute => return """leftof""";
         when Iir_Kind_Rightof_Attribute => return """rightof""";
         when Iir_Kind_Signal_Slew_Attribute => return """signal_slew""";
         when Iir_Kind_Quantity_Slew_Attribute => return """quantity_slew""";
         when Iir_Kind_Ramp_Attribute => return """ramp""";
         when Iir_Kind_Zoh_Attribute => return """zoh""";
         when Iir_Kind_Ltf_Attribute => return """ltf""";
         when Iir_Kind_Ztf_Attribute => return """ztf""";
         when Iir_Kind_Dot_Attribute => return """dot""";
         when Iir_Kind_Integ_Attribute => return """integ""";
         when Iir_Kind_Quantity_Delayed_Attribute => return """quantity_delayed""";
         when Iir_Kind_Above_Attribute => return """above""";
         when Iir_Kind_Delayed_Attribute => return """delayed""";
         when Iir_Kind_Stable_Attribute => return """stable""";
         when Iir_Kind_Quiet_Attribute => return """quiet""";
         when Iir_Kind_Transaction_Attribute => return """transaction""";
         when Iir_Kind_Event_Attribute => return """event""";
         when Iir_Kind_Active_Attribute => return """active""";
         when Iir_Kind_Last_Event_Attribute => return """last_event""";
         when Iir_Kind_Last_Active_Attribute => return """last_active""";
         when Iir_Kind_Last_Value_Attribute => return """last_value""";
         when Iir_Kind_Driving_Attribute => return """driving""";
         when Iir_Kind_Driving_Value_Attribute => return """driving_value""";
         when Iir_Kind_Behavior_Attribute => return """behavior""";
         when Iir_Kind_Structure_Attribute => return """structure""";
         when Iir_Kind_Simple_Name_Attribute => return """simple_name""";
         when Iir_Kind_Instance_Name_Attribute => return """instance_name""";
         when Iir_Kind_Path_Name_Attribute => return """path_name""";
         when Iir_Kind_Converse_Attribute => return """converse""";
         when Iir_Kind_Left_Array_Attribute => return """left_array""";
         when Iir_Kind_Right_Array_Attribute => return """right_array""";
         when Iir_Kind_High_Array_Attribute => return """high_array""";
         when Iir_Kind_Low_Array_Attribute => return """low_array""";
         when Iir_Kind_Length_Array_Attribute => return """length_array""";
         when Iir_Kind_Ascending_Array_Attribute => return """ascending_array""";
         when Iir_Kind_Range_Array_Attribute => return """range_array""";
         when Iir_Kind_Reverse_Range_Array_Attribute => return """reverse_range_array""";
         when others => return "";
      end case;
   end Get_Attribute_Kind;

   procedure Append_Node_Metadata (Buffer : System.Address; K : Iir_Kind; N : Iir) is
      Is_Operator : Boolean := False;
      Is_Attribute : Boolean := False;
   begin

      Append (Buffer, "{""");

      case K is
         when Iir_Kinds_Monadic_Operator =>
            Append (Buffer, "unary_operator");
            Is_Operator := True;

         when Iir_Kinds_Dyadic_Operator =>
            Append (Buffer, "binary_operator");
            Is_Operator := True;

         when Iir_Kinds_Attribute =>
            Append (Buffer, "attribute");
            Is_Attribute := True;

         when others =>
            Append (Buffer, Get_Iir_Image (K));
      end case;

      Append (Buffer, """:{""id"":");
      Append (Buffer, Unsigned_32 (N));

      declare
         Loc : constant Location_Type := Get_Location (N);
         File : Source_File_Entry;
         Line_Pos : Source_Ptr;
         Line, Offset : Natural;
      begin
         if Loc /= No_Location then
            Files_Map.Location_To_Coord (Loc, File, Line_Pos, Line, Offset);
            Append (Buffer, ",""loc"":[");
            Append (Buffer, Unsigned_32 (File) - 1);
            Append (Buffer, ',');
            Append (Buffer, Integer_64 (Line));
            Append (Buffer, ',');
            Append (Buffer, Integer_64 (Offset) + 1);
            Append (Buffer, ']');
         end if;
      end;

      if Is_Operator then
         Append (Buffer, ",""kind"":");
         Append (Buffer, Get_Operator_Kind (K));
      end if;
      if Is_Attribute then
         Append (Buffer, ",""kind"":");
         Append (Buffer, Get_Attribute_Kind (K));
      end if;
   end Append_Node_Metadata;

   procedure Append_Field (Buffer : System.Address; F : Fields_Enum; N : Iir) is
   begin
      case Get_Field_Type (F) is
         when Type_Iir =>
            declare
               Val : Iir;
            begin
               Val := Get_Iir (N, F);
               if Val /= Null_Iir then
                  if Get_Field_Attribute (F) = Attr_Chain then
                     Append_Iir_Chain (Buffer, Get_Field_Image (F), Val);
                  else
                     Append_Attribute (Buffer, Get_Field_Image (F), Uns32 (Val));
                  end if;
               end if;
            end;

         when Type_Iir_List =>
            declare
               Val : Iir_List;
            begin
               Val := Get_Iir_List (N, F);
               if Val /= Null_Iir_List then
                  Append_Iir_List (Buffer, Get_Field_Image (F), Val);
               end if;
            end;

         when Type_Iir_Flist =>
            declare
               Val : Iir_Flist;
            begin
               Val := Get_Iir_Flist (N, F);
               if Val /= Null_Iir_Flist then
                  Append_Iir_Flist (Buffer, Get_Field_Image (F), Val);
               end if;
            end;

         when Type_String8_Id =>
            Append (Buffer, ",""");
            Append (Buffer, Get_Field_Image (F));
            Append (Buffer, """:""");
            Append_Escaped (Buffer, Image_String8 (N));
            Append (Buffer, """");

         when Type_PSL_NFA =>
            Append_Quoted_Attribute (Buffer, Get_Field_Image (F), "PSL-NFA");

         when Type_PSL_Node =>
            Append_Quoted_Attribute (Buffer, Get_Field_Image (F), "PSL-NODE");

         when Type_Source_Ptr =>
            null;

         when Type_Date_Type =>
            null;

         when Type_Number_Base_Type =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Number_Base_Type'Image (Get_Number_Base_Type (N, F)));

         when Type_Iir_Constraint =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_Constraint (Get_Iir_Constraint (N, F)));

         when Type_Iir_Mode =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_Mode (Get_Iir_Mode (N, F)));

         when Type_Iir_Force_Mode =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_Force_Mode (Get_Iir_Force_Mode (N, F)));

         when Type_Iir_Index32 =>
            Append_Attribute (
               Buffer,
               Get_Field_Image (F),
               Uns32 (Get_Iir_Index32 (N, F)));

         when Type_Int64 =>
            Append_Attribute (Buffer, Get_Field_Image (F), Get_Int64 (N, F));

         when Type_Boolean =>
            Append_Attribute (Buffer, Get_Field_Image (F), Get_Boolean (N, F));

         when Type_Iir_Staticness =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_Staticness (Get_Iir_Staticness (N, F)));

         when Type_Scalar_Size =>
            null;

         when Type_Date_State_Type =>
            null;

         when Type_Iir_All_Sensitized =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_All_Sensitized (Get_Iir_All_Sensitized (N, F)));

         when Type_Iir_Signal_Kind =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_Signal_Kind (Get_Iir_Signal_Kind (N, F)));

         when Type_Tri_State_Type =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Tri_State_Type (Get_Tri_State_Type (N, F)));

         when Type_Iir_Pure_State =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_Pure_State (Get_Iir_Pure_State (N, F)));

         when Type_Iir_Delay_Mechanism =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Iir_Delay_Mechanism (
                  Get_Iir_Delay_Mechanism (N, F)));

         when Type_Iir_Predefined_Functions =>
            declare
               Implicit : constant Iir_Predefined_Functions :=
                  Get_Iir_Predefined_Functions (N, F);
            begin
               if Implicit /= Iir_Predefined_None then
                  Append_Quoted_Attribute (
                     Buffer,
                     Get_Field_Image (F),
                     Image_Iir_Predefined_Functions (Implicit));
               end if;
            end;

         when Type_Direction_Type =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Direction_Type (Get_Direction_Type (N, F)));

         when Type_Iir_Int32 =>
            Append_Attribute (
               Buffer,
               Get_Field_Image (F),
               Int32 (Get_Iir_Int32 (N, F)));

         when Type_Int32 =>
            Append_Attribute (
               Buffer,
               Get_Field_Image (F),
               Get_Int32 (N, F));

         when Type_Fp64 =>
            Append_Attribute (
               Buffer,
               Get_Field_Image (F),
               Get_Fp64 (N, F));

         when Type_Time_Stamp_Id =>
            null;

         when Type_File_Checksum_Id =>
            null;

         when Type_Token_Type =>
            Append_Quoted_Attribute (
               Buffer,
               Get_Field_Image (F),
               Image_Token_Type (Get_Token_Type (N, F)));

         when Type_Name_Id =>
            declare
               Name : constant String := Image (Get_Name_Id (N, F));
               Has_Original : Boolean := False;
            begin
               if Name'Length > 0 then
                  Append (Buffer, ",""");
                  Append (Buffer, Get_Field_Image (F));
                  Append (Buffer, """:[""");
                  Append_Escaped (Buffer, Name);
                  Append (Buffer, """,");
                  -- Look up original identifier from source for regular identifiers (not extended
                  -- identifiers or char literals); also skip original identifier for design units
                  -- (because for them, Get_Location doesn't actually return the location of the
                  -- identifier).
                  if (Name (Name'First) /= '\' and Name (Name'First) /= ''')
                     and then Get_Kind (N) /= Iir_Kind_Design_Unit
                  then
                     declare
                        Loc : constant Location_Type := Get_Location (N);
                        File : Source_File_Entry;
                        Pos : Source_Ptr;
                     begin
                        if Loc /= No_Location then
                           Files_Map.Location_To_File_Pos (Loc, File, Pos);
                           if File > 3 then
                              declare
                                 Buf : constant File_Buffer_Acc := Files_Map.Get_File_Source (File);
                                 Len : constant Natural := Name'Length;
                              begin
                                 Append (Buffer, """");
                                 Append_Escaped (Buffer,
                                    String (Buf (Pos .. (Pos + Source_Ptr (Len) - 1))));
                                 Append (Buffer, """]");
                                 Has_Original := True;
                              end;
                           end if;
                        end if;
                     end;
                  end if;
                  if not Has_Original then
                     Append (Buffer, "null]");
                  end if;
               end if;
            end;

         when Type_Source_File_Entry =>
            null;
      end case;
   end Append_Field;

   procedure Encode_Iir (Buffer : System.Address; N : Iir) is
      Kind : constant Iir_Kind := Get_Kind (N);
   begin
      if Kind = Iir_Kind_Unused then
         Append (Buffer, "null" & ASCII.LF);
         return;
      end if;

      Append_Node_Metadata (Buffer, Kind, N);

      declare
         Fields : constant Fields_Array := Get_Fields (Get_Kind (N));
         F : Fields_Enum;
      begin
         for I in Fields'Range loop
            F := Fields (I);
            case F is
               when Field_Chain
                  | Field_Hash_Chain
                  | Field_Covered_Flag
                  | Field_Elab_Flag
                  | Field_Elaborated_Flag
                  | Field_Is_Forward_Ref
                  | Field_Is_Ref
                  | Field_Is_Within_Flag
                  | Field_Seen_Flag
                  | Field_Subprogram_Hash
                  | Field_Suspend_Flag
                  | Field_Suspend_State_Chain
                  | Field_Use_Flag
                  | Field_Visible_Flag
                     => null;
               when others => Append_Field (Buffer, F, N);
            end case;
         end loop;
      end;

      Append (Buffer, "}}" & ASCII.LF);
   end Encode_Iir;

   procedure Prepare_Ast is
      Library, Design_File, Design_Unit : Iir;
   begin
      --  Load work library.
      if not Setup_Libraries (True) then
         return;
      end if;

      Flags.Flag_Elaborate_With_Outdated := True;

      -- Load and parse all design units,
      -- including secondary units and transitive dependencies.
      Library := Libraries.Get_Libraries_Chain;
      while Is_Valid (Library) loop
         Design_File := Get_Design_File_Chain (Library);
         while Is_Valid (Design_File) loop
            Design_Unit := Get_First_Design_Unit (Design_File);
            while Is_Valid (Design_Unit) loop
               Load_Design_Unit (Design_Unit, No_Location);
               Design_Unit := Get_Chain (Design_Unit);
            end loop;
            Design_File := Get_Chain (Design_File);
         end loop;
         Library := Get_Chain (Library);
      end loop;
   end Prepare_Ast;

   procedure Encode_File_List (Buffer : System.Address) is
      Is_First : Boolean := True;
      File_Name, Dir_Name : Name_Id;

      First_Source : constant Source_File_Entry :=
         No_Source_File_Entry + 1;
      Last_Source : constant Source_File_Entry :=
         Files_Map.Get_Last_Source_File_Entry;

      File_Start : Unsigned_32;
   begin
      Append (Buffer, '[');
      for File in First_Source .. Last_Source loop
         if Is_First then
            Is_First := False;
         else
            Append (Buffer, ',');
         end if;
         Append (Buffer, "{""source"":""");
         Dir_Name := Files_Map.Get_Directory_Name (File);
         File_Name := Files_Map.Get_File_Name (File);
         Append_Escaped (Buffer, Files_Map.Get_Pathname (Dir_Name, File_Name));
         Append (Buffer, """,""start"":");
         File_Start := Unsigned_32 (Files_Map.File_To_Location (File));
         Append (Buffer, File_Start);
         Append (Buffer, ",""end"":");
         -- Subtract 2 for the two terminal EOT
         Append (Buffer, File_Start + Unsigned_32 (Files_Map.Get_Buffer_Length (File)) - 2);
         Append (Buffer, '}');
      end loop;
      Append (Buffer, ']');
   end Encode_File_List;

   procedure Encode_Library_List (Buffer : System.Address) is
      Is_First : Boolean := True;
      Library : Iir;
   begin
      Append (Buffer, '[');
      Library := Libraries.Get_Libraries_Chain;
      while Is_Valid (Library) loop
         if Is_First then
            Is_First := False;
         else
            Append (Buffer, ',');
         end if;
         Append (Buffer, Unsigned_32 (Library));
         Library := Get_Chain (Library);
      end loop;
      Append (Buffer, ']');
   end Encode_Library_List;

   procedure Encode_Metadata (Buffer : System.Address) is
   begin
      Append (Buffer, "{""first_id"":");
      Append (Buffer, Unsigned_32 (Get_First_Node));
      Append (Buffer, ",""last_id"":");
      Append (Buffer, Unsigned_32 (Get_Last_Node));
      Append (Buffer, ",""files"":");
      Encode_File_List (Buffer);
      Append (Buffer, ",""libraries"":");
      Encode_Library_List (Buffer);
      Append (Buffer, '}' & ASCII.LF);
   end Encode_Metadata;

   procedure Encode_Node (Buffer : System.Address; N : Iir) is
      -- The preceeding node if N is odd
      Aligned_N : constant Iir := Iir (Uns32 (N) and not 1);
   begin
      -- If N is an odd node and the preceeding even node takes two slots, output "null"
      if N /= Aligned_N and then Next_Node (Aligned_N) /= N then
         Append (Buffer, "null" & ASCII.LF);
      else
         Encode_Iir (Buffer, N);
      end if;
   end Encode_Node;

   procedure Encode_Ast (Buffer : System.Address) is
      Current : Iir := Get_First_Node;
      Last : constant Iir := Get_Last_Node;
      Counter : Natural := 0;
   begin
      while Uns32 (Current) <= Uns32 (Last) loop
         Encode_Node (Buffer, Current);
         Current := Iir (Uns32 (Current) + 1);
         Counter := Counter + 1;
         if Counter >= 256 then
            Adapter.Flush (Buffer);
            Counter := 0;
         end if;
      end loop;
   end Encode_Ast;

   --  Command --ast-to-json
   type Command_Ast_To_Json is new Command_Lib with null record;

   function Decode_Command (Cmd : Command_Ast_To_Json; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Ast_To_Json) return String;

   procedure Perform_Action (Cmd : in out Command_Ast_To_Json;
                             Files_Name : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Ast_To_Json; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "ast-to-json"
        or else Name = "--ast-to-json";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Ast_To_Json) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "ast-to-json"
        & ASCII.LF & "  Dump AST in JSON"
        & ASCII.LF & "  alias: --ast-to-json";
   end Get_Short_Help;

   procedure Dump_Ast is
      Buffer : constant System.Address := Adapter.Create_Buffer (64 * 1024);
   begin
      Encode_Metadata (Buffer);
      Encode_Ast (Buffer);
      Adapter.Flush (Buffer);

      Adapter.Free_Buffer (Buffer);
   end Dump_Ast;

   procedure Encode_Ast_Node (Buffer : System.Address; Node_Id : Unsigned_32);
   pragma Export (C, Encode_Ast_Node, "adapter_encode_ast_node");

   procedure Encode_Ast_Node (Buffer : System.Address; Node_Id : Unsigned_32) is
   begin
      Encode_Iir (Buffer, Iir (Node_Id));
   end Encode_Ast_Node;


   procedure Perform_Action (Cmd : in out Command_Ast_To_Json;
                             Files_Name : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);
   begin
      Success := False;

      Prepare_Ast;
      Dump_Ast;

      Success := True;
   exception
      when Compilation_Error =>
         Error ("json dump failed due to compilation error");
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Ast_To_Json);
   end Register_Commands;
end Ghdljson;
