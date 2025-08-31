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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

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

package body Ghdljson is

   Json : Unbounded_String;

   procedure Put (S : String) is
   begin
      Append (Json, S);
      if Length (Json) >= 32000 then
         Ada.Strings.Unbounded.Text_IO.Put (Json);
         Set_Unbounded_String (Json, "");
      end if;
   end Put;

   procedure Put (N : Int64) is
      Buffer : String (1 .. 20);
      Pos : Natural := Buffer'Last;
      Val : Uns64;
   begin
      declare
         pragma Suppress (Overflow_Check);
      begin
         -- Treat negative numbers as positive, add minus sign later
         if N < 0 then
            Val := Uns64 (-N);
         else
            Val := Uns64 (N);
         end if;
      end;

      -- Convert digits from right to left
      loop
         Buffer (Pos) :=
            Character'Val (Character'Pos ('0') + Integer (Val mod 10));
         Val := Val / 10;
         exit when Val = 0;
         Pos := Pos - 1;
      end loop;

      if N < 0 then
         Pos := Pos - 1;
         Buffer (Pos) := '-';
      end if;
      Append (Json, Buffer (Pos .. Buffer'Last));
   end Put;

   procedure Put (C : Character) is
   begin
      Append (Json, C);
   end Put;

   procedure Put_Quoted_Attribute (Attr : String; Value : String) is
   begin
      Put (",""");
      Put (Attr);
      Put (""":""");
      Put (Value);
      Put ("""");
   end Put_Quoted_Attribute;

   procedure Put_Attribute (Attr : String; Value : Int64) is
   begin
      Put (",""");
      Put (Attr);
      Put (""":");
      Put (Value);
   end Put_Attribute;

   procedure Put_Attribute (Attr : String; Value : Fp64) is
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
      Put (",""");
      Put (Attr);
      Put (""":");
      Put (Strip (Print_Fp64'Image (Print_Fp64 (Value))));
   end Put_Attribute;

   procedure Put_Attribute (Attr : String; Value : Boolean) is
   begin
      Put (",""");
      Put (Attr);
      if Value then
         Put (""":true");
      else
         Put (""":false");
      end if;
   end Put_Attribute;

   --  Espace special characters for JSON strings.
   function To_JSON (Str : String) return String is
      To_Hex : constant array (0 .. 15) of Character := "0123456789abcdef";
      --  The escape sequence uses up to 6 characters.
      Res : String (1 ..  6 * Str'Length);
      Idx : Positive;
      C : Character;
      C_Pos : Natural;
   begin
      Idx := Res'First;
      for I in Str'Range loop
         C := Str (I);
         case C is
            when '\' | '"' =>
               Res (Idx + 0) := '\';
               Res (Idx + 1) := C;
               Idx := Idx + 2;

            when Character'Val (0) .. Character'Val (31) =>
               Res (Idx + 0) := '\';
               Res (Idx + 1) := 'u';
               Res (Idx + 2) := '0';
               Res (Idx + 3) := '0';
               C_Pos := Character'Pos (C);
               Res (Idx + 4) := To_Hex (C_Pos / 16);
               Res (Idx + 5) := To_Hex (C_Pos mod 16);
               Idx := Idx + 6;

            when Character'Val (128) .. Character'Val (255) =>
               C_Pos := Character'Pos (C);
               Res (Idx + 0) := Character'Val (192 + (C_Pos / 64));
               Res (Idx + 1) := Character'Val (128 + (C_Pos mod 64));
               Idx := Idx + 2;

            when others =>
               Res (Idx) := C;
               Idx := Idx + 1;
         end case;
      end loop;
      return Res (1 .. Idx - 1);
   end To_JSON;

   procedure Disp_Iir_Chain (Id : String; N : Iir) is
      El : Iir;
      Is_First_Item : Boolean := True;
   begin
      Put (",""");
      Put (Id);
      Put (""":[");

      El := N;
      while Is_Valid (El) loop
         if Is_First_Item then
            Is_First_Item := False;
         else
            Put (',');
         end if;
         Put (Int64 (El));
         El := Get_Chain (El);
      end loop;

      Put (']');
   end Disp_Iir_Chain;

   procedure Disp_Iir_List (Id : String; L : Iir_List) is
      El : Iir;
      It : List_Iterator;
      Is_First_Item : Boolean := True;
   begin
      Put (",""");
      Put (Id);
      Put (""":");

      case L is
         when Iir_List_All =>
            Put ("""all""");

         when others =>
            Put ('[');
            It := List_Iterate (L);
            while Is_Valid (It) loop
               El := Get_Element (It);
               if Is_First_Item then
                  Is_First_Item := False;
               else
                  Put (',');
               end if;
               Put (Int64 (El));
               Next (It);
            end loop;
            Put (']');
      end case;
   end Disp_Iir_List;

   procedure Disp_Iir_Flist (Id : String; L : Iir_Flist) is
      El : Iir;
      Is_First_Item : Boolean := True;
   begin
      Put (",""");
      Put (Id);
      Put (""":");

      case L is
         when Iir_Flist_All =>
            Put ("""all""");

         when Iir_Flist_Others =>
            Put ("""others""");

         when others =>
            Put ('[');
            for I in Flist_First .. Flist_Last (L) loop
               El := Get_Nth_Element (L, I);
               if Is_First_Item then
                  Is_First_Item := False;
               else
                  Put (',');
               end if;
               Put (Int64 (El));
            end loop;
            Put (']');
      end case;
   end Disp_Iir_Flist;

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

   procedure Put_Node_Metadata (K : Iir_Kind; N : Iir) is
      Is_Operator : Boolean := False;
      Is_Attribute : Boolean := False;
   begin

      Put ("{""");

      case K is
         when Iir_Kinds_Monadic_Operator =>
            Put ("unary_operator");
            Is_Operator := True;

         when Iir_Kinds_Dyadic_Operator =>
            Put ("binary_operator");
            Is_Operator := True;

         when Iir_Kinds_Attribute =>
            Put ("attribute");
            Is_Attribute := True;

         when others =>
            Put (Get_Iir_Image (K));
      end case;

      Put (""":{""id"":");
      Put (Int64 (N));

      declare
         Loc : constant Location_Type := Get_Location (N);
         File : Source_File_Entry;
         Line_Pos : Source_Ptr;
         Line, Offset : Natural;
      begin
         if Loc /= No_Location then
            Files_Map.Location_To_Coord (Loc, File, Line_Pos, Line, Offset);
            Put (",""loc"":[");
            Put (Int64 (File) - 1);
            Put (',');
            Put (Int64 (Line));
            Put (',');
            Put (Int64 (Offset) + 1);
            Put (']');
         end if;
      end;

      if Is_Operator then
         Put (",""kind"":");
         Put (Get_Operator_Kind (K));
      end if;
      if Is_Attribute then
         Put (",""kind"":");
         Put (Get_Attribute_Kind (K));
      end if;
   end Put_Node_Metadata;

   procedure Disp_Field (F : Fields_Enum; N : Iir) is
   begin
      case Get_Field_Type (F) is
         when Type_Iir =>
            declare
               Val : Iir;
            begin
               Val := Get_Iir (N, F);
               if Val /= Null_Iir then
                  if Get_Field_Attribute (F) = Attr_Chain then
                     Disp_Iir_Chain (Get_Field_Image (F), Val);
                  else
                     Put_Attribute (Get_Field_Image (F), Int64 (Val));
                  end if;
               end if;
            end;

         when Type_Iir_List =>
            declare
               Val : Iir_List;
            begin
               Val := Get_Iir_List (N, F);
               if Val /= Null_Iir_List then
                  Disp_Iir_List (Get_Field_Image (F), Val);
               end if;
            end;

         when Type_Iir_Flist =>
            declare
               Val : Iir_Flist;
            begin
               Val := Get_Iir_Flist (N, F);
               if Val /= Null_Iir_Flist then
                  Disp_Iir_Flist (Get_Field_Image (F), Val);
               end if;
            end;

         when Type_String8_Id =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               To_JSON (Image_String8 (N)));

         when Type_PSL_NFA =>
            Put_Quoted_Attribute (Get_Field_Image (F), "PSL-NFA");

         when Type_PSL_Node =>
            Put_Quoted_Attribute (Get_Field_Image (F), "PSL-NODE");

         when Type_Source_Ptr =>
            null;

         when Type_Date_Type =>
            null;

         when Type_Number_Base_Type =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Number_Base_Type'Image (Get_Number_Base_Type (N, F)));

         when Type_Iir_Constraint =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Constraint (Get_Iir_Constraint (N, F)));

         when Type_Iir_Mode =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Mode (Get_Iir_Mode (N, F)));

         when Type_Iir_Force_Mode =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Force_Mode (Get_Iir_Force_Mode (N, F)));

         when Type_Iir_Index32 =>
            Put_Attribute (
               Get_Field_Image (F),
               Int64 (Get_Iir_Index32 (N, F)));

         when Type_Int64 =>
            Put_Attribute (Get_Field_Image (F), Get_Int64 (N, F));

         when Type_Boolean =>
            Put_Attribute (Get_Field_Image (F), Get_Boolean (N, F));

         when Type_Iir_Staticness =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Staticness (Get_Iir_Staticness (N, F)));

         when Type_Scalar_Size =>
            null;

         when Type_Date_State_Type =>
            null;

         when Type_Iir_All_Sensitized =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_All_Sensitized (Get_Iir_All_Sensitized (N, F)));

         when Type_Iir_Signal_Kind =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Signal_Kind (Get_Iir_Signal_Kind (N, F)));

         when Type_Tri_State_Type =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Tri_State_Type (Get_Tri_State_Type (N, F)));

         when Type_Iir_Pure_State =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Pure_State (Get_Iir_Pure_State (N, F)));

         when Type_Iir_Delay_Mechanism =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Delay_Mechanism (
                  Get_Iir_Delay_Mechanism (N, F)));

         when Type_Iir_Predefined_Functions =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Iir_Predefined_Functions (
                  Get_Iir_Predefined_Functions (N, F)));

         when Type_Direction_Type =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Direction_Type (Get_Direction_Type (N, F)));

         when Type_Iir_Int32 =>
            Put_Attribute (
               Get_Field_Image (F),
               Int64 (Get_Iir_Int32 (N, F)));

         when Type_Int32 =>
            Put_Attribute (
               Get_Field_Image (F),
               Int64 (Get_Int32 (N, F)));

         when Type_Fp64 =>
            Put_Attribute (
               Get_Field_Image (F),
               Get_Fp64 (N, F));

         when Type_Time_Stamp_Id =>
            null;

         when Type_File_Checksum_Id =>
            null;

         when Type_Token_Type =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               Image_Token_Type (Get_Token_Type (N, F)));

         when Type_Name_Id =>
            Put_Quoted_Attribute (
               Get_Field_Image (F),
               To_JSON (Image (Get_Name_Id (N, F))));

         when Type_Source_File_Entry =>
            null;
      end case;
   end Disp_Field;

   procedure Disp_Iir (N : Iir) is
      Kind : constant Iir_Kind := Get_Kind (N);
   begin
      if Kind = Iir_Kind_Unused then
         Put ("null");
         return;
      end if;

      Put_Node_Metadata (Kind, N);

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
               when others => Disp_Field (F, N);
            end case;
         end loop;
      end;

      Put ("}}");
   end Disp_Iir;

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

   procedure Output_File_List is
      Is_First : Boolean := True;
      File_Name, Dir_Name : Name_Id;

      First_Source : constant Source_File_Entry :=
         No_Source_File_Entry + 1;
      Last_Source : constant Source_File_Entry :=
         Files_Map.Get_Last_Source_File_Entry;
   begin
      Put ('[');
      for File in First_Source .. Last_Source loop
         if Is_First then
            Is_First := False;
         else
            Put (',');
         end if;
         Put ('"');
         Dir_Name := Files_Map.Get_Directory_Name (File);
         File_Name := Files_Map.Get_File_Name (File);
         Put (To_JSON (Files_Map.Get_Pathname (Dir_Name, File_Name)));
         Put ('"');
      end loop;
      Put (']');
   end Output_File_List;

   procedure Output_Library_List is
      Is_First : Boolean := True;
      Library : Iir;
   begin
      Put ('[');
      Library := Libraries.Get_Libraries_Chain;
      while Is_Valid (Library) loop
         if Is_First then
            Is_First := False;
         else
            Put (',');
         end if;
         Put (Int64 (Library));
         Library := Get_Chain (Library);
      end loop;
      Put (']');
   end Output_Library_List;

   procedure Output_Metadata is
   begin
      Put ("{""first_id"":");
      Put (Int64 (Get_First_Node));
      Put (",""files"":");
      Output_File_List;
      Put (",""libraries"":");
      Output_Library_List;
      Put ('}' & ASCII.LF);
   end Output_Metadata;

   procedure Output_Ast is
      Current : Iir := Get_First_Node;
      Next : Iir;
      Last : constant Iir := Get_Last_Node;
   begin
      while Int32 (Current) <= Int32 (Last) loop
         Disp_Iir (Current);
         Put (ASCII.LF);
         Next := Next_Node (Current);
         for I in Int32 (Current) + 1 .. Int32 (Next) - 1 loop
            Put ("null" & ASCII.LF);
         end loop;
         Current := Next;
      end loop;
   end Output_Ast;

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
   begin
      Output_Metadata;
      Output_Ast;
      Ada.Strings.Unbounded.Text_IO.Put (Json);
      Set_Unbounded_String (Json, "");
   end Dump_Ast;


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
