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
      Val : Int64 := N;
   begin
      -- Treat negative numbers as positive, add minus sign later
      if Val < 0 then
         Val := -Val;
      end if;

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

   procedure Put_Attribute (Attr : String; Value : String) is
   begin
      Put (",""");
      Put (Attr);
      Put (""":");
      Put (Value);
   end Put_Attribute;

   procedure Put_Attribute (Attr : String; Value : Int64) is
   begin
      Put (",""");
      Put (Attr);
      Put (""":");
      Put (Value);
   end Put_Attribute;

   procedure Put_Field (F : Fields_Enum; Value : String) is
   begin
      Put_Quoted_Attribute (Get_Field_Image (F), Value);
   end Put_Field;

   procedure Put_Field (F : Fields_Enum; Value : Int64) is
   begin
      Put_Attribute (Get_Field_Image (F), Value);
   end Put_Field;

   procedure Put_Field (F : Fields_Enum; Value : Fp64) is
      -- Strip leading blank from Fp64'Image
      function Strip (S : String) return String is
      begin
         if S (S'First) = ' ' then
            return S (S'First + 1 .. S'Last);
         else
            return S;
         end if;
      end Strip;
   begin
      Put_Quoted_Attribute (Get_Field_Image (F), Strip (Fp64'Image (Value)));
   end Put_Field;

   procedure Put_Field (F : Fields_Enum; Value : Boolean) is
   begin
      Put_Attribute (Get_Field_Image (F), Image_Boolean (Value));
   end Put_Field;

   --  Espace special characters for JSON strings.
   function To_JSON (Str : String) return String
   is
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
               Res (Idx + 0) := Character'Val(192 + (C_Pos / 64));
               Res (Idx + 1) := Character'Val(128 + (C_Pos mod 64));
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
      if Id'Length > 6 and then Id (Id'First .. Id'First + 5) = "first_" then
         Put (Id (Id'First + 6 .. Id'Last) & "s");
      elsif Id'Length > 6 and then Id (Id'Last - 5 .. Id'Last) = "_chain" then
         Put (Id (Id'First .. Id'Last - 6) & "s");
      else
         Put (Id);
      end if;
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

   procedure Disp_Iir_List (Id : String; L : Iir_List)
   is
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

   procedure Disp_Iir_Flist (Id : String; L : Iir_Flist)
   is
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

   procedure Disp_Iir (N : Iir) is
   begin
      Put ("{""");
      Put (Get_Iir_Image (Get_Kind (N)));
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

      declare
         Fields : constant Fields_Array := Get_Fields (Get_Kind (N));
         F : Fields_Enum;
      begin
         --  First attributes
         for I in Fields'Range loop
            F := Fields (I);
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
                           Put_Field (F, Int64 (Val));
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
                  Put_Field (F, To_JSON (Image_String8 (N)));
               when Type_PSL_NFA =>
                  Put_Field (F, "PSL-NFA");
               when Type_PSL_Node =>
                  Put_Field (F, "PSL-NODE");
               when Type_Source_Ptr =>
                  null;
               when Type_Date_Type =>
                  null;
               when Type_Number_Base_Type =>
                  Put_Field (F, Number_Base_Type'Image
                               (Get_Number_Base_Type (N, F)));
               when Type_Iir_Constraint =>
                  Put_Field (F, Image_Iir_Constraint
                               (Get_Iir_Constraint (N, F)));
               when Type_Iir_Mode =>
                  Put_Field (F, Image_Iir_Mode (Get_Iir_Mode (N, F)));
               when Type_Iir_Force_Mode =>
                  Put_Field (F, Image_Iir_Force_Mode
                               (Get_Iir_Force_Mode (N, F)));
               when Type_Iir_Index32 =>
                  Put_Field (F, Int64 (Get_Iir_Index32 (N, F)));
               when Type_Int64 =>
                  Put_Field (F, Get_Int64 (N, F));
               when Type_Boolean =>
                  Put_Field (F, Get_Boolean (N, F));
               when Type_Iir_Staticness =>
                  Put_Field (F, Image_Iir_Staticness
                               (Get_Iir_Staticness (N, F)));
               when Type_Scalar_Size =>
                  null;
               when Type_Date_State_Type =>
                  null;
               when Type_Iir_All_Sensitized =>
                  Put_Field (F, Image_Iir_All_Sensitized
                               (Get_Iir_All_Sensitized (N, F)));
               when Type_Iir_Signal_Kind =>
                  Put_Field (F, Image_Iir_Signal_Kind
                               (Get_Iir_Signal_Kind (N, F)));
               when Type_Tri_State_Type =>
                  Put_Field (F, Image_Tri_State_Type
                               (Get_Tri_State_Type (N, F)));
               when Type_Iir_Pure_State =>
                  Put_Field (F, Image_Iir_Pure_State
                               (Get_Iir_Pure_State (N, F)));
               when Type_Iir_Delay_Mechanism =>
                  Put_Field (F, Image_Iir_Delay_Mechanism
                               (Get_Iir_Delay_Mechanism (N, F)));
               when Type_Iir_Predefined_Functions =>
                  Put_Field (F, Image_Iir_Predefined_Functions
                               (Get_Iir_Predefined_Functions (N, F)));
               when Type_Direction_Type =>
                  Put_Field (F, Image_Direction_Type
                               (Get_Direction_Type (N, F)));
               when Type_Iir_Int32 =>
                  Put_Field (F, Int64 (Get_Iir_Int32 (N, F)));
               when Type_Int32 =>
                  Put_Field (F, Int64 (Get_Int32 (N, F)));
               when Type_Fp64 =>
                  Put_Field (F, Get_Fp64 (N, F));
               when Type_Time_Stamp_Id =>
                  null;
               when Type_File_Checksum_Id =>
                  null;
               when Type_Token_Type =>
                  Put_Field (F, Image_Token_Type (Get_Token_Type (N, F)));
               when Type_Name_Id =>
                  Put_Field (F, To_JSON (Image (Get_Name_Id (N, F))));
               when Type_Source_File_Entry =>
                  null;
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
         if Current /= Null_Iir then
            Disp_Iir (Current);
         else
            Put ("null");
         end if;
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

   procedure Perform_Action (Cmd : in out Command_Ast_To_Json;
                             Files_Name : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);
   begin
      Success := False;

      Prepare_Ast;
      Output_Metadata;
      Output_Ast;
      Ada.Strings.Unbounded.Text_IO.Put (Json);

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
