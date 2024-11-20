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
with Ghdlprint; use Ghdlprint;
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

   procedure Put (C : Character) is
   begin
      Append (Json, C);
   end Put;

   procedure Disp_Iir (N : Iir; Requires_Brace : Boolean);

   Is_First : Boolean;

   procedure Put_Quoted_Attribute (Attr : String; Value : String) is
   begin
      if Is_First then
         Put ('"');
         Is_First := False;
      else
         Put (",""");
      end if;
      Put (Attr);
      Put (""":""");
      Put (Value);
      Put ("""");
   end Put_Quoted_Attribute;

   procedure Put_Attribute (Attr : String; Value : String) is
   begin
      if Is_First then
         Put ('"');
         Is_First := False;
      else
         Put (",""");
      end if;
      Put (Attr);
      Put (""":");
      Put (Value);
   end Put_Attribute;

   procedure Put_Field (F : Fields_Enum; Value : String) is
      -- Strip leading blank if any.
      function Strip (S : String) return String
      is
         F : constant Natural := S'First;
      begin
         if F > S'Last then
            return "";
         elsif S (F) = ' ' then
            return S (F + 1 .. S'Last);
         else
            return S;
         end if;
      end Strip;
   begin
      Put_Quoted_Attribute (Get_Field_Image (F), Strip (Value));
   end Put_Field;

   procedure Put_Field (F : Fields_Enum; Value : Iir_Index32) is
   begin
      Put_Attribute (Get_Field_Image (F), Iir_Index32'Image (Value));
   end Put_Field;

   procedure Put_Field (F : Fields_Enum; Value : Iir_Int32) is
   begin
      Put_Attribute (Get_Field_Image (F), Iir_Int32'Image (Value));
   end Put_Field;

   procedure Put_Field (F : Fields_Enum; Value : Int64) is
   begin
      Put_Attribute (Get_Field_Image (F), Int64'Image (Value));
   end Put_Field;

   procedure Put_Field (F : Fields_Enum; Value : Fp64) is
   begin
      Put_Attribute (Get_Field_Image (F), Fp64'Image (Value));
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

   procedure Disp_Iir_Ref (Id : String; N : Iir) is
   begin
      if N = Null_Iir then
         return;
      end if;

      Put ('"');
      Put (Id);
      Put (".ref"":");
      Put (Iir'Image (N));
   end Disp_Iir_Ref;

   procedure Disp_Iir_Chain_Elements (Chain : Iir)
   is
      El : Iir;
      Is_First : Boolean := True;
   begin
      El := Chain;
      while Is_Valid (El) loop
         if El /= Null_Iir then
            if Is_First then
               Is_First := False;
            else
               Put (',');
            end if;
            Disp_Iir (El, True);
         end if;
         El := Get_Chain (El);
      end loop;
   end Disp_Iir_Chain_Elements;

   procedure Disp_Iir_Chain (Id : String; N : Iir) is
   begin
      if N = Null_Iir then
         return;
      end if;

      Put ('"');
      Put (Id);
      Put (""":");
      Put ('[');
      Is_First := True;
      Disp_Iir_Chain_Elements (N);
      Put ("]");
   end Disp_Iir_Chain;

   procedure Disp_Iir_List (Id : String; L : Iir_List; Ref : Boolean)
   is
      El : Iir;
      It : List_Iterator;
      Is_First_Item : Boolean := True;
   begin
      if L = Null_Iir_List then
         return;
      end if;

      Put ('"');
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
               if El /= Null_Iir then
                  if Is_First_Item then
                     Is_First_Item := False;
                  else
                     Put (',');
                  end if;
                  if Ref then
                     Put (Iir'Image (El));
                  else
                     Disp_Iir (El, True);
                  end if;
               end if;
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
      if L = Null_Iir_Flist then
         return;
      end if;

      Put ('"');
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
               if El /= Null_Iir then
                  if Is_First_Item then
                     Is_First_Item := False;
                  else
                     Put (',');
                  end if;
                  Disp_Iir (El, True);
               end if;
            end loop;
            Put (']');
      end case;
   end Disp_Iir_Flist;

   procedure Disp_Iir (N : Iir; Requires_Brace : Boolean) is
   begin
      if N = Null_Iir then
         return;
      end if;

      if Requires_Brace then
         Put ("{""");
      else
         Put ('"');
      end if;
      Put (Get_Iir_Image (Get_Kind (N)));
      Put (""":{");
      Is_First := True;

      Put_Attribute ("id", Iir'Image (N));

      declare
         Loc : constant Location_Type := Get_Location (N);
         File : Name_Id;
         Line : Natural;
         Col : Natural;
      begin
         if Loc /= No_Location then
            Files_Map.Location_To_Position (Loc, File, Line, Col);
            Put_Attribute ("file", Name_Id'Image (File));
            Put_Attribute ("line", Natural'Image (Line));
            Put_Attribute ("col", Natural'Image (Col));
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
                  null;
               when Type_Iir_List =>
                  null;
               when Type_Iir_Flist =>
                  null;
               when Type_String8_Id =>
                  null;
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
                  Put_Field (F, Get_Iir_Index32 (N, F));
               when Type_Int64 =>
                  Put_Field (F, Get_Int64 (N, F));
               when Type_Boolean =>
                  Put_Field (F, Get_Boolean (N, F));
               when Type_Iir_Staticness =>
                  Put_Field (F, Image_Iir_Staticness
                               (Get_Iir_Staticness (N, F)));
               when Type_Scalar_Size =>
                  Put_Field (F, Image_Scalar_Size (Get_Scalar_Size (N, F)));
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
                  Put_Field (F, Get_Iir_Int32 (N, F));
               when Type_Int32 =>
                  Put_Field (F, Int64(Get_Int32 (N, F)));
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

         for I in Fields'Range loop
            F := Fields (I);
            case Get_Field_Type (F) is
               when Type_Iir =>
                  declare
                     V : constant Iir := Get_Iir (N, F);
                     Img : constant String := Get_Field_Image (F);
                  begin
                     if V /= Null_Iir then
                        case Get_Field_Attribute (F) is
                           when Attr_None =>
                              Put (",""");
                              Put (Img);
                              Put (""":{");
                              Is_First := True;
                              Disp_Iir (V, False);
                              Put ('}');
                           when Attr_Ref
                           | Attr_Forward_Ref
                           | Attr_Maybe_Forward_Ref =>
                              Put (',');
                              Disp_Iir_Ref (Img, V);
                           when Attr_Maybe_Ref =>
                              if Get_Is_Ref (N) then
                                 Put (',');
                                 Disp_Iir_Ref (Img, V);
                              else
                                 Put (",""");
                                 Put (Img);
                                 Put (""":{");
                                 Is_First := True;
                                 Disp_Iir (V, False);
                                 Put ('}');
                              end if;
                           when Attr_Chain =>
                              Put (',');
                              Disp_Iir_Chain (Img, V);
                           when Attr_Chain_Next =>
                              null;
                           when Attr_Of_Ref | Attr_Of_Maybe_Ref =>
                              raise Internal_Error;
                        end case;
                     end if;
                  end;
               when Type_Iir_List =>
                  declare
                     L : constant Iir_List := Get_Iir_List (N, F);
                     Img : constant String := Get_Field_Image (F);
                  begin
                     if L /= Null_Iir_List then
                        case Get_Field_Attribute (F) is
                           when Attr_None =>
                              Put (',');
                              Disp_Iir_List (Img, L, False);
                           when Attr_Of_Ref =>
                              Put (',');
                              Disp_Iir_List (Img, L, True);
                           when Attr_Of_Maybe_Ref =>
                              Put (',');
                              Disp_Iir_List (Img, L, Get_Is_Ref (N));
                           when Attr_Ref =>
                              null;
                           when others =>
                              raise Internal_Error;
                        end case;
                     end if;
                  end;
               when Type_Iir_Flist =>
                  declare
                     L : constant Iir_Flist := Get_Iir_Flist (N, F);
                     Img : constant String := Get_Field_Image (F);
                  begin
                     if L /= Null_Iir_Flist then
                        case Get_Field_Attribute (F) is
                           when Attr_None =>
                              Put (',');
                              Disp_Iir_Flist (Img, L);
                           when Attr_Of_Ref =>
                              null;
                           when Attr_Of_Maybe_Ref =>
                              if not Get_Is_Ref (N) then
                                 Put (',');
                                 Disp_Iir_Flist (Img, L);
                              end if;
                           when Attr_Ref =>
                              null;
                           when others =>
                              raise Internal_Error;
                        end case;
                     end if;
                  end;
               when Type_String8_Id =>
                  --  Special handling for strings
                  Put (",""");
                  Put (Get_Field_Image (F));
                  Put (""":""");
                  Put (To_JSON (Image_String8 (N)));
                  Put ('"');
               when others =>
                  null;
            end case;
         end loop;
      end;

      if Requires_Brace then
         Put ("}}");
      else
         Put ("}");
      end if;
   end Disp_Iir;

   --  Command --file-to-json
   type Command_File_To_Json is new Command_Lib with null record;

   function Decode_Command (Cmd : Command_File_To_Json; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_File_To_Json) return String;

   procedure Perform_Action (Cmd : in out Command_File_To_Json;
                             Files_Name : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_File_To_Json; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "file-to-json"
        or else Name = "--file-to-json";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_File_To_Json) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "file-to-json FILEs"
        & ASCII.LF & "  Dump AST in JSON"
        & ASCII.LF & "  alias: --file-to-json";
   end Get_Short_Help;

   procedure Perform_Action (Cmd : in out Command_File_To_Json;
                             Files_Name : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);

      use Files_Map;

      Id : Name_Id;
      File : Source_File_Entry;

      type File_Data is record
         Fe : Source_File_Entry;
         Design_File : Iir;
      end record;
      type File_Data_Array is array (Files_Name'Range) of File_Data;

      Files : File_Data_Array;
   begin
      Success := False;

      --  Load work library.
      if not Setup_Libraries (True) then
         return;
      end if;

      Flags.Flag_Elaborate_With_Outdated := True;

      --  Parse all files.
      for I in Files'Range loop
         Id := Get_Identifier (Files_Name (I).all);
         File := Read_Source_File (Libraries.Local_Directory, Id);
         if File = No_Source_File_Entry then
            Error ("cannot open " & Image (Id));
            return;
         end if;
         Files (I).Fe := File;
         Files (I).Design_File := Load_File (File);
         if Files (I).Design_File = Null_Iir then
            return;
         end if;
         --  Put units in library.
         --  Note: design_units stay while design_file get empty.
         Libraries.Add_Design_File_Into_Library (Files (I).Design_File);
      end loop;

      --  Analyze all files.
      for I in Files'Range loop
         Analyze_Design_File_Units (Files (I).Design_File);
      end loop;

      Put("[");
      Is_First := True;
      Disp_Iir_Chain_Elements (Libraries.Get_Libraries_Chain);
      Put("]");
      Ada.Strings.Unbounded.Text_IO.Put (Json);

      Success := True;
   exception
      when Compilation_Error =>
         Error ("json dump failed due to compilation error");
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_File_To_Json);
   end Register_Commands;
end Ghdljson;
