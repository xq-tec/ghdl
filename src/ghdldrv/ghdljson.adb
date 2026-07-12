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

with Types; use Types;
with Flags;
with Libraries;
with Errorout; use Errorout;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Ghdlmain; use Ghdlmain;
with Ghdllocal; use Ghdllocal;
with Ghdlcomp; use Ghdlcomp;
with Options; use Options;

with Ast_Export;
with Design_Export;

package body Ghdljson is

   procedure Prepare_Ast is
      Library, Design_File, Design_Unit : Iir;
   begin
      -- Load work library.
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
        & ASCII.LF & "  Dump analyzed AST in JSON"
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
      Ast_Export.Dump_Ast;

      Success := True;
   exception
      when Compilation_Error =>
         Error ("json dump failed due to compilation error");
   end Perform_Action;

   --  Command --design-to-json
   type Command_Design_To_Json is new Command_Lib with null record;

   function Decode_Command (Cmd : Command_Design_To_Json; Name : String)
                           return Boolean;
   function Get_Short_Help (Cmd : Command_Design_To_Json) return String;
   procedure Decode_Option (Cmd : in out Command_Design_To_Json;
                            Option : String;
                            Arg : String;
                            Res : out Option_State);
   procedure Disp_Long_Help (Cmd : Command_Design_To_Json);
   procedure Perform_Action (Cmd : in out Command_Design_To_Json;
                             Args : String_Acc_Array;
                             Success : out Boolean);

   function Decode_Command (Cmd : Command_Design_To_Json; Name : String)
                           return Boolean
   is
      pragma Unreferenced (Cmd);
   begin
      return Name = "design-to-json"
        or else Name = "--design-to-json";
   end Decode_Command;

   function Get_Short_Help (Cmd : Command_Design_To_Json) return String
   is
      pragma Unreferenced (Cmd);
   begin
      return "design-to-json [OPTS] UNIT [ARCH]"
        & ASCII.LF & "  Dump analyzed AST and elaborated design in JSONL"
        & ASCII.LF & "  alias: --design-to-json";
   end Get_Short_Help;

   procedure Decode_Option (Cmd : in out Command_Design_To_Json;
                            Option : String;
                            Arg : String;
                            Res : out Option_State)
   is
      pragma Unreferenced (Cmd);
      pragma Assert (Option'First = 1);
   begin
      if Option = "-o" then
         if Arg'Length = 0 then
            Res := Option_Arg_Req;
         else
            --  Silently accepted.
            Res := Option_Arg;
         end if;
      elsif Option'Length >= 4 and then Option (1 .. 4) = "-Wl," then
         Error_Msg_Option ("option -Wl is not available when ghdl "
                             & "is not configured with gcc or llvm");
         Res := Option_Err;
      else
         Decode_Comp_Option (Option, Arg, Res);
      end if;
   end Decode_Option;

   procedure Disp_Long_Help (Cmd : Command_Design_To_Json) is
      pragma Unreferenced (Cmd);
   begin
      Disp_Long_Help (Command_Lib (Cmd));
      Disp_Comp_Long_Help;
   end Disp_Long_Help;

   procedure Perform_Action (Cmd : in out Command_Design_To_Json;
                             Args : String_Acc_Array;
                             Success : out Boolean)
   is
      pragma Unreferenced (Cmd);
      Run_Arg : Natural;
   begin
      Success := False;

      if Hooks.Compile_Elab = null then
         Error ("design-to-json requires elaboration support");
         return;
      end if;

      Hooks.Compile_Init.all (False);

      Libraries.Load_Work_Library (False);
      Flags.Flag_Elaborate_With_Outdated := False;
      Flags.Flag_Only_Elab_Warnings := True;

      Hooks.Compile_Elab.all ("design-to-json", Args, Run_Arg);
      if Run_Arg <= Args'Last then
         Error_Msg_Option ("options after unit are ignored");
         return;
      end if;

      -- First dump the AST, then the corresponding design.
      Ast_Export.Dump_Ast;
      Design_Export.Dump_Design;

      Success := not Flag_Expect_Failure;
   exception
      when Compilation_Error =>
         Success := Flag_Expect_Failure and then Errorout.Nbr_Errors > 0;
   end Perform_Action;

   procedure Register_Commands is
   begin
      Register_Command (new Command_Ast_To_Json);
      Register_Command (new Command_Design_To_Json);
   end Register_Commands;
end Ghdljson;
