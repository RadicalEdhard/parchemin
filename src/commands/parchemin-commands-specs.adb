with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.IO_Exceptions;
With Ada.Command_Line;

with Parchemin.MCUs_Specs;
with Parchemin.Commands;

package body Parchemin.Commands.Specs is
    
    package STR renames Ada.Strings;
    package TIO renames Ada.Text_IO;
    package STF renames Ada.Strings.Fixed;
    package IOE renames Ada.IO_Exceptions;
    package ACH renames Ada.Characters.Handling;
    Package ACL renames Ada.Command_Line;

    ----------------------------------------------------------------------------
    -- Procedure: List
    ----------------------------------------------------------------------------
    procedure List (File_Path : String; Print_Headers : Boolean := True) is
        File    : TIO.File_Type;
    begin
        TIO.open (File, TIO.In_File, File_Path);
    
        while not TIO.End_Of_File (File) loop
            declare
                Line: constant String := STF.trim (TIO.Get_Line (File), STR.Both);
            begin
                if Line'Length = 0 
                    or else Line in Parchemin.MCUs_Specs.Comment_Line then
                        null;
                elsif Print_Headers 
                    and then Line in Parchemin.MCUs_Specs.Header_Line then
                        TIO.Put_Line (Line);
                elsif Line in Parchemin.MCUs_Specs.Header_Line then
                    null;
                else
                    TIO.Put_Line (Line);
                end if;
            end;
        end loop;
     
        TIO.Close (File);

    exception
        when IOE.Name_Error =>
            raise IOE.Name_Error with File_Path & " not found.";
        when others => 
            if  TIO.IS_Open (File) then
                TIO.Close (File);
            end if;
            raise IOE.Use_Error with 
                "An error occurs while reading file : " & File_Path;
    
    end List;

    ----------------------------------------------------------------------------
    -- Procedure: Show
    --
    -- Not implemnted yet.
    ----------------------------------------------------------------------------
    procedure Show (MCU_Name : String) is
    begin
        TIO.Put_Line("Show " & MCU_Name);
    end Show;
    
    ----------------------------------------------------------------------------
    -- Overring procedure: Handle
    ----------------------------------------------------------------------------
    overriding
    procedure Handle
        (Self       : Specs_Commands;
         Path       : String;
         Callable   : String; 
         Start_Arg  : Positive) 
    is
        type Call is (List, Show, Unknown);

        function Parse_Call (S: String) return Call is
        begin
            return Call'Value (ACH.To_Upper (S));
        exception
            when others => return Unknown;
        end Parse_Call;

        Current_Call : constant Call := Parse_Call (Callable);

    begin
        case Current_Call is
            when List =>
                List (File_Path => Path);
            when Show =>

                Show (MCU_Name => Parchemin.Commands.Get_Arg (Start_Arg));
            when Unknown =>
                raise Parchemin.Commands.Program_Error with
                    "Invalide callable '" & Callable & "' for Specs module.";
                end case;
    end Handle;

end Parchemin.Commands.Specs;
