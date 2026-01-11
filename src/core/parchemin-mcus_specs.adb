with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;

package body Parchemin.MCUs_Specs is

    ---------------------------------------------------------------------------
    -- Function Clean_Hex
    ---------------------------------------------------------------------------
    function Clean_Hex (S : String) return String is
        begin 
            if S'Length > 2 and then S (S'First .. S'First + 1) = "0x" then
                return "16#" & S (S'First + 2 .. S'Last) & "#";
            else
                return S;
        end if;
    end Clean_Hex;

    ---------------------------------------------------------------------------
    -- Function Find_Spec
    ---------------------------------------------------------------------------
    function Find_Spec (File_Path: String; Target_Name : String) 
        return MCU_Spec is

        package TIO renames Ada.Text_Io;
        package STR renames Ada.strings;
        package STF renames Ada.Strings.Fixed;
        package MAP renames Ada.Strings.Maps;
        package SUS renames Ada.Strings.Unbounded;
        package IOE renames Ada.IO_Exceptions; 
        
        File        : TIO.File_Type;
 
        -- Whitepasces, horizontal tab, carriage returns and line feed  are
        -- considered as whitespace.
        Whitespace  : constant MAP.Character_Set :=
            MAP.To_Set(" " & ASCII.HT & ASCII.CR & ASCII.LF);
        Item        : MCU_Spec;
        Found       : Boolean := False;

        -- Cursor variables
        Current_First   : Positive;
        Current_Last    : Natural;
        Current_Cursor  : Positive;

        -- Pick extracts the next space-separated token and updates the
        -- parsing loop cursor. Implemented for readability.
        function Pick (From : String) return String is
        begin
            STF.Find_Token(
                Source  => From(Current_Cursor .. From'Last),
                Set     => Whitespace,
                Test    => STR.Outside,
                First   => Current_First,
                Last    => Current_Last);
            if Current_Last = 0 then 
                raise Invalid_Format with 
                    "Missing column for " & Target_Name & " in " & File_Path;
            end if;
            Current_Cursor := Current_Last +1;
            return From (Current_First .. Current_Last);
        end Pick;

    begin
        TIO.Open (File, TIO.In_File, File_Path);

        While not TIO.End_Of_FIle (File) and then not Found loop
            declare
                Line: constant String := STF.trim (TIO.Get_Line (File), STR.both);
            begin
                -- Skip comments, headers and empty lines.
                if Line not in Ignorable_Line then
                    Current_Cursor := Line'First;
                    declare 
                        Extracted_Name: constant String := Pick (Line);
                    begin
                        if Extracted_Name = Target_Name then
                            Found := True;
                            Item.Name       := SUS.To_Unbounded_String (Extracted_Name);
                            Item.Storage    := Parchemin.Types.Storage_Type'Value (Pick (Line));
                            Item.Endian     := Parchemin.Types.Endian_Type'Value (Pick (Line));
                            Item.Padding    := Interfaces.Unsigned_8'Value (Clean_Hex (Pick (Line)));
                            Item.Size       := Parchemin.types.Memory_Size'Value (Pick (Line));
                            Item.pg         := Parchemin.Types.Memory_Size'Value (Pick (Line));
                            Item.Begin_Addr := Parchemin.Types.Memory_Addr'Value (Clean_Hex (Pick (Line)));
                            Item.Addr_Mode  := Interfaces.Unsigned_8'Value (Pick (Line));
                        end if;
                    end;
                end if;
            end;
        end loop;

        TIO.Close (File);

        if not Found then
            raise MCU_Not_Found with 
            "Target " & Target_Name & " not found in " & File_Path;
        end if;

        Return Item;

    exception 
        when IOE.Name_Error => 
            raise IOE.Name_Error with 
                "Specifications file : " & File_Path & " not found.";
        when others =>
            if  TIO.Is_Open (File) then
                TIO.Close (File);
            end if;
            raise;

    end Find_Spec;

end Parchemin.MCUs_Specs;
