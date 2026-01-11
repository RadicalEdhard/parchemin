with Ada.Text_IO;
with Ada.Command_Line;

with Parchemin.Config;
with Parchemin.Commands.Specs;

procedure Parchemin.Cli is

    Data_File : constant String := Parchemin.Config.Data_Path & "/mcus";

    package TIO renames Ada.Text_IO;
    package ACL renames Ada.Command_Line;

    Specs_Cmd : Parchemin.Commands.Specs.Specs_Commands;

begin
    if ACL.Argument_Count < 1 then
        TIO.Put_Line ("Usage: parchemin <module> <command>");
        return;
    end if;

    if ACL.Argument (1) = "specs" then
        if ACL.Argument_Count < 2 then
            TIO.Put_Line ("Error: 'specs' requires a command (e.g., list)");
        else
            Specs_Cmd.Handle (Path      => Data_File,
                              Callable  => ACL.Argument (2),
                              Start_Arg => 3);
        end if;
    else
        TIO.Put_Line ("Unknown module: " & ACL.Argument (1));
    end if;

end Parchemin.Cli;
