with Ada.Text_IO;
with Parchemin.Types;

package Parchemin.Commands.Specs is
    
    type Specs_Commands is new Parchemin.Commands.Commands_Interface with record
        Base_Path : Parchemin.Types.Unbounded_String;
    end record; 
    ----------------------------------------------------------------------------
    -- Procedure: Handle (Overriding)
    --
    -- Main dispatcher for the 'specs' submodule commands.
    --
    -- @param Self      The Specs_Commands instance. 
    -- @param Callable  The command (e.g., "show", "list").
    -- @param Start_Arg Index where subcommand's parameters begin.
    --
    -- @raise Name_Error if the file path provided in arguments is invalid.
    ----------------------------------------------------------------------------
    overriding
    procedure Handle 
        (Self       : Specs_Commands;
         Path       : String;
         Callable   : String;
         Start_Arg  : Positive);
    
    ---------------------------------------------------------------------------
    -- Procedure: List
    --
    -- Print the specification database
    --
    -- @param File_Path     The path of microcontrollers database.
    ---------------------------------------------------------------------------
    procedure List (File_Path: String; Print_Headers : Boolean := True);

    ---------------------------------------------------------------------------
    -- Procedure: Show 
    --
    -- Show the expected specification details.
    --
    -- @param MCU_Name  The microcontroller name.
    -- ------------------------------------------------------------------------
    procedure Show (MCU_Name : String);

end Parchemin.Commands.specs;
