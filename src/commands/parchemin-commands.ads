-- File: parchemin-commands.ads
-- Description: Root definitions for the command-line interface.

package Parchemin.Commands is

    ----------------------------------------------------------------------------
    -- Type: Commande_Interface
    --
    -- Abstract root interface for all submodules. Provides the contract for
    -- subcomand dispatching.
    ----------------------------------------------------------------------------
    type Commands_Interface is interface;

    ----------------------------------------------------------------------------
    -- Procedure: Handle
    --
    -- Main dispatcher for commands within a module. 
    --
    -- @param self      The specific command object instance.
    -- @param Action    The command verb(e.g, "show", "list", "...").
    -- @param Start_Arg The index in Ada.Command_Line where parameters start.
    --
    -- @raise Program_Error if the requested action is not supported.
    ----------------------------------------------------------------------------
    procedure Handle (
        Self        : Commands_Interface;
        Path        : String;
        Action      : String;
        Start_Arg   : Positive) is abstract;

    -------------------------------------------------------------------------
    --  Get_Arg
    --
    --  Retrieves a command-line argument at a specific position.
    --
    --  @param Index The 1-based position of the argument to retrieve.
    --  @return The string content of the argument at the given index.
    --  @exception Program_Error Raised if Index exceeds the number of 
    --    available arguments.
    -------------------------------------------------------------------------
    function Get_Arg (Index : Positive) return String;
    
    -- Exceptions
    Program_Error : exception;

end Parchemin.Commands;
