with Interfaces;
with Ada.Strings.Unbounded;
with Parchemin.Types;

package Parchemin.MCUs_Specs is 

    -- Predicates
    -- Comment_Line is used to skip comments in the mcus database.
    subtype Comment_Line is String
        with Dynamic_Predicate =>
            (Comment_Line'Length >= 2 and then 
             Comment_Line(Comment_Line'First .. Comment_Line'First + 1) = "##");
    
    -- Header_Line is used to detect headers in the mcus database.
    subtype Header_Line is String
        with Dynamic_Predicate =>
            (Header_Line'Length >= 2 and then
             Header_Line(Header_Line'First .. Header_Line'First + 1) = "#%");
 
    -- Ignorable_Line is the combination of the preceding predicates
    subtype Ignorable_Line is String
        with Dynamic_Predicate =>   Ignorable_Line'Length = 0
                                    or Ignorable_Line in Comment_Line
                                    or Ignorable_Line in Header_Line;

    ----------------------------------------------------------------------------
    -- Record MCU_Spec
    -- 
    -- This record defines the mandatory hardware constraints require to enSure
    -- the generated data structure is physically compatible with the target MCU.
    --
    --  @component Name         : MCU commercial name.
    --  @component Storage      : Storage type (e.g. EEPROM, NVRAM, FRAM ...).
    --  @component Endian       : Endianess (e.g. LITTLE or BIG).
    --  @component Padding      : How to fill gap ? (e.g. with 0x00 or 0xFF).
    --  @component Size         : Total size of the storage in bytes.
    --  @component Pg           : Pagination size in bytes.
    --  @component Begin_Addr   : Wherre the storage begin, usefull on emulated storage.
    --  @component Addr_Mode    : Is in the name (e.g. 16 or 32 bits).
    ----------------------------------------------------------------------------
    type MCU_Spec is record
        Name        : Parchemin.Types.Unbounded_String;    
        Storage     : Parchemin.Types.Storage_Type;                             
        Endian      : Parchemin.Types.Endian_Type;                              
        Padding     : Interfaces.Unsigned_8;                    
        Size        : Parchemin.Types.Memory_Size;                              
        Pg          : Parchemin.Types.Memory_Size;                              
        Begin_Addr  : Parchemin.Types.Memory_Addr;                              
        Addr_Mode   : Interfaces.Unsigned_8;
    end record;
 
    ----------------------------------------------------------------------------
    -- Function Clean_Hex
    --
    -- Normalize hexadecimals literals by converting the standard 0x prefix 
    -- into Ada's native 16#...# syntax. Enabling the use of the 'Value attr
    -- for parsing.
    --
    --  @param S : The string to normalize;
    --
    --  @return The normalized string into Ada's native syntaxe 
    --      for hexadecimal repr. 
    ----------------------------------------------------------------------------
    function Clean_Hex (S : String) return String;

    ----------------------------------------------------------------------------
    -- Function Find_Spec
    --
    -- Perform linear search for target within MCUs writing specifications file. 
    --
    -- @param File_Path    : The absolute or relative path to the 'devices' database.
    -- @param Target_Name  : The Unique Identifier of the microcontroller (e.g., "ATtiny13").
    --
    -- @return A populated MCU_Spec record containing the writing parameters.
    -- 
    -- @raise Name_Error   : Raised if Target_Name is not found in database.
    -- @raise Data_Error   : Raised if record is found but contains malformed data.
    ----------------------------------------------------------------------------
    function Find_Spec (File_Path: String; Target_Name: String )
        return MCU_Spec;

    -- Exceptions
    MCU_Not_Found   : Exception;
    Invalid_Format  : Exception;

end Parchemin.Mcus_Specs;
