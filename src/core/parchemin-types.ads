with Ada.strings.Unbounded;
with Interfaces; use Interfaces;

package Parchemin.Types is 
    
    ----------------------------------------------------------------------------
    -- Types
    ----------------------------------------------------------------------------
    type Endian_Type is (LITTLE, BIG);
    type Memory_Addr is new Interfaces.Unsigned_32;
    type Memory_Size is new Interfaces.Unsigned_32;
    type Storage_type is (EEPROM, FLASH, FRAM, NVRAM);
    
    ----------------------------------------------------------------------------  
    -- Subtypes
    ----------------------------------------------------------------------------
    subtype Unbounded_String is Ada.Strings.Unbounded.Unbounded_String;

end Parchemin.Types;
