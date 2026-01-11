package Parchemin.Utils is

    ---------------------------------------------------------------------------
    -- To_hex
    --
    -- Converts a numeric value into its hexadecimal string representation.
    --
    -- @param Value     : The integer to convert.
    -- @param Width     : the minimum number of digits for the output.
    -- @param prefix    : If True, preprends "0x" to the result (e.g., 0x0000).
    --
    -- @return  A string containing the hexadecimal representation in uppercase.
    ---------------------------------------------------------------------------
    function To_hex (   
        Value   : Long_Integer;
        Width   : Natural := 0;
        Prefix  : Boolean := False) Return String;

end Parchemin.Utils;
