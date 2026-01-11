with Ada.Command_Line;

package body Parchemin.Commands is

   package ACL renames Ada.Command_Line;

   -------------------------------------------------------------------------
   -- Get_Arg
   -------------------------------------------------------------------------
   function Get_Arg (Index : Positive) return String is
   begin
      if Index <= ACL.Argument_Count then
         return ACL.Argument (Index);
      else
         raise Program_Error with 
           "Missing required argument at position" & Index'Img;
      end if;
   end Get_Arg;

end Parchemin.Commands;
