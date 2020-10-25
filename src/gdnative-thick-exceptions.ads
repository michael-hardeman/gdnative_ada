
with Ada.Exceptions;

package GDNative.Thick.Exceptions is
  
  procedure Put_Warning (Message    : in Wide_String);
  procedure Put_Error   (Occurrence : in Ada.Exceptions.Exception_Occurrence);

  -------------------------
  -- Last Chance Handler --
  -------------------------
  -- Should hopefully capture all errors
  procedure Last_Chance_Handler (Occurrence : Ada.Exceptions.Exception_Occurrence) 
    with No_Return, Unreferenced, Export, Convention => C, External_Name => "__gnat_last_chance_handler";

end;