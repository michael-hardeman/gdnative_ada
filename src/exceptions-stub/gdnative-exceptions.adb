
package body GDNative.Exceptions is

  -- Here I should probably allow the library user to specify handlers for exceptions 
  -- (crash report dialog?)

  procedure Put_Warning (Message    : in Wide_String) is null;
  procedure Put_Error   (Occurrence : in Ada.Exceptions.Exception_Occurrence) is null;

end;