with Interfaces.C;

with GDNative.Thin;
with GDNative.Context;

package body GDNative.Console is
  
  package IC renames Interfaces.C;

  ---------
  -- Put --
  ---------
  procedure Put (Item : in Wide_String) is 
    C_Item     : IC.wchar_array := IC.To_C (Item);
    Godot_Item : aliased Thin.godot_string;
  begin
    pragma Assert (Context.Core_Initialized, "Please run GDNative_Initialize");

    Context.Core_Api.godot_string_new_with_wide_string (Godot_Item'access, C_Item (0)'access, C_Item'Length);
    Context.Core_Api.godot_print (Godot_Item'access);
    Context.Core_Api.godot_string_destroy (Godot_Item'access);
  end;

end;