with GDNative.Thin;
with GDNative.Strings;
with GDNative.Context;

package body GDNative.Console is
  
  ---------
  -- Put --
  ---------
  procedure Put (Item : in Wide_String) is 
    Godot_Item : aliased Thin.godot_string := Strings.To_Godot (Item);
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_print (Godot_Item'access);
    Context.Core_Api.godot_string_destroy (Godot_Item'access);
  end;

end;