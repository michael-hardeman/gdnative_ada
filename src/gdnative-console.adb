with GDNative.Thin;
with GDNative.Strings;
with GDNative.Context;

package body GDNative.Console is
  
  ---------
  -- Put --
  ---------
  procedure Put (Item : in Wide_String) is 
    Godot_Item : Strings.GString;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Strings.Initialize (Godot_Item, Item);
    Context.Core_Api.godot_print (Strings.Ref (Godot_Item));
  end;

end;