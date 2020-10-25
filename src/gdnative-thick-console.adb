package body GDNative.Thick.Console is

  ---------
  -- Put --
  ---------
  procedure Put (Item : in Wide_String) is 
    C_Item     : IC.wchar_array := IC.To_C (Item);
    Godot_Item : aliased godot_string;
  begin
    Assert_Core_Initalized;

    Core_Api.godot_string_new_with_wide_string (Godot_Item'access, C_Item (0)'access, C_Item'Length);
    Core_Api.godot_print (Godot_Item'access);
    Core_Api.godot_string_destroy (Godot_Item'access);
  end;

end;