with Interfaces.C;
with Interfaces.C.Strings;

package body Entry is

  package IC renames Interfaces.C;
  
  use all type gdnative.GDNATIVE_API_TYPES;

  Core_Api         : access constant godot_gdnative_core_api_struct;
  Nativescript_Api : access constant godot_gdnative_ext_nativescript_api_struct;

  procedure Godot_Print (Item : in Wide_String) is 
    C_Item     : IC.wchar_array := IC.To_C (Item);
    Godot_Item : aliased godot_string;
  begin
    Core_Api.godot_string_new_with_wide_string (Godot_Item'access, C_Item (0)'access, C_Item'Length);
    Core_Api.godot_print (Godot_Item'access);
    Core_Api.godot_string_destroy (Godot_Item'access);
  end;

  procedure GDNative_Intialize (p_options : access godot_gdnative_init_options) is 
    Cursor : GDnative_Api_Struct_Pointers.Pointer;
  begin
    Core_Api := p_options.api_struct;
    Cursor := Core_Api.extensions;
    for I in 1 .. Core_Api.num_extensions loop
      case Cursor.all.c_type is
        when GDNATIVE_EXT_NATIVESCRIPT =>
          Nativescript_Api := To_godot_gdnative_ext_nativescript_api_struct (Cursor.all);
        when others => null;
      end case;
      GDnative_Api_Struct_Pointers.Increment (Cursor);
    end loop;

    Godot_Print ("GDNative Initialized");
  end;

  procedure GDNative_Finalize (p_options : access godot_gdnative_terminate_options) is begin
    Godot_Print ("GDNative Finalized");

    Core_Api         := null;
    Nativescript_Api := null;
  end;

  procedure Nativescript_Initialize (p_handle : System.Address) is begin
    Godot_Print ("NativeScript Initialized");
  end;
  
end;
