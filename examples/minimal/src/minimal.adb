with Interfaces.C;
with Interfaces.C.Strings;

package body Minimal is

  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;
  
  use all type gdnative.GDNATIVE_API_TYPES;

  Core_Api         : access constant godot_gdnative_core_api_struct;
  Nativescript_Api : access constant godot_gdnative_ext_nativescript_api_struct;

  type Godot_String_Kind is (
    GDNATIVE_INITIALIZE_MESSAGE,
    GDNATIVE_TERMINATE_MESSAGE,
    NATIVESCRIPT_INITIALIZE_MESSAGE);

  function To_Godot_String (Item : in Wide_String) return Godot_String is 
    C_Item     : IC.wchar_array := IC.To_C (Item);
    Godot_Item : aliased godot_string;
  begin
    Core_Api.godot_string_new_with_wide_string (Godot_Item'access, C_Item (0)'access, C_Item'Length);
    return Godot_Item;
  end;

  type Godot_String_Array is array (Godot_String_Kind) of aliased godot_string;
  Strings : Godot_String_Array;

  procedure Initialize (Strings : in out Godot_String_Array) is begin
    for Key in Godot_String_Kind'range loop
      Strings (Key) := To_Godot_String (
        (case Key is
          when GDNATIVE_INITIALIZE_MESSAGE     => "GDNative Intialized",
          when GDNATIVE_TERMINATE_MESSAGE      => "GDNative Terminated",
          when NATIVESCRIPT_INITIALIZE_MESSAGE => "Nativescript Intialized",
          when others                          => "Unknown String"));
    end loop;
  end;

  procedure Finalize (Strings : in out Godot_String_Array) is begin
    for Key in Godot_String_Kind'range loop
      Core_Api.godot_string_destroy (Strings (Key)'access);
    end loop;
  end;

  procedure Godot_Print (Item : in Godot_String_Kind) is begin
    Core_Api.godot_print (Strings (Item)'access);
  end;

  procedure GDNative_Intialize (p_options : access godot_gdnative_init_options) is 
    Cursor : GDnative_Api_Struct_Pointers.Pointer;
  begin
    Core_Api := p_options.api_struct;
    Cursor := Core_Api.extensions;
    for I in 1 .. Core_Api.num_extensions loop
      case Cursor.all.c_type is
        when GDNATIVE_EXT_NATIVESCRIPT =>
          Nativescript_Api := To_godot_gdnative_ext_nativescript_api_struct (Cursor);
        when others => null;
      end case;
      GDnative_Api_Struct_Pointers.Increment (Cursor);
    end loop;

    Initialize (Strings);
    Godot_Print (GDNATIVE_INITIALIZE_MESSAGE);
  end;

  procedure GDNative_Finalize (p_options : access godot_gdnative_terminate_options) is begin
    Godot_Print (GDNATIVE_TERMINATE_MESSAGE);
    Finalize (Strings);

    Core_Api         := null;
    Nativescript_Api := null;
  end;

  procedure Nativescript_Initialize (p_handle : System.Address) is begin
    Godot_Print (NATIVESCRIPT_INITIALIZE_MESSAGE);
  end;
  
end;
