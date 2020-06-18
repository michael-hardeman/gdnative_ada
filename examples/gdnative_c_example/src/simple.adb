with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Exceptions;

package body Simple is

  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;
  
  use all type gdnative.GDNATIVE_API_TYPES;

  Core_Api         : access constant godot_gdnative_core_api_struct;
  Nativescript_Api : access constant godot_gdnative_ext_nativescript_api_struct;

  procedure godot_gdnative_init (p_options : access godot_gdnative_init_options) is 
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
  exception
    when Error: others =>
      declare
        C_Error_Info : ICS.chars_ptr := ICS.New_String (Ada.Exceptions.Exception_Information (Error));
      begin
        p_options.report_loading_error (p_options.gd_native_library, C_Error_Info);
        ICS.Free (C_Error_Info);
      end;
  end;

  procedure godot_gdnative_terminate (p_options : access godot_gdnative_terminate_options) is begin
    Core_Api         := null;
    Nativescript_Api := null;
  end;

  procedure godot_nativescript_init (p_handle : System.Address) is 
    create     : godot_instance_create_func  := (Object.simple_constructor'access, System.Null_Address, null);
    destroy    : godot_instance_destroy_func := (Object.simple_destructor'access, System.Null_Address, null);
    get_data   : godot_instance_method       := (Object.simple_get_data'access, System.Null_Address, null);
    attributes : godot_method_attributes     := (rpc_type => GODOT_METHOD_RPC_MODE_DISABLED);

    Simple_Ptr    : ICS.chars_ptr := ICS.New_String ("Simple");
    Reference_Ptr : ICS.chars_ptr := ICS.New_String ("Reference");
    Get_Data_Ptr  : ICS.chars_ptr := ICS.New_String ("get_data");
  begin
    Nativescript_Api.godot_nativescript_register_class (p_handle, Simple_Ptr, Reference_Ptr, create, destroy);
    Nativescript_Api.godot_nativescript_register_method (p_handle, Simple_Ptr, Get_Data_Ptr, attributes, get_data);
    ICS.Free (Simple_Ptr); ICS.Free (Reference_Ptr); ICS.Free (Get_Data_Ptr);
  end;

  package body Object is
  
    type user_data_struct is record
      data : aliased godot_string;
    end record;

    function simple_constructor (
      p_instance    : System.Address;
      p_method_data : System.Address)
      return System.Address
    is
      p_user_data : System.Address := Core_Api.godot_alloc (user_data_struct'Size);
      user_data : user_data_struct;
      for user_data'address use p_user_data;
      pragma Import (C, user_data);
      data : IC.wchar_array := IC.To_C ("World from GDNative!");
    begin
      Core_Api.godot_string_new(user_data.data'access);
      Core_Api.godot_string_new_with_wide_string (user_data.data'access, data (data'First)'access, data'length);
      return p_user_data;
    end;

    procedure simple_destructor (
      p_instance    : System.Address;
      p_method_data : System.Address;
      p_user_data   : System.Address)
    is 
      user_data : user_data_struct;
      for user_data'address use p_user_data;
      pragma Import (C, user_data);
    begin
      Core_Api.godot_string_destroy(user_data.data'access);
      Core_Api.godot_free (p_user_data);
    end;

    function simple_get_data (
      p_instance    : System.Address;
      p_method_data : System.Address;
      p_user_data   : System.Address;
      p_num_args    : IC.int;
      p_args        : System.Address)
      return godot_variant
    is
      user_data : user_data_struct;
      for user_data'address use p_user_data;
      pragma Import (C, user_data);
      variant : aliased godot_variant;
    begin
      Core_Api.godot_variant_new_string (variant'access, user_data.data'access);
      return variant;
    end;

  end;
  
end;
