with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Text_IO; use Ada.Text_IO;

package body Simple is

  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;
  
  use all type gdnative.GDNATIVE_API_TYPES;

  Core_Api         : access constant godot_gdnative_core_api_struct;
  Nativescript_Api : access constant godot_gdnative_ext_nativescript_api_struct;

  procedure Ignore (I : godot_bool) is begin null; end;

  procedure godot_gdnative_init (p_options : access godot_gdnative_init_options) is 
    Cursor : GDnative_Api_Struct_Pointers.Pointer;
  begin
    Put_Line ("godot_gdnative_init");
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
    Put_Line ("godot_gdnative_inited");
  end;

  procedure godot_gdnative_terminate (p_options : access godot_gdnative_terminate_options) is begin
    Put_Line ("godot_gdnative_terminate");
    Core_Api         := null;
    Nativescript_Api := null;
    Put_Line ("godot_gdnative_terminated");
  end;

  procedure godot_nativescript_init (p_handle : System.Address) is 
    create     : godot_instance_create_func  := (Object.simple_constructor'access, System.Null_Address, NULL);
    destroy    : godot_instance_destroy_func := (Object.simple_destructor'access, System.Null_Address, NULL);
    get_data   : godot_instance_method       := (Object.simple_get_data'access, System.Null_Address, NULL );
    attributes : godot_method_attributes     := (rpc_type => GODOT_METHOD_RPC_MODE_DISABLED);

    Simple_Str     : aliased IC.char_array := IC.To_C ("Simple");
    Simple_Ptr     : ICS.chars_ptr := ICS.To_Chars_Ptr (Simple_Str'Unchecked_Access);
    Reference_Str  : aliased IC.char_array := IC.To_C ("Reference");
    Reference_Ptr  : ICS.chars_ptr := ICS.To_Chars_Ptr (Reference_Str'Unchecked_Access);
    Get_Data_Str   : aliased IC.char_array := IC.To_C ("get_data");
    Get_Data_Ptr   : ICS.chars_ptr := ICS.To_Chars_Ptr (Get_Data_Str'Unchecked_Access);
  begin
    Put_Line ("godot_nativescript_init");
    Nativescript_Api.godot_nativescript_register_class (p_handle, Simple_Ptr, Reference_Ptr, create, destroy);
    Put_Line ("A");
    Nativescript_Api.godot_nativescript_register_method (p_handle, Simple_Ptr, Get_Data_Ptr, attributes, get_data);
    Put_Line ("godot_nativescript_inited");
  end;


  package body Object is
  
    type user_data_struct is record
      data : aliased IC.char_array (1 .. 256) := (others => IC.nul);
    end record;

    function simple_constructor (
      p_instance    : System.Address;
      p_method_data : System.Address)
      return System.Address
    is 
      c_user_data : constant System.Address := Core_Api.godot_alloc (user_data_struct'Size);
      user_data   : user_data_struct;
      for user_data'address use c_user_data;
      pragma Import (C, user_data);
      message  : IC.char_array := IC.To_C ("World from GDNative!");
      data_ptr : ICS.chars_ptr := ICS.To_Chars_Ptr (user_data.data'Unchecked_Access);
    begin
      ICS.Update (data_ptr, 0, message);
      return c_user_data;
    end;

    procedure simple_destructor (
      p_instance    : System.Address;
      p_method_data : System.Address;
      p_user_data   : System.Address)
    is begin
      Core_Api.godot_free (p_user_data);
    end;
    
    function simple_get_data (
      p_instance    : System.Address;
      p_method_data : System.Address;
      p_user_data   : System.Address;
      p_num_args    : IC.int; 
      p_args        : System.Address) -- godot_variant **
      return godot_variant
    is
      user_data : constant user_data_struct;
      for user_data'address use p_user_data;
      pragma Import (C, user_data);
      data : aliased godot_string;
      ret  : aliased godot_variant;
      data_ptr : constant ICS.chars_ptr := ICS.To_Chars_Ptr (user_data.data'Unchecked_Access);
    begin
      Core_Api.godot_string_new(data'access);
      Ignore (Core_Api.godot_string_parse_utf8 (data'access, data_ptr));
      Core_Api.godot_variant_new_string (ret'access, data'access);
      Core_Api.godot_string_destroy(data'access);
      return ret;
    end;

  end;
  
end;
