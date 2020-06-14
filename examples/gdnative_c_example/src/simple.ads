with System;
with Interfaces.C;
with GDNative; use GDNative;

package Simple is

  procedure godot_gdnative_init (p_options : access godot_gdnative_init_options)
    with Export => True, Convention => C, External_Name => "godot_gdnative_init";
  
  procedure godot_gdnative_terminate (p_options : access godot_gdnative_terminate_options)
    with Export => True, Convention => C, External_Name => "godot_gdnative_terminate";

  procedure godot_nativescript_init (p_handle : System.Address)
    with Export => True, Convention => C, External_Name => "godot_nativescript_init";

-------
private
-------

  package Object is
  
    function simple_constructor (
      p_instance    : System.Address;
      p_method_data : System.Address) 
      return System.Address;
    pragma Convention(C, simple_constructor);

    procedure simple_destructor (
      p_instance    : System.Address;
      p_method_data : System.Address;
      p_user_data   : System.Address);
    pragma Convention(C, simple_destructor);
    
    function simple_get_data (
      p_instance    : System.Address;
      p_method_data : System.Address;
      p_user_data   : System.Address;
      p_num_args    : Interfaces.C.int;
      p_args        : System.Address) -- godot_variant **
      return godot_variant;
    pragma Convention(C, simple_get_data);

  end;

end;