with GDNative; use GDNative;

package Engine_Hooks is

  procedure On_GDNative_Init (p_options : access godot_gdnative_init_options)
    with Export => True, Convention => C, External_Name => "adventure_gdnative_init";
  
  procedure On_GDNative_Terminate (p_options : access godot_gdnative_terminate_options)
    with Export => True, Convention => C, External_Name => "adventure_gdnative_terminate";

  procedure On_Nativescript_Init (p_handle : System.Address)
    with Export => True, Convention => C, External_Name => "adventure_nativescript_init";

end;