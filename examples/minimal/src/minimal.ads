with System;
with GDNative; use GDNative;

package Minimal is

  procedure GDNative_Intialize (p_options : access godot_gdnative_init_options)
    with Export => True, Convention => C, External_Name => "minimal_gdnative_init";
  
  procedure GDNative_Finalize (p_options : access godot_gdnative_terminate_options)
    with Export => True, Convention => C, External_Name => "minimal_gdnative_terminate";

  procedure Nativescript_Initialize (p_handle : System.Address)
    with Export => True, Convention => C, External_Name => "minimal_nativescript_init";

end;