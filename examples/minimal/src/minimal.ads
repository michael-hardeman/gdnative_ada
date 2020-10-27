with System;
with GDNative.Thin; use GDNative.Thin;

package Minimal is

  procedure GDNative_Intialize (p_options : access godot_gdnative_init_options)
    with Export => True, Convention => C, External_Name => "minimal_gdnative_init";
  
  procedure GDNative_Finalize (p_options : access godot_gdnative_terminate_options)
    with Export => True, Convention => C, External_Name => "minimal_gdnative_terminate";

  procedure Nativescript_Initialize (p_handle : Nativescript_Handle)
    with Export => True, Convention => C, External_Name => "minimal_nativescript_init";

end;