with GDNative; use GDNative;
with GDNative.Thin;

package Engine_Hooks is

  procedure On_GDNative_Init (p_options : access Thin.godot_gdnative_init_options)
    with Export => True, Convention => C, External_Name => "action_rpg_gdnative_init";
  
  procedure On_Nativescript_Init (p_handle : Thin.Nativescript_Handle)
    with Export => True, Convention => C, External_Name => "action_rpg_nativescript_init";

  procedure On_GDNative_Terminate (p_options : access Thin.godot_gdnative_terminate_options)
    with Export => True, Convention => C, External_Name => "action_rpg_gdnative_terminate";
end;