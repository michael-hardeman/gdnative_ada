with GDNative.Thin;

package GDNative.Context is

  procedure GDNative_Initialize     (p_options : access Thin.godot_gdnative_init_options);
  procedure GDNative_Finalize       (p_options : access Thin.godot_gdnative_terminate_options);

  procedure Nativescript_Initialize (p_handle  : in Thin.Nativescript_Handle);

  -- Use for function returns for void
  Nil_Godot_Variant        : aliased Thin.godot_variant;

  -- Set during GDNative_Initialize
  Core_Initialized         : Boolean                                             := False;
  Nativescript_Initialized : Boolean                                             := False;
  Core_Api                 : Thin.godot_gdnative_core_api_struct_ptr             := null;
  Nativescript_Api         : Thin.godot_gdnative_ext_nativescript_api_struct_ptr := null;
  Nativescript_Ptr         : Thin.Nativescript_Handle                            := Thin.Null_Handle;
  Pluginscript_Api         : Thin.godot_gdnative_ext_pluginscript_api_struct_ptr := null;
  Android_Api              : Thin.godot_gdnative_ext_android_api_struct_ptr      := null;
  Arvr_Api                 : Thin.godot_gdnative_ext_arvr_api_struct_ptr         := null;
  Videodecoder_Api         : Thin.godot_gdnative_ext_videodecoder_api_struct_ptr := null;
  Net_Api                  : Thin.godot_gdnative_ext_net_api_struct_ptr          := null;
end;