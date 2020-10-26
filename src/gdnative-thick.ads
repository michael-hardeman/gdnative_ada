with Ada.Exceptions;

package GDNative.Thick is

  procedure GDNative_Initialize     (p_options : access godot_gdnative_init_options);
  procedure GDNative_Finalize       (p_options : access godot_gdnative_terminate_options);

  procedure Nativescript_Initialize (p_handle  : in Nativescript_Handle);

  procedure Assert_Core_Initalized;
  procedure Assert_Nativescript_Initalized;

  function To_Str  (S : Wide_String) return String;
  function To_Wide (S : String)      return Wide_String;

  -- Use for function returns for void
  Nil_Godot_Variant : aliased godot_variant;

-------
private
-------

  Core_Initialized         : Boolean                                        := False;
  Core_Api                 : godot_gdnative_core_api_struct_ptr             := null;
  Nativescript_Initialized : Boolean                                        := False;
  Nativescript_Api         : godot_gdnative_ext_nativescript_api_struct_ptr := null;
  Nativescript_Ptr         : Nativescript_Handle                            := Null_Handle;
  Pluginscript_Api         : godot_gdnative_ext_pluginscript_api_struct_ptr := null;
  Android_Api              : godot_gdnative_ext_android_api_struct_ptr      := null;
  Arvr_Api                 : godot_gdnative_ext_arvr_api_struct_ptr         := null;
  Videodecoder_Api         : godot_gdnative_ext_videodecoder_api_struct_ptr := null;
  Net_Api                  : godot_gdnative_ext_net_api_struct_ptr          := null;

end;