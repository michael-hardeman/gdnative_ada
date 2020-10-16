with System;

package GDNative.Thick is

  type GDNative_Context is private;

  procedure Initialize              (Context : in out GDNative_Context; p_options : access godot_gdnative_init_options);
  procedure Initialize_Nativescript (Context : in out GDNative_Context; p_handle  : in System.Address);
  procedure Finalize                (Context : in out GDNative_Context);

  -- Console I/O
  procedure Put (Context : in GDNative_Context; Item : in Wide_String);

  Multiple_Initalizations : exception;
  Context_Uninitialized   : exception;

-------
private
-------

  type GDNative_Context is record
    Core_Initialized        : Boolean                                        := False;
    Core_Api                : godot_gdnative_core_api_struct_ptr             := null;
    Nativescript_Initalized : Boolean                                        := False;
    Nativescript_Api        : godot_gdnative_ext_nativescript_api_struct_ptr := null;
    Nativescript_Handle     : System.Address                                 := System.Null_Address;
    Pluginscript_Api        : godot_gdnative_ext_pluginscript_api_struct_ptr := null;
    Android_Api             : godot_gdnative_ext_android_api_struct_ptr      := null;
    Arvr_Api                : godot_gdnative_ext_arvr_api_struct_ptr         := null;
    Videodecoder_Api        : godot_gdnative_ext_videodecoder_api_struct_ptr := null;
    Net_Api                 : godot_gdnative_ext_net_api_struct_ptr          := null;
  end record;

end;