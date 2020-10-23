with System;
with Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;

package GDNative.Thick is

  procedure Initialize_GDNative     (p_options : access godot_gdnative_init_options);
  procedure Initialize_Nativescript (p_handle  : in Nativescript_Handle);
  procedure Finalize_GDNative       (p_options : access godot_gdnative_terminate_options);

  -- Console I/O
  procedure Put (Item : in Wide_String);

  -- Class Registration
  package Objects is

    -- This is jank forced by the Generic_Dispatching_Constructor
    type No_Parameters is null record;

    ------------
    -- Object --
    ------------
    type Object is abstract tagged null record;
    function Constructor (Parameters : not null access No_Parameters) return Object is abstract;

    ----------
    -- Node --
    ----------
    type Node is abstract new Object with null record;
    function Constructor (Parameters : not null access No_Parameters) return Node is abstract;
    procedure Process (Item : in Node; Delta_Time : in Long_Float) is null;

    type Process_Callback_Access is access procedure (Item : in Node'class; Delta_Time : in Long_Float);

    -- Registration Methods
    procedure Register_Class   (Object : Ada.Tags.Tag);
    procedure Register_Process (Node   : Ada.Tags.Tag; Callback : Process_Callback_Access);

  -------
  private
  -------

    function Construct is new Ada.Tags.Generic_Dispatching_Constructor (T => Object, Parameters => No_Parameters, Constructor => Constructor);
  end;

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
    Nativescript_Ptr        : Nativescript_Handle                            := Null_Handle;
    Pluginscript_Api        : godot_gdnative_ext_pluginscript_api_struct_ptr := null;
    Android_Api             : godot_gdnative_ext_android_api_struct_ptr      := null;
    Arvr_Api                : godot_gdnative_ext_arvr_api_struct_ptr         := null;
    Videodecoder_Api        : godot_gdnative_ext_videodecoder_api_struct_ptr := null;
    Net_Api                 : godot_gdnative_ext_net_api_struct_ptr          := null;
  end record;

  Context : GDNative_Context;

end;