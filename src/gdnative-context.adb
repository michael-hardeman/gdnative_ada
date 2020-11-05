
with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Exceptions;

with GDNative.Input_Map;
with GDNative.Input;

package body GDNative.Context is

  package IC  renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;
  package AE  renames Ada.Exceptions;

  ------------------------
  -- GDNative Initalize --
  ------------------------
  procedure GDNative_Initialize (p_options : access Thin.godot_gdnative_init_options) is
    Cursor : Thin.GDnative_Api_Struct_Pointers.Pointer;
  begin
    pragma Assert (not Core_Initialized, CORE_MULTIPLE_INITIALIZATION_ASSERT);

    Core_Api := p_options.api_struct;
    Core_Initialized := True;

    Core_Api.godot_variant_new_nil (Nil_Godot_Variant'access);

    Cursor := Core_Api.extensions;
    for I in 1 .. Core_Api.num_extensions loop
      case Cursor.all.c_type is
        when Thin.GDNATIVE_EXT_NATIVESCRIPT => Nativescript_Api := Thin.To_Api_Struct_Ptr (Cursor.all);
        when Thin.GDNATIVE_EXT_PLUGINSCRIPT => Pluginscript_Api := Thin.To_Api_Struct_Ptr (Cursor.all);
        when Thin.GDNATIVE_EXT_ANDROID      => Android_Api      := Thin.To_Api_Struct_Ptr (Cursor.all);
        when Thin.GDNATIVE_EXT_ARVR         => Arvr_Api         := Thin.To_Api_Struct_Ptr (Cursor.all);
        when Thin.GDNATIVE_EXT_VIDEODECODER => Videodecoder_Api := Thin.To_Api_Struct_Ptr (Cursor.all);
        when Thin.GDNATIVE_EXT_NET          => Net_Api          := Thin.To_Api_Struct_Ptr (Cursor.all);
        when others => null;
      end case;
      Thin.GDnative_Api_Struct_Pointers.Increment (Cursor);
    end loop;

    -- Singletons
    Input_Map.Initialize;
    Input.Initialize;

  exception
    when Error : others =>
      declare
        C_Error_Info : ICS.chars_ptr := ICS.New_String (AE.Exception_Information (Error));
      begin
        p_options.report_loading_error (p_options.gd_native_library, C_Error_Info);
        ICS.Free (C_Error_Info);
      end;
  end;

  -----------------------
  -- GDNative Finalize --
  -----------------------
  procedure GDNative_Finalize (p_options : access Thin.godot_gdnative_terminate_options) is begin
    pragma Assert (Core_Initialized,         CORE_EARLY_FINALIZE_ASSERT);
    pragma Assert (Nativescript_Initialized, NATIVESCRIPT_EARLY_FINALIZE_ASSERT);

    -- Singletons
    Input.Finalize;
    Input_Map.Finalize;

    Core_Initialized         := False;
    Nativescript_Initialized := False;
    Core_Api                 := null;
    Nativescript_Api         := null;
    Nativescript_Ptr         := Thin.NULL_NATIVESCRIPT_HANDLE;
    Pluginscript_Api         := null;
    Android_Api              := null;
    Arvr_Api                 := null;
    Videodecoder_Api         := null;
    Net_Api                  := null;
  end;

  -----------------------------
  -- Nativescript Initialize --
  -----------------------------
  procedure Nativescript_Initialize (p_handle : in Thin.Nativescript_Handle) is begin
    pragma Assert (Core_Initialized,             CORE_UNINITIALIZED_ASSERT);
    pragma Assert (not Nativescript_Initialized, NATIVESCRIPT_MULTIPLE_INITIALIZATION_ASSERT);

    Nativescript_Ptr         := p_handle;
    Nativescript_Initialized := True;
  end;

end;