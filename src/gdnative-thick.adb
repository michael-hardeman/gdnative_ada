
with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Exceptions;

package body GDNative.Thick is

  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;

  ---------------
  -- Initalize --
  ---------------
  procedure Initialize (Context : in out GDNative_Context; p_options : access godot_gdnative_init_options) is 
    Cursor : GDnative_Api_Struct_Pointers.Pointer;
  begin
    if Context.Core_Initialized then
      raise Multiple_Initalizations with "GDNative Context already Initialized!";
    end if;

    Context.Core_Api := p_options.api_struct;
    Context.Core_Initialized := True;

    Cursor := Core_Api.extensions;
    for I in 1 .. Core_Api.num_extensions loop
      case Cursor.all.c_type is
        when GDNATIVE_EXT_NATIVESCRIPT => Context.Nativescript_Api := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_PLUGINSCRIPT => Context.Pluginscript_Api := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_ANDROID      => Context.Android_Api      := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_ARVR         => Context.Arvr_Api         := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_VIDEODECODER => Context.Videodecoder_Api := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_NET          => Context.Net_Api          := To_Api_Struct_Ptr (Cursor.all);
        when others => null;
      end case;
      GDnative_Api_Struct_Pointers.Increment (Cursor);
    end loop;

  exception
    when Error: others =>
      declare
        C_Error_Info : ICS.chars_ptr := ICS.New_String (Ada.Exceptions.Exception_Information (Error));
      begin
        p_options.report_loading_error (p_options.gd_native_library, C_Error_Info);
        ICS.Free (C_Error_Info);
      end;
  end;

  --------------
  -- Finalize --
  --------------
  procedure Finalize (Context : in out GDNative_Context) is begin
    Context.Core_Initialized         := False;
    Context.Core_Api                 := null;
    Context.Nativescript_Initialized := False;
    Context.Nativescript_Api         := null;
    Context.Nativescript_Handle      := System.Null_Address;
    Context.Pluginscript_Api         := null;
    Context.Android_Api              := null;
    Context.Arvr_Api                 := null;
    Context.Videodecoder_Api         := null;
    Context.Net_Api                  := null;
  end;

  ----------------------------
  -- Assert Core Initalized --
  ----------------------------
  procedure Assert_Core_Initalized (Context : in GDNative_Context) is begin
    if Context.Core_Initialized then return; end if;
    raise Context_Uninitialized with "GDNative Context not Initialized!";
  end;

  ---------
  -- Put --
  ---------
  procedure Put (Context : in GDNative_Context; Item : in Wide_String) is 
    C_Item     : IC.wchar_array := IC.To_C (Item);
    Godot_Item : aliased godot_string;
  begin
    Assert_Core_Initalized (Context);

    Context.Core_Api.godot_string_new_with_wide_string (Godot_Item'access, C_Item (0)'access, C_Item'Length);
    Context.Core_Api.godot_print (Godot_Item'access);
    Context.Core_Api.godot_string_destroy (Godot_Item'access);
  end;

  -----------------------------
  -- Initialize_Nativescript --
  -----------------------------
  procedure Initialize_Nativescript (Context : in out GDNative_Context; p_handle : in System.Address) is begin
    if Context.Core_Initialized then
      raise Multiple_Initalizations with "Nativescript already Initialized!";
    end if;

    Context.Nativescript_Handle      := p_handle;
    Context.Nativescript_Initialized := True;
  exception
    when Error: others => Put (Context, Ada.Exceptions.Exception_Information (Error));
  end;

end;