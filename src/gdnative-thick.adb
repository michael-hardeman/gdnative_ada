
with System;
with System.Address_To_Access_Conversions;
with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Tags;
with Ada.Exceptions;

package body GDNative.Thick is

  package S renames System;
  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;

  ----------------------------
  -- Assert Core Initalized --
  ----------------------------
  procedure Assert_Core_Initalized is begin
    if Core_Initialized then return; end if;
    raise Context_Uninitialized with "GDNative Context not Initialized!";
  end;

  ------------------------------------
  -- Assert Nativescript Initalized --
  ------------------------------------
  procedure Assert_Nativescript_Initalized is begin
    if Core_Initialized and then Nativescript_Initialized then return; end if;
    raise Context_Uninitialized with "GDNative Context Nativescript not Initialized!";
  end;

  ------------------------
  -- Initalize GDNative --
  ------------------------
  procedure Initialize_GDNative (p_options : access godot_gdnative_init_options) is 
    Cursor : GDnative_Api_Struct_Pointers.Pointer;
  begin
    if Core_Initialized then
      raise Multiple_Initalizations with "GDNative Context already Initialized!";
    end if;

    Core_Api := p_options.api_struct;
    Core_Initialized := True;

    Cursor := Core_Api.extensions;
    for I in 1 .. Core_Api.num_extensions loop
      case Cursor.all.c_type is
        when GDNATIVE_EXT_NATIVESCRIPT => Nativescript_Api := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_PLUGINSCRIPT => Pluginscript_Api := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_ANDROID      => Android_Api      := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_ARVR         => Arvr_Api         := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_VIDEODECODER => Videodecoder_Api := To_Api_Struct_Ptr (Cursor.all);
        when GDNATIVE_EXT_NET          => Net_Api          := To_Api_Struct_Ptr (Cursor.all);
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

  -----------------------
  -- Finalize GDNative --
  -----------------------
  procedure Finalize_GDNative (p_options : access godot_gdnative_terminate_options) is begin
    Core_Initialized         := False;
    Core_Api                 := null;
    Nativescript_Initialized := False;
    Nativescript_Api         := null;
    Nativescript_Ptr         := Null_Handle;
    Pluginscript_Api         := null;
    Android_Api              := null;
    Arvr_Api                 := null;
    Videodecoder_Api         := null;
    Net_Api                  := null;
  end;

  ---------
  -- Put --
  ---------
  procedure Put (Item : in Wide_String) is 
    C_Item     : IC.wchar_array := IC.To_C (Item);
    Godot_Item : aliased godot_string;
  begin
    Assert_Core_Initalized;

    Core_Api.godot_string_new_with_wide_string (Godot_Item'access, C_Item (0)'access, C_Item'Length);
    Core_Api.godot_print (Godot_Item'access);
    Core_Api.godot_string_destroy (Godot_Item'access);
  end;


  -----------------------------
  -- Initialize_Nativescript --
  -----------------------------
  procedure Initialize_Nativescript (p_handle : in Nativescript_Handle) is begin
    if Core_Initialized then
      raise Multiple_Initalizations with "Nativescript already Initialized!";
    end if;

    Nativescript_Ptr         := p_handle;
    Nativescript_Initialized := True;
  end;

  -------------
  -- Objects --
  -------------
  package body Objects is

    package body Object_Registration is

      NO_PARAMS : aliased No_Parameters;

      package Cast is new System.Address_To_Access_Conversions (T);

      function Create (
        p_instance    : S.Address;
        p_method_data : S.Address)
        return S.Address
      is
        Addr : S.Address := Core_Api.godot_alloc (IC.int (T'size));
        Access_Instance : Cast.Object_Pointer := Cast.To_Pointer (Addr);
      begin
        Access_Instance.all := T (Construct (T'tag, NO_PARAMS'access));
        return Addr;
      end;

      procedure Destroy (
        p_instance    : S.Address;
        p_method_data : S.Address;
        p_user_data   : S.Address)
      is begin
        Core_Api.godot_free (p_user_data);
      end;

      --------------------
      -- Register_Class --
      --------------------
      procedure Register_Class is
        Create_Func  : godot_instance_create_func  := (Create'access, S.Null_Address, null);
        Destroy_Func : godot_instance_destroy_func := (Destroy'access, S.Null_Address, null);
       
        Name_Ptr : ICS.chars_ptr := ICS.New_String (Ada.Tags.Expanded_Name (T'Tag));

        -- I honestly don't know what this is for
        Reference_Ptr : ICS.chars_ptr := ICS.New_String ("Reference");
      begin
        Assert_Nativescript_Initalized;

        Nativescript_Api.godot_nativescript_register_class (
          Nativescript_Ptr, 
          Name_Ptr,
          Reference_Ptr,
          Create_Func, 
          Destroy_Func);
        ICS.Free (Name_Ptr); ICS.Free (Reference_Ptr);
      end;

    end;

    package body Node_Registration is

      NO_PARAMS : aliased No_Parameters;

      package Cast is new System.Address_To_Access_Conversions (T);

      function Process (
        p_instance    : S.Address;
        p_method_data : S.Address;
        p_user_data   : S.Address;
        p_num_args    : IC.int;
        p_args        : Godot_Instance_Method_Args_Ptrs.Pointer) -- godot_variant **
        return GDNative.godot_variant
      is
        Result   : aliased godot_variant;
        Access_Instance : Cast.Object_Pointer := Cast.To_Pointer (p_user_data);
        Time_Elapsed : IC.double := Core_Api.godot_variant_as_real (p_args.all);
      begin
        Process (Access_Instance.all, Long_Float (Time_Elapsed));

        Core_Api.godot_variant_new_nil (Result'access);
        return Result;
      end;

      ----------------------
      -- Register Process --
      ----------------------
      procedure Register_Process is
        Process_Ptr  : ICS.chars_ptr := ICS.New_String ("_process");
        Process_Func : godot_instance_method   := (Process'access, S.Null_Address, null);
        Process_Attr : godot_method_attributes := (rpc_type => GODOT_METHOD_RPC_MODE_DISABLED);

        Name_Ptr : ICS.chars_ptr := ICS.New_String (Ada.Tags.Expanded_Name (T'tag));
      begin
        Nativescript_Api.godot_nativescript_register_method (
          Nativescript_Ptr,
          Name_Ptr,
          Process_Ptr, 
          Process_Attr, 
          Process_Func);
        ICS.Free (Name_Ptr);
      end;
    end;
  end;

end;