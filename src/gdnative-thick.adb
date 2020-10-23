
with System;
with System.Memory_Copy;
with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Tags;
with Ada.Exceptions;

package body GDNative.Thick is

  package S renames System;
  package SMC renames System.Memory_Copy;
  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;

  package body Objects is

    --------------------
    -- Register_Class --
    --------------------
    procedure Register_Class (T : in Ada.Tags.Tag) is 
      
      NO_PARAMS : aliased No_Parameters;

      function Create (
        p_instance    : S.Address;
        p_method_data : S.Address)
        return S.Address
      is
        Source : Object := Construct (T, NO_PARAMS'access);
        Target : S.Address := Context.Core_Api.godot_alloc (Source'size);
      begin
        SMC.memcpy (Source'address, Target, Source'size);
        return Target;
      end;

      procedure Destroy (
        p_instance    : System.Address;
        p_method_data : System.Address;
        p_user_data   : System.Address)
      is 
        Instance : Object;
        for Instance'address use p_user_data;
      begin
        Free (Instance);
        Context.Core_Api.godot_free (p_user_data);
      end;

      Create_Func  : godot_instance_create_func  := (Create'access, System.Null_Address, null);
      Destroy_Func : godot_instance_destroy_func := (Destroy'access, System.Null_Address, null);
     
      Name_Ptr : ICS.chars_ptr := ICS.New_String (Ada.Tags.Expanded_Name (T));

      -- I honestly don't know what this is for
      Reference_Ptr : ICS.chars_ptr := ICS.New_String ("Reference");
    begin
      Assert_Nativescript_Initalized;

      Context.Nativescript_Api.godot_nativescript_register_class (
        Context.Nativescript_Ptr, 
        Name_Ptr,
        Reference_Ptr,
        Create_Func, 
        Destroy_Func);
      ICS.Free (Name_Ptr); ICS.Free (Reference_Ptr);
    end;

    ----------------------
    -- Register Process --
    ----------------------
    procedure Register_Process (Node : Ada.Tags.Tag, Callback : Process_Callback_Access) is

      function Process (
        p_instance    : System.Address;
        p_method_data : System.Address;
        p_user_data   : System.Address;
        p_num_args    : Interfaces.C.int;
        p_args        : Godot_Instance_Method_Args_Ptrs.Pointer) -- godot_variant **
        return GDNative.godot_variant 
      is
        Result   : aliased godot_variant;
        Instance : Node;
        for Instance'address use p_user_data;
        Time_Elapsed : double := Core_Api.godot_variant_as_real (p_args.all);
      begin
        Process (Instance, Long_Float (Time_Elapsed));

        Core_Api.godot_variant_new_nil (Result'access);
        return Result;
      end;

      Process_Ptr   : ICS.chars_ptr := ICS.New_String ("_process");
      Process_Func : godot_instance_method   := (Process'access, System.Null_Address, null);
      Process_Attr : godot_method_attributes := (rpc_type => GODOT_METHOD_RPC_MODE_DISABLED);

      Name_Ptr : ICS.chars_ptr := ICS.New_String (Ada.Tags.Expanded_Name (T));
    begin
      Context.Nativescript_Api.godot_nativescript_register_method (
        Context.Nativescript_Ptr,
        Name_Ptr,
        Process_Ptr, 
        Process_Attr, 
        Process_Func);
      ICS.Free (Name_Ptr);
    end;
  end;

  ------------------------
  -- Initalize GDNative --
  ------------------------
  procedure Initialize_GDNative (p_options : access godot_gdnative_init_options) is 
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

  -----------------------
  -- Finalize GDNative --
  -----------------------
  procedure Finalize_GDNative (p_options : in godot_gdnative_terminate_options) is begin
    Context.Core_Initialized         := False;
    Context.Core_Api                 := null;
    Context.Nativescript_Initialized := False;
    Context.Nativescript_Api         := null;
    Context.Nativescript_Ptr         := Null_Handle;
    Context.Pluginscript_Api         := null;
    Context.Android_Api              := null;
    Context.Arvr_Api                 := null;
    Context.Videodecoder_Api         := null;
    Context.Net_Api                  := null;
  end;

  ----------------------------
  -- Assert Core Initalized --
  ----------------------------
  procedure Assert_Core_Initalized is begin
    if Context.Core_Initialized then return; end if;
    raise Context_Uninitialized with "GDNative Context not Initialized!";
  end;

  ------------------------------------
  -- Assert Nativescript Initalized --
  ------------------------------------
  procedure Assert_Nativescript_Initalized is begin
    if Context.Core_Initialized and then Context.Nativescript_Initialized then return; end if;
    raise Context_Uninitialized with "GDNative Context Nativescript not Initialized!";
  end;

  ---------
  -- Put --
  ---------
  procedure Put (Item : in Wide_String) is 
    C_Item     : IC.wchar_array := IC.To_C (Item);
    Godot_Item : aliased godot_string;
  begin
    Assert_Core_Initalized;

    Context.Core_Api.godot_string_new_with_wide_string (Godot_Item'access, C_Item (0)'access, C_Item'Length);
    Context.Core_Api.godot_print (Godot_Item'access);
    Context.Core_Api.godot_string_destroy (Godot_Item'access);
  end;

  -----------------------------
  -- Initialize_Nativescript --
  -----------------------------
  procedure Initialize_Nativescript (p_handle : in Nativescript_Handle) is begin
    if Context.Core_Initialized then
      raise Multiple_Initalizations with "Nativescript already Initialized!";
    end if;

    Context.Nativescript_Ptr         := p_handle;
    Context.Nativescript_Initialized := True;
  exception
    when Error: others => Put (Ada.Exceptions.Exception_Information (Error));
  end;

end;