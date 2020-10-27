
with System.Address_To_Access_Conversions;

with Interfaces.C.Strings;

with Ada.Exceptions;

with GDNative.Context;
with GDNative.Console;
with GDNative.Exceptions;

package body GDNative.Objects is

  package S   renames System;
  package IC  renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;
  package AE  renames Ada.Exceptions;

  -------------------------
  -- Object Registration --
  -------------------------
  package body Object_Registration is

    NO_PARAMS : aliased No_Parameters;

    package Cast is new System.Address_To_Access_Conversions (T);

    function Create (
      p_instance    : S.Address;
      p_method_data : S.Address)
      return S.Address
    is
      Addr            : S.Address           := Context.Core_Api.godot_alloc (IC.int (T'size));
      Access_Instance : Cast.Object_Pointer := Cast.To_Pointer (Addr);
    begin
      Access_Instance.all := T (Construct (T'tag, NO_PARAMS'access));
      return Addr;
    exception
      when Occurrence : others =>
        Exceptions.Put_Error (Occurrence);
        return S.Null_Address;
    end;

    procedure Destroy (
      p_instance    : S.Address;
      p_method_data : S.Address;
      p_user_data   : S.Address)
    is begin
      Context.Core_Api.godot_free (p_user_data);
    exception
      when Occurrence : others => Exceptions.Put_Error (Occurrence);
    end;

    --------------------
    -- Register_Class --
    --------------------
    procedure Register_Class is
      Create_Func  : Thin.godot_instance_create_func  := (Create'access, S.Null_Address, null);
      Destroy_Func : Thin.godot_instance_destroy_func := (Destroy'access, S.Null_Address, null);
     
      Name_Ptr      : ICS.chars_ptr := ICS.New_String (Ada.Tags.External_Tag (T'Tag));
      Reference_Ptr : ICS.chars_ptr := ICS.New_String ("Reference");
    begin
      Context.Assert_Nativescript_Initalized;

      Console.Put ("Registering Class: " & To_Wide (Ada.Tags.External_Tag (T'Tag)));

      Context.Nativescript_Api.godot_nativescript_register_class (
        Context.Nativescript_Ptr, 
        Name_Ptr,
        Reference_Ptr,
        Create_Func, 
        Destroy_Func);
      ICS.Free (Name_Ptr); 
      ICS.Free (Reference_Ptr);
    exception
      when Occurrence : others => Exceptions.Put_Error (Occurrence);
    end;

  end;

  -----------------------
  -- Node Registration --
  -----------------------
  package body Node_Registration is

    NO_PARAMS : aliased No_Parameters;

    package Cast is new System.Address_To_Access_Conversions (T);

    function Process (
      p_instance    : S.Address;
      p_method_data : S.Address;
      p_user_data   : S.Address;
      p_num_args    : IC.int;
      p_args        : Thin.Godot_Instance_Method_Args_Ptrs.Pointer) -- godot_variant **
      return Thin.godot_variant
    is
      Access_Instance : Cast.Object_Pointer := Cast.To_Pointer (p_user_data);
      Time_Elapsed    : IC.double           := Context.Core_Api.godot_variant_as_real (p_args.all);
    begin
      Process (Access_Instance.all, Long_Float (Time_Elapsed));
      return Context.Nil_Godot_Variant;
    exception
      when Occurrence : others => 
        Exceptions.Put_Error (Occurrence);
        return Context.Nil_Godot_Variant;
    end;

    ----------------------
    -- Register Process --
    ----------------------
    procedure Register_Process is
      Process_Func : Thin.godot_instance_method   := (Process'access, S.Null_Address, null);
      Process_Attr : Thin.godot_method_attributes := (rpc_type => Thin.GODOT_METHOD_RPC_MODE_DISABLED);

      Name_Ptr    : ICS.chars_ptr := ICS.New_String (Ada.Tags.External_Tag (T'tag));
      Process_Ptr : ICS.chars_ptr := ICS.New_String ("_process");
    begin
      Context.Assert_Nativescript_Initalized;

      Console.Put ("Registering Method: " & To_Wide (Ada.Tags.External_Tag (T'Tag)) & "._process");

      Context.Nativescript_Api.godot_nativescript_register_method (
        Context.Nativescript_Ptr,
        Name_Ptr,
        Process_Ptr, 
        Process_Attr, 
        Process_Func);
      ICS.Free (Name_Ptr);
      ICS.Free (Process_Ptr);
    exception
      when Occurrence : others => Exceptions.Put_Error (Occurrence);
    end;
  end;
end;