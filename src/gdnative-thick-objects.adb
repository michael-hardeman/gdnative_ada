
with System;
with System.Address_To_Access_Conversions;

with Interfaces.C;
with Interfaces.C.Strings;

With Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;

with GDNative.Thick.Console;

package body GDNative.Thick.Objects is

  package S renames System;
  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;

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

      Console.Put ("Registering_Class: " & To_Wide (Ada.Tags.Expanded_Name (T'Tag)));

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
      Assert_Nativescript_Initalized;

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