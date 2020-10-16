
package body Example_Object is

  use GDNative;
  package IC renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;

  type User_Data_State is record
    Data : aliased godot_string;
  end record;

  function To_Ada (p_user_data : System.Address) return User_Data_State is
    User_Data : User_Data_State;
    for User_Data'address use p_user_data;
    pragma Import (C, User_Data);
  begin
    return User_Data;
  end;

  -----------------
  -- Constructor --
  -----------------
  function Constructor (
    p_instance    : System.Address;
    p_method_data : System.Address)
    return System.Address
  is
    User_Data : User_Data_State := To_Ada (Core_Api.godot_alloc (User_Data_State'Size));
    Data      : IC.wchar_array  := IC.To_C ("World from GDNative!");
  begin
    Core_Api.godot_string_new_with_wide_string (User_Data.Data'access, Data (Data'First)'access, Data'length);
    return User_Data'address;
  end;

  ----------------
  -- Destructor --
  ----------------
  procedure Destructor (
    p_instance    : System.Address;
    p_method_data : System.Address;
    p_user_data   : System.Address)
  is 
    User_Data : User_Data_State := To_Ada (p_user_data);
  begin
    Core_Api.godot_string_destroy (User_Data.Data'access);
    Core_Api.godot_free (p_user_data);
  end;

  -------------
  -- Process --
  -------------
  function Process (
    p_instance    : System.Address;
    p_method_data : System.Address;
    p_user_data   : System.Address;
    p_num_args    : IC.int;
    p_args        : Godot_Instance_Method_Args_Ptrs.Pointer)
    return godot_variant
  is
    use IC;
    User_Data    : User_Data_State := To_Ada (p_user_data);
    Time_Elapsed : double;
    Result       : aliased godot_variant;
  begin
    if 1 /= p_num_args then raise Program_Error; end if;

    Time_Elapsed := Core_Api.godot_variant_as_real (p_args.all);

    Godot_Print (Time_Elapsed'Wide_Image);

    Core_Api.godot_variant_new_nil (Result'access);
    return Result;
  end;

  --------------
  -- Register --
  --------------
  procedure Register (Con : in GDNative_Context) is 
    Create_Func  : godot_instance_create_func  := (Constructor'access, System.Null_Address, null);
    Destroy_Func : godot_instance_destroy_func := (Destructor'access, System.Null_Address, null);
    Process_Func : godot_instance_method       := (Process'access, System.Null_Address, null);
    Process_Attr : godot_method_attributes     := (rpc_type => GODOT_METHOD_RPC_MODE_DISABLED);

    Name_Ptr      : ICS.chars_ptr := ICS.New_String ("Example_Object");
    Reference_Ptr : ICS.chars_ptr := ICS.New_String ("Reference");
    Process_Ptr   : ICS.chars_ptr := ICS.New_String ("_process");
  begin
    Context := Con;

    Nativescript_Api.godot_nativescript_register_class (p_handle, Name_Ptr, Reference_Ptr, Create_Func, Destroy_Func);
    Nativescript_Api.godot_nativescript_register_method (p_handle, Name_Ptr, Process_Ptr, Process_Attr, Process_Func);
    ICS.Free (Name_Ptr); ICS.Free (Reference_Ptr); ICS.Free (Process_Ptr);
  end;

end;