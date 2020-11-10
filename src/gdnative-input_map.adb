with Interfaces.C;
with Interfaces.C.Strings;

with GDNative.Context;
with GDNative.Console;
with GDNative.Exceptions;
with GDNative.Strings;
with GDNative.Variants;

package body GDNative.Input_Map is

  use GDNative.Thin;
  use GDNative.Variants;

  package IC  renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;

  --------------------------
  -- Get_Input_Map_Method --
  --------------------------
  function Get_Input_Map_Method (Input_Map_Class_Name : in ICS.chars_ptr; Method_Name : in String) return godot_method_bind_ptr is
    C_Method_Name : ICS.chars_ptr := ICS.New_String (Method_Name);
    Result        : godot_method_bind_ptr;
  begin
    Result := Context.Core_Api.godot_method_bind_get_method (Input_Map_Class_Name, C_Method_Name);
    ICS.Free (C_Method_Name);
    return Result;
  end;

  ----------------
  -- Initialize --
  ----------------
  procedure Initialize is
    Input_Map_Class_Name : ICS.chars_ptr := ICS.New_String ("InputMap");
  begin
    pragma Assert (Context.Core_Initialized,            CORE_UNINITIALIZED_ASSERT);
    pragma Assert (not Input_Map_Singleton.Initialized, INPUT_MAP_MULTIPLE_INITIALIZATION_ASSERT);
    Input_Map_Singleton.Handle              := Context.Core_Api.godot_global_get_singleton (Input_Map_Class_Name);
    Input_Map_Singleton.Action_Add_Event    := Get_Input_Map_Method (Input_Map_Class_Name, "action_add_event");
    Input_Map_Singleton.Action_Erase_Event  := Get_Input_Map_Method (Input_Map_Class_Name, "action_erase_event");
    Input_Map_Singleton.Action_Erase_Events := Get_Input_Map_Method (Input_Map_Class_Name, "action_erase_events");
    Input_Map_Singleton.Action_Has_Event    := Get_Input_Map_Method (Input_Map_Class_Name, "action_has_event");
    Input_Map_Singleton.Action_Set_Deadzone := Get_Input_Map_Method (Input_Map_Class_Name, "action_set_deadzone");
    Input_Map_Singleton.Add_Action          := Get_Input_Map_Method (Input_Map_Class_Name, "add_action");
    Input_Map_Singleton.Erase_Action        := Get_Input_Map_Method (Input_Map_Class_Name, "erase_action");
    Input_Map_Singleton.Event_Is_Action     := Get_Input_Map_Method (Input_Map_Class_Name, "event_is_action");
    Input_Map_Singleton.Get_Action_List     := Get_Input_Map_Method (Input_Map_Class_Name, "get_action_list");
    Input_Map_Singleton.Get_Actions         := Get_Input_Map_Method (Input_Map_Class_Name, "get_actions");
    Input_Map_Singleton.Has_Action          := Get_Input_Map_Method (Input_Map_Class_Name, "has_action");
    Input_Map_Singleton.Load_From_Globals   := Get_Input_Map_Method (Input_Map_Class_Name, "load_from_globals");
    Input_Map_Singleton.Initialized         := True;
    ICS.Free (Input_Map_Class_Name);
  end;

  --------------
  -- Finalize --
  --------------
  procedure Finalize is begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_EARLY_FINALIZE_ASSERT);
    Input_Map_Singleton.Initialized         := False;
    Input_Map_Singleton.Handle              := Null_Godot_Object;
    Input_Map_Singleton.Action_Add_Event    := null;
    Input_Map_Singleton.Action_Erase_Event  := null;
    Input_Map_Singleton.Action_Erase_Events := null;
    Input_Map_Singleton.Action_Has_Event    := null;
    Input_Map_Singleton.Action_Set_Deadzone := null;
    Input_Map_Singleton.Add_Action          := null;
    Input_Map_Singleton.Erase_Action        := null;
    Input_Map_Singleton.Event_Is_Action     := null;
    Input_Map_Singleton.Get_Action_List     := null;
    Input_Map_Singleton.Get_Actions         := null;
    Input_Map_Singleton.Has_Action          := null;
    Input_Map_Singleton.Load_From_Globals   := null;
  end;

  ----------------------
  -- Method Bind Call --
  ----------------------
  procedure Method_Bind_Call (
    Method_Bind : in  godot_method_bind_ptr;
    Arguments   : access godot_variant_const_ptr;
    Arg_Count   : in  IC.int;
    Result      : out Variant)
  is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Temp_Result : aliased godot_variant; 
  begin
    Temp_Result := Context.Core_Api.godot_method_bind_call (
      Method_Bind,
      Input_Map_Singleton.Handle, 
       (Arguments),  
      Arg_Count, 
      Call_Result'access);
    Initialize (Result, Temp_Result'unchecked_access);
    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  end;

  ----------------------
  -- Writer Bind Call --
  ----------------------
  procedure Writer_Bind_Call (
    Method_Bind : in godot_method_bind_ptr;
    Arguments   : access godot_variant_const_ptr;
    Arg_Count   : in IC.int)
  is 
    Nil : Variant;
  begin
    Method_Bind_Call (Method_Bind, Arguments, Arg_Count, Nil);
    if Kind (Nil) /= Nil_Kind then
      Exceptions.Put_Warning ("Expected Nil Result from procedure call");
    end if;
  end;

  ----------------------
  -- Getter Bind Call --
  ----------------------
  procedure Reader_Bind_Call (Method_Bind : in godot_method_bind_ptr; Result : out Variant) is begin
    Method_Bind_Call (Method_Bind, null, 0, Result);
  end;

  -----------------------
  -- Command Bind Call --
  -----------------------
  procedure Command_Bind_Call (Method_Bind : in godot_method_bind_ptr) is begin
    Writer_Bind_Call (Method_Bind, null, 0);
  end;

  ----------------------
  -- Action Add Event --
  ----------------------
  -- TODO: InputEvent
  procedure Action_Add_Event (Action : in Wide_String; Event : in InputEvent) is begin
    raise Unimplemented_Feature;
  end;

  ------------------------
  -- Action Erase Event --
  ------------------------
  -- TODO: InputEvent
  procedure Action_Erase_Event (Action : in Wide_String; Event : in InputEvent) is begin
    raise Unimplemented_Feature;
  end;

  -------------------------
  -- Action Erase Events --
  -------------------------
  procedure Action_Erase_Events (Action : in Wide_String) is
    Action_Variant : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Writer_Bind_Call (Input_Map_Singleton.Action_Erase_Events, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  ----------------------
  -- Action Has Event --
  ----------------------
  -- TODO: InputEvent
  function Action_Has_Event (Action : in Wide_String; Event : in InputEvent) return Boolean is begin
    raise Unimplemented_Feature;
    return False;
  end;

  -------------------------
  -- Action Set Deadzone --
  -------------------------
  procedure Action_Set_Deadzone (Action : in Wide_String; Deadzone : in Real_64_Percent) is
    Action_Variant   : Variant;
    Deadzone_Variant : Variant;
    Arguments        : aliased godot_variant_const_ptr_array (1 .. 2) ;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant,   Action);
    Initialize (Deadzone_Variant, Deadzone);
    Arguments := (Ref (Action_Variant), Ref (Deadzone_Variant));
    Writer_Bind_Call (Input_Map_Singleton.Action_Set_Deadzone, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  ----------------
  -- Add Action --
  ----------------
  procedure Add_Action (Action : in Wide_String; Deadzone : in Real_64_Percent := 0.5) is 
    Action_Variant   : Variant;
    Deadzone_Variant : Variant;
    Arguments        : aliased godot_variant_const_ptr_array (1 .. 2);
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant,   Action);
    Initialize (Deadzone_Variant, Deadzone);
    Arguments := (Ref (Action_Variant), Ref (Deadzone_Variant));
    Writer_Bind_Call (Input_Map_Singleton.Add_Action, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  ------------------
  -- Erase Action --
  ------------------
  procedure Erase_Action (Action : in Wide_String) is
    Action_Variant : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Writer_Bind_Call (Input_Map_Singleton.Add_Action, Arguments (1)'unchecked_access,  Arguments'Length);
  end;

  ---------------------
  -- Event Is Action --
  ---------------------
  -- TODO: InputEvent
  procedure Event_Is_Action (Event : in InputEvent; Action : in Wide_String) is begin
    raise Unimplemented_Feature;
  end;

  ---------------------
  -- Get Action List --
  ---------------------
  procedure Get_Action_List (Action : in Wide_String; Result : out Arrays.GArray) is
    Action_Variant : Variant;
    Result_Variant : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Method_Bind_Call (Input_Map_Singleton.Add_Action, Arguments (1)'unchecked_access, Arguments'Length, Result_Variant);
    Arrays.Copy (Result, Ref (Result_Variant));
  end;

  -----------------
  -- Get_Actions --
  -----------------
  procedure Get_Actions (Result : out Arrays.GArray) is
    Result_Variant : Variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Map_Singleton.Add_Action, Result_Variant);
    Arrays.Copy (Result, Ref (Result_Variant));
  end;

  ----------------
  -- Has Action --
  ----------------
  function Has_Action (Action : in Wide_String) return Boolean is
    Action_Variant : Variant;
    Result_Variant : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Method_Bind_Call (Input_Map_Singleton.Has_Action, Arguments (1)'unchecked_access, Arguments'Length, Result_Variant);
    return Value (Result_Variant);
  end;

  -----------------------
  -- Load From Globals --
  -----------------------
  procedure Load_From_Globals is begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);
    Command_Bind_Call (Input_Map_Singleton.Load_From_Globals);
  end;
end;
