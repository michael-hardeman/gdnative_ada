with Interfaces.C;
with Interfaces.C.Strings;

with GDNative.Context;
with GDNative.Console;
with GDNative.Exceptions;

package body GDNative.Input_Map is

  use GDNative.Thin;

  package IC  renames Interfaces.C;
  package ICS renames Interfaces.C.Strings;

  --------------------------
  -- Get_Input_Map_Method --
  --------------------------
  function Get_Input_Map_Method (Input_Map_Class_Name : in ICS.chars_ptr; Method_Name : in String) return access Thin.godot_method_bind is
    C_Method_Name : ICS.chars_ptr := ICS.New_String (Method_Name);
    Result        : access Thin.godot_method_bind;
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
    Input_Map_Singleton.Handle              := Thin.Null_Godot_Object;
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
    Action_Param   : aliased godot_variant := Variants.To_Godot (Action);
    Arguments      : aliased Thin.godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Action_Erase_Events,
      Input_Map_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
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
  procedure Action_Set_Deadzone (Action : in Wide_String; Deadzone : in Percentage) is
    Action_Param   : aliased godot_variant := Variants.To_Godot (Action);
    Deadzone_Param : aliased godot_variant := Variants.To_Godot (Long_Float (Deadzone));
    Arguments      : aliased godot_variant_ptr_array (1 .. 2) := (Action_Param'unchecked_access, Deadzone_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Action_Set_Deadzone,
      Input_Map_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  ----------------
  -- Add Action --
  ----------------
  procedure Add_Action (Action : in Wide_String; Deadzone : in Percentage := 0.5) is 
    Action_Param   : aliased godot_variant := Variants.To_Godot (Action);
    Deadzone_Param : aliased godot_variant := Variants.To_Godot (Long_Float (Deadzone));
    Arguments      : aliased godot_variant_ptr_array (1 .. 2) := (Action_Param'unchecked_access, Deadzone_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Add_Action,
      Input_Map_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  ------------------
  -- Erase Action --
  ------------------
  procedure Erase_Action (Action : in Wide_String) is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Erase_Action,
      Input_Map_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
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
  function Get_Action_List (Action : in Wide_String) return Wide_String_Array is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Get_Action_List,
      Input_Map_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;

    return Variants.To_Ada (Result'access);
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
      return Variants.To_Ada (Result'access);
  end;

  -----------------
  -- Get_Actions --
  -----------------
  function Get_Actions return Wide_String_Array is
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Get_Actions,
      Input_Map_Singleton.Handle, 
      null, 
      0, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;

    return Variants.To_Ada (Result'access);
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
      return Variants.To_Ada (Result'access);
  end;

  ----------------
  -- Has Action --
  ----------------
  function Has_Action (Action : in Wide_String) return Boolean is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Has_Action,
      Input_Map_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;

    return Variants.To_Ada (Result'access);
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
      return False;
  end;

  -----------------------
  -- Load From Globals --
  -----------------------
  procedure Load_From_Globals is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Map_Singleton.Initialized, INPUT_MAP_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Map_Singleton.Load_From_Globals,
      Input_Map_Singleton.Handle, 
      null, 
      0, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
  end;

end;