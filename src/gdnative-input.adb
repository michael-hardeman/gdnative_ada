with Interfaces.C.Strings;

with GDNative.Context;
with GDNative.Console;
with GDNative.Exceptions;
with GDNative.Variants;
with GDNative.Input_Map;

package body GDNative.Input is

  use GDNative.Thin;
  
  package ICS renames Interfaces.C.Strings;

  ----------------------
  -- Get_Input_Method --
  ----------------------
  function Get_Input_Method (Input_Class_Name : in ICS.chars_ptr; Method_Name : in String) return access godot_method_bind is
    C_Method_Name : ICS.chars_ptr := ICS.New_String (Method_Name);
    Result        : access godot_method_bind;
  begin
    Result := Context.Core_Api.godot_method_bind_get_method (Input_Class_Name, C_Method_Name);
    ICS.Free (C_Method_Name);
    return Result;
  end;

  ----------------
  -- Initialize --
  ----------------
  procedure Initialize is
    Input_Class_Name : ICS.chars_ptr := ICS.New_String ("Input");
  begin
    pragma Assert (Context.Core_Initialized,        CORE_UNINITIALIZED_ASSERT);
    pragma Assert (not Input_Singleton.Initialized, INPUT_MULTIPLE_INITIALIZATION_ASSERT);

    Input_Singleton.Handle                           := Context.Core_Api.godot_global_get_singleton (Input_Class_Name);
    Input_Singleton.Action_Press                     := Get_Input_Method (Input_Class_Name, "action_press");
    Input_Singleton.Action_Release                   := Get_Input_Method (Input_Class_Name, "action_release");
    Input_Singleton.Add_Joy_Mapping                  := Get_Input_Method (Input_Class_Name, "add_joy_mapping");
    Input_Singleton.Get_Accelerometer                := Get_Input_Method (Input_Class_Name, "get_accelerometer");
    Input_Singleton.Get_Action_Strength              := Get_Input_Method (Input_Class_Name, "get_action_strength");
    Input_Singleton.Get_Connected_Joypads            := Get_Input_Method (Input_Class_Name, "get_connected_joypads");
    Input_Singleton.Get_Current_Cursor_Shape         := Get_Input_Method (Input_Class_Name, "get_current_cursor_shape");
    Input_Singleton.Get_Gravity                      := Get_Input_Method (Input_Class_Name, "get_gravity");
    Input_Singleton.Get_Gyroscope                    := Get_Input_Method (Input_Class_Name, "get_gyroscope");
    Input_Singleton.Get_Joy_Axis                     := Get_Input_Method (Input_Class_Name, "get_joy_axis");
    Input_Singleton.Get_Joy_Axis_Index_From_String   := Get_Input_Method (Input_Class_Name, "get_joy_axis_index_from_string");
    Input_Singleton.Get_Joy_Axis_String              := Get_Input_Method (Input_Class_Name, "get_joy_axis_string");
    Input_Singleton.Get_Joy_Button_Index_From_String := Get_Input_Method (Input_Class_Name, "get_joy_button_index_from_string");
    Input_Singleton.Get_Joy_Button_String            := Get_Input_Method (Input_Class_Name, "get_joy_button_string");
    Input_Singleton.Get_Joy_Guid                     := Get_Input_Method (Input_Class_Name, "get_joy_guid");
    Input_Singleton.Get_Joy_Name                     := Get_Input_Method (Input_Class_Name, "get_joy_name");
    Input_Singleton.Get_Joy_Vibration_Duration       := Get_Input_Method (Input_Class_Name, "get_joy_vibration_duration");
    Input_Singleton.Get_Joy_Vibration_Strength       := Get_Input_Method (Input_Class_Name, "get_joy_vibration_strength");
    Input_Singleton.Get_Last_Mouse_Speed             := Get_Input_Method (Input_Class_Name, "get_last_mouse_speed");
    Input_Singleton.Get_Magnetometer                 := Get_Input_Method (Input_Class_Name, "get_magnetometer");
    Input_Singleton.Get_Mouse_Button_Mask            := Get_Input_Method (Input_Class_Name, "get_mouse_button_mask");
    Input_Singleton.Get_Mouse_Mode                   := Get_Input_Method (Input_Class_Name, "get_mouse_mode");
    Input_Singleton.Is_Action_Just_Pressed           := Get_Input_Method (Input_Class_Name, "is_action_just_pressed");
    Input_Singleton.Is_Action_Just_Released          := Get_Input_Method (Input_Class_Name, "is_action_just_released");
    Input_Singleton.Is_Action_Pressed                := Get_Input_Method (Input_Class_Name, "is_action_pressed");
    Input_Singleton.Is_Joy_Button_Pressed            := Get_Input_Method (Input_Class_Name, "is_joy_button_pressed");
    Input_Singleton.Is_Joy_Known                     := Get_Input_Method (Input_Class_Name, "is_joy_known");
    Input_Singleton.Is_Key_Pressed                   := Get_Input_Method (Input_Class_Name, "is_key_pressed");
    Input_Singleton.Is_Mouse_Button_Pressed          := Get_Input_Method (Input_Class_Name, "is_mouse_button_pressed");
    Input_Singleton.Joy_Connection_Changed           := Get_Input_Method (Input_Class_Name, "joy_connection_changed");
    Input_Singleton.Parse_Input_Event                := Get_Input_Method (Input_Class_Name, "parse_input_event");
    Input_Singleton.Remove_Joy_Mapping               := Get_Input_Method (Input_Class_Name, "remove_joy_mapping");
    Input_Singleton.Set_Custom_Mouse_Cursor          := Get_Input_Method (Input_Class_Name, "set_custom_mouse_cursor");
    Input_Singleton.Set_Default_Cursor_Shape         := Get_Input_Method (Input_Class_Name, "set_default_cursor_shape");
    Input_Singleton.Set_Mouse_Mode                   := Get_Input_Method (Input_Class_Name, "set_mouse_mode");
    Input_Singleton.Set_Use_Accumulated_Input        := Get_Input_Method (Input_Class_Name, "set_use_accumulated_input");
    Input_Singleton.Start_Joy_Vibration              := Get_Input_Method (Input_Class_Name, "start_joy_vibration");
    Input_Singleton.Stop_Joy_Vibration               := Get_Input_Method (Input_Class_Name, "stop_joy_vibration");
    Input_Singleton.Vibrate_Handheld                 := Get_Input_Method (Input_Class_Name, "vibrate_handheld");
    Input_Singleton.Warp_Mouse_Position              := Get_Input_Method (Input_Class_Name, "warp_mouse_position");
    Input_Singleton.Initialized                      := True;
    ICS.Free (Input_Class_Name);
  end;

  --------------
  -- Finalize --
  --------------
  procedure Finalize is begin
    pragma Assert (Input_Singleton.Initialized, INPUT_EARLY_FINALIZE_ASSERT);

    Input_Singleton.Initialized                      := False;
    Input_Singleton.Handle                           := Null_Godot_Object;
    Input_Singleton.Action_Press                     := null;
    Input_Singleton.Action_Release                   := null;
    Input_Singleton.Add_Joy_Mapping                  := null;
    Input_Singleton.Get_Accelerometer                := null;
    Input_Singleton.Get_Action_Strength              := null;
    Input_Singleton.Get_Connected_Joypads            := null;
    Input_Singleton.Get_Current_Cursor_Shape         := null;
    Input_Singleton.Get_Gravity                      := null;
    Input_Singleton.Get_Gyroscope                    := null;
    Input_Singleton.Get_Joy_Axis                     := null;
    Input_Singleton.Get_Joy_Axis_Index_From_String   := null;
    Input_Singleton.Get_Joy_Axis_String              := null;
    Input_Singleton.Get_Joy_Button_Index_From_String := null;
    Input_Singleton.Get_Joy_Button_String            := null;
    Input_Singleton.Get_Joy_Guid                     := null;
    Input_Singleton.Get_Joy_Name                     := null;
    Input_Singleton.Get_Joy_Vibration_Duration       := null;
    Input_Singleton.Get_Joy_Vibration_Strength       := null;
    Input_Singleton.Get_Last_Mouse_Speed             := null;
    Input_Singleton.Get_Magnetometer                 := null;
    Input_Singleton.Get_Mouse_Button_Mask            := null;
    Input_Singleton.Get_Mouse_Mode                   := null;
    Input_Singleton.Is_Action_Just_Pressed           := null;
    Input_Singleton.Is_Action_Just_Released          := null;
    Input_Singleton.Is_Action_Pressed                := null;
    Input_Singleton.Is_Joy_Button_Pressed            := null;
    Input_Singleton.Is_Joy_Known                     := null;
    Input_Singleton.Is_Key_Pressed                   := null;
    Input_Singleton.Is_Mouse_Button_Pressed          := null;
    Input_Singleton.Joy_Connection_Changed           := null;
    Input_Singleton.Parse_Input_Event                := null;
    Input_Singleton.Remove_Joy_Mapping               := null;
    Input_Singleton.Set_Custom_Mouse_Cursor          := null;
    Input_Singleton.Set_Default_Cursor_Shape         := null;
    Input_Singleton.Set_Mouse_Mode                   := null;
    Input_Singleton.Set_Use_Accumulated_Input        := null;
    Input_Singleton.Start_Joy_Vibration              := null;
    Input_Singleton.Stop_Joy_Vibration               := null;
    Input_Singleton.Vibrate_Handheld                 := null;
    Input_Singleton.Warp_Mouse_Position              := null;
  end;

  ------------------
  -- Action Press --
  ------------------
  procedure Action_Press (Action : in Wide_String; Strength : in Percentage := 1.0) is
    Action_Param   : aliased godot_variant := Variants.To_Godot (Action);
    Strength_Param : aliased godot_variant := Variants.To_Godot (Long_Float (Strength));
    Arguments      : aliased godot_variant_ptr_array (1 .. 2) := (Action_Param'unchecked_access, Strength_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : aliased godot_variant;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Action_Press,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  --------------------
  -- Action_Release --
  --------------------
  procedure Action_Release (Action : in Wide_String) is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Action_Release,
      Input_Singleton.Handle, 
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
  -- Add_Joy_Mapping --
  ---------------------
  -- Mapping is an SDL2 gamepad mapping string
  -- Get one from this tool: https://generalarcade.com/gamepadtool/
  -- Todo: create a package for generating these strings
  procedure Add_Joy_Mapping (Mapping : in Wide_String; Update_Existing : in Boolean := False) is
    Mapping_Param : aliased godot_variant := Variants.To_Godot (Mapping);
    Update_Param  : aliased godot_variant := Variants.To_Godot (Update_Existing);
    Arguments     : aliased godot_variant_ptr_array (1 .. 2) := (Mapping_Param'unchecked_access, Update_Param'unchecked_access);
    Call_Result   : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result        : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Add_Joy_Mapping,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  -----------------------
  -- Get_Accelerometer --
  -----------------------
  function Get_Accelerometer return Math.Vector3 is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Accelerometer,
      Input_Singleton.Handle, 
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
      return Math.Zero;
  end;

  -------------------------
  -- Get_Action_Strength --
  -------------------------
  function Get_Action_Strength (Action : in Wide_String) return Percentage is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
    Temp         : Long_Float;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Action_Strength,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access,
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;

    Temp := Variants.To_Ada (Result'access);
    return Percentage (Temp);
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
      return 0.0;
  end;

  ---------------------------
  -- Get_Connected_Joypads --
  ---------------------------
  -- TODO: JoypadArray
  function Get_Connected_Joypads return JoypadArray is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Null_JoypadArray;
  end;

  ------------------------------
  -- Get_Current_Cursor_Shape --
  ------------------------------
  function Get_Current_Cursor_Shape return Cursor_Shape_Kind is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
    Temp        : Natural;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Current_Cursor_Shape,
      Input_Singleton.Handle, 
      null, 
      0, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;

    Temp := Variants.To_Ada (Result'access);
    return Cursor_Shape_Kind'Val (Temp);
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
      return Arrow_Shape;
  end;

  -----------------
  -- Get_Gravity --
  -----------------
  function Get_Gravity return Math.Vector3 is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Gravity,
      Input_Singleton.Handle, 
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
      return Math.Zero;
  end;

  -------------------
  -- Get_Gyroscope --
  -------------------
  function Get_Gyroscope return Math.Vector3 is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Gyroscope,
      Input_Singleton.Handle, 
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
      return Math.Zero;
  end;

  ------------------
  -- Get_Joy_Axis --
  ------------------
  function Get_Joy_Axis (Device : in Long_Integer; Axis : in Joypad_Axis_Kind) return Percentage is
    Device_Param : aliased godot_variant := Variants.To_Godot (Device);
    Axis_Param   : aliased godot_variant := Variants.To_Godot (Long_Integer (Joypad_Axis_Kind'Pos (Axis)));
    Arguments    : aliased godot_variant_ptr_array (1 .. 2) := (Device_Param'unchecked_access, Axis_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
    Temp         : Long_Float;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Axis,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access, 
      Arguments'Length, 
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;

    Temp := Variants.To_Ada (Result'access);
    return Percentage (Temp);
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
      return 0.0;
  end;

  ------------------------------------
  -- Get_Joy_Axis_Index_From_String --
  ------------------------------------
  function Get_Joy_Axis_Index_From_String (Axis : in Wide_String) return Long_Integer is
    Axis_Param  : aliased godot_variant := Variants.To_Godot (Axis);
    Arguments   : aliased godot_variant_ptr_array (1 .. 1) := (1 => Axis_Param'unchecked_access);
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Axis_Index_From_String,
      Input_Singleton.Handle, 
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
      return 0;
  end;

  -------------------------
  -- Get_Joy_Axis_String --
  -------------------------
  function Get_Joy_Axis_String (Axis_Index : in Long_Integer) return Wide_String is
    Axis_Param  : aliased godot_variant := Variants.To_Godot (Axis);
    Arguments   : aliased godot_variant_ptr_array (1 .. 1) := (1 => Axis_Param'unchecked_access);
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Axis_String,
      Input_Singleton.Handle, 
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
      return "";
  end;

  --------------------------------------
  -- Get_Joy_Button_Index_From_String --
  --------------------------------------
  function Get_Joy_Button_Index_From_String (Button : in Wide_String) return Long_Integer is 
    Button_Param : aliased godot_variant := Variants.To_Godot (Button);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Button_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Button_Index_From_String,
      Input_Singleton.Handle, 
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
      return 0;
  end;

  ---------------------------
  -- Get_Joy_Button_String --
  ---------------------------
  function Get_Joy_Button_String (Button_Index : in Long_Integer) return Wide_String is
    Button_Param : aliased godot_variant := Variants.To_Godot (Button_Index);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Button_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Button_String,
      Input_Singleton.Handle, 
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
      return "";
  end;

  ------------------
  -- Get_Joy_Guid --
  ------------------
  function Get_Joy_Guid (Device : in Long_Integer) return Wide_String is
    Device_Param : aliased godot_variant := Variants.To_Godot (Device);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Device_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Guid,
      Input_Singleton.Handle, 
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
      return "";
  end;

  ------------------
  -- Get_Joy_Name --
  ------------------
  function Get_Joy_Name (Device : in Long_Integer) return Wide_String is
    Device_Param : aliased godot_variant := Variants.To_Godot (Device);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Device_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Name,
      Input_Singleton.Handle, 
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
      return "";
  end;

  --------------------------------
  -- Get_Joy_Vibration_Duration --
  --------------------------------
  function Get_Joy_Vibration_Duration (Device : in Long_Integer) return Long_Float is
    Device_Param : aliased godot_variant := Variants.To_Godot (Device);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Device_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Vibration_Duration,
      Input_Singleton.Handle, 
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
      return 0.0;
  end;

  --------------------------------
  -- Get_Joy_Vibration_Strength --
  --------------------------------
  function Get_Joy_Vibration_Strength (Device : in Long_Integer) return Math.Vector2 is
    Device_Param : aliased godot_variant := Variants.To_Godot (Device);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Device_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Joy_Vibration_Strength,
      Input_Singleton.Handle, 
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
      return Math.Zero;
  end;

  --------------------------
  -- Get_Last_Mouse_Speed --
  --------------------------
  function Get_Last_Mouse_Speed return Math.Vector2 is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Last_Mouse_Speed,
      Input_Singleton.Handle, 
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
      return Math.Zero;
  end;

  ----------------------
  -- Get_Magnetometer --
  ----------------------
  function Get_Magnetometer return Math.Vector3 is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Magnetometer,
      Input_Singleton.Handle, 
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
      return Math.Zero;
  end;

  ---------------------------
  -- Get_Mouse_Button_Mask --
  ---------------------------
  function Get_Mouse_Button_Mask return Long_Integer is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Mouse_Button_Mask,
      Input_Singleton.Handle, 
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
      return 0;
  end;

  --------------------
  -- Get_Mouse_Mode --
  --------------------
  function Get_Mouse_Mode return Mouse_Mode_Kind is
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
    Temp        : Natural;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Get_Mouse_Mode,
      Input_Singleton.Handle, 
      null,
      0,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;

    Temp := Variants.To_Ada (Result'access);
    return Mouse_Mode_Kind'Val (Temp);
  exception
    when Error : others => 
      Exceptions.Put_Error (Error);
      return Visible_Mode;
  end;

  ----------------------------
  -- Is_Action_Just_Pressed --
  ----------------------------
  function Is_Action_Just_Pressed (Action : in Wide_String) return Boolean is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Action_Just_Pressed,
      Input_Singleton.Handle, 
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

  -----------------------------
  -- Is_Action_Just_Released --
  -----------------------------
  function Is_Action_Just_Released (Action : in Wide_String) return Boolean is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Action_Just_Released,
      Input_Singleton.Handle, 
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
  -- Is_Action_Pressed --
  -----------------------
  function Is_Action_Pressed (Action : in Wide_String) return Boolean is
    Action_Param : aliased godot_variant := Variants.To_Godot (Action);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Action_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Action_Pressed,
      Input_Singleton.Handle, 
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

  ---------------------------
  -- Is_Joy_Button_Pressed --
  ---------------------------
  function Is_Joy_Button_Pressed (Device : in Long_Integer; Button : in Long_Integer) return Boolean is
    Device_Param : aliased godot_variant := Variants.To_Godot (Device);
    Button_Param : aliased godot_variant := Variants.To_Godot (Button);
    Arguments    : aliased godot_variant_ptr_array (1 .. 2) := (Device_Param'unchecked_access, Button_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Joy_Button_Pressed,
      Input_Singleton.Handle, 
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

  ------------------
  -- Is_Joy_Known --
  ------------------
  function Is_Joy_Known (Device : in Long_Integer) return Boolean is
    Device_Param : aliased godot_variant := Variants.To_Godot (Device);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Device_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Joy_Known,
      Input_Singleton.Handle, 
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

  --------------------
  -- Is_Key_Pressed --
  --------------------
  function Is_Key_Pressed (Scancode : in Long_Integer) return Boolean is
    Scancode_Param : aliased godot_variant := Variants.To_Godot (Scancode);
    Arguments      : aliased godot_variant_ptr_array (1 .. 1) := (1 => Scancode_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Key_Pressed,
      Input_Singleton.Handle, 
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

  -----------------------------
  -- Is_Mouse_Button_Pressed --
  -----------------------------
  function Is_Mouse_Button_Pressed (Button : in Long_Integer) return Boolean is
    Button_Param : aliased godot_variant := Variants.To_Godot (Button);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Button_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Mouse_Button_Pressed,
      Input_Singleton.Handle, 
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

  ----------------------------
  -- Joy_Connection_Changed --
  ----------------------------
  procedure Joy_Connection_Changed (Device : in Long_Integer; Connected : in Boolean; Name : in Wide_String; Guid : in Wide_String) is
    Device_Param    : aliased godot_variant := Variants.To_Godot (Device);
    Connected_Param : aliased godot_variant := Variants.To_Godot (Connected);
    Name_Param      : aliased godot_variant := Variants.To_Godot (Name);
    Guid_Param      : aliased godot_variant := Variants.To_Godot (Guid);
    Arguments       : aliased godot_variant_ptr_array (1 .. 4) := (Device_Param'unchecked_access, Connected_Param'unchecked_access, Name_Param'unchecked_access, Guid_Param'unchecked_access);
    Call_Result     : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result          : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Mouse_Button_Pressed,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  -----------------------
  -- Parse_Input_Event --
  -----------------------
  -- TODO: InputEvent
  procedure Parse_Input_Event (Event : in InputEvent) is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  ------------------------
  -- Remove_Joy_Mapping --
  ------------------------
  procedure Remove_Joy_Mapping (Guid : in Wide_String) is
    Guid_Param  : aliased godot_variant := Variants.To_Godot (Guid);
    Arguments   : aliased godot_variant_ptr_array (1 .. 1) := (1 => Guid_Param'unchecked_access);
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Is_Mouse_Button_Pressed,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  -----------------------------
  -- Set_Custom_Mouse_Cursor --
  -----------------------------
  -- TODO: Resource
  procedure Set_Custom_Mouse_Cursor (Image : in Resource; Shape : in Cursor_Shape_Kind := Arrow_Shape; Hotspot : in Math.Vector2 := Math.Zero) is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  ------------------------------
  -- Set_Default_Cursor_Shape --
  ------------------------------
  procedure Set_Default_Cursor_Shape (Shape : in Cursor_Shape_Kind := Arrow_Shape) is
    Temp        : Long_Integer := Cursor_Shape_Kind'Pos (Shape);
    Shape_Param : aliased godot_variant := Variants.To_Godot (Temp);
    Arguments   : aliased godot_variant_ptr_array (1 .. 1) := (1 => Shape_Param'unchecked_access);
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Set_Default_Cursor_Shape,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  --------------------
  -- Set_Mouse_Mode --
  --------------------
  procedure Set_Mouse_Mode (Mode : in Mouse_Mode_Kind) is
    Temp        : Long_Integer := Mouse_Mode_Kind'Pos (Shape);
    Mode_Param  : aliased godot_variant := Variants.To_Godot (Temp);
    Arguments   : aliased godot_variant_ptr_array (1 .. 1) := (1 => Mode_Param'unchecked_access);
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Set_Mouse_Mode,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  -------------------------------
  -- Set_Use_Accumulated_Input --
  -------------------------------
  procedure Set_Use_Accumulated_Input (Enable : in Boolean) is
    Enable_Param : aliased godot_variant := Variants.To_Godot (Temp);
    Arguments    : aliased godot_variant_ptr_array (1 .. 1) := (1 => Enable_Param'unchecked_access);
    Call_Result  : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result       : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Set_Use_Accumulated_Input,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  -------------------------
  -- Start_Joy_Vibration --
  -------------------------
  procedure Start_Joy_Vibration (Device : in Long_Integer; Weak_Magnitude : in Long_Float; Strong_Magnitude : in Long_Float; Duration : in Long_Float := 0.0) is
    Device_Param   : aliased godot_variant := Variants.To_Godot (Device);
    Weak_Param     : aliased godot_variant := Variants.To_Godot (Weak);
    Strong_Param   : aliased godot_variant := Variants.To_Godot (Strong);
    Duration_Param : aliased godot_variant := Variants.To_Godot (Duration);
    Arguments      : aliased godot_variant_ptr_array (1 .. 4) := (Device_Param'unchecked_access, Weak_Param'unchecked_access, Strong_Param'unchecked_access, Duration_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Start_Joy_Vibration,
      Input_Singleton.Handle, 
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  ------------------------
  -- Stop_Joy_Vibration --
  ------------------------
  procedure Stop_Joy_Vibration (Device : in Long_Integer) is
    Device_Param   : aliased godot_variant := Variants.To_Godot (Device);
    Arguments      : aliased godot_variant_ptr_array (1 .. 1) := (Device_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Start_Joy_Vibration,
      Input_Singleton.Handle,
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
  -- Vibrate_Handheld --
  ----------------------
  procedure Vibrate_Handheld (Duration_Ms : in Long_Integer := 500) is
    Duration_Param : aliased godot_variant := Variants.To_Godot (Duration_Ms);
    Arguments      : aliased godot_variant_ptr_array (1 .. 1) := (Duration_Param'unchecked_access);
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Vibrate_Handheld,
      Input_Singleton.Handle,
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

  -------------------------
  -- Warp_Mouse_Position --
  -------------------------
  procedure Warp_Mouse_Position (To : in Math.Vector2) is
    To_Param    : aliased godot_variant := Variants.To_Godot (To);
    Arguments   : aliased godot_variant_ptr_array (1 .. 1) := (Duration_Param'unchecked_access);
    Call_Result : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result      : aliased godot_variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Result := Context.Core_Api.godot_method_bind_call (
      Input_Singleton.Warp_Mouse_Position,
      Input_Singleton.Handle,
      Arguments (1)'unchecked_access,
      Arguments'Length,
      Call_Result'access);

    if Call_Result /= GODOT_CALL_ERROR_CALL_OK then
      raise Program_Error with Call_Result'Image;
    end if;
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;

end;