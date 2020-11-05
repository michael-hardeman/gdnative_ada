with Interfaces.C;
with Interfaces.C.Strings;

with GDNative.Context;
with GDNative.Console;
with GDNative.Exceptions;
with GDNative.Variants;
with GDNative.Input_Map;

package body GDNative.Input is

  use GDNative.Thin;
  
  package IC  renames Interfaces.C;
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
    Arguments      : aliased godot_variant_ptr_array (1 .. 2) := (others => null);
    Action_Param   : aliased godot_variant;
    Strength_Param : aliased godot_variant;
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : godot_variant;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Action_Param   := Variants.To_Godot (Action);
    Strength_Param := Variants.To_Godot (Long_Float (Strength));

    Arguments (1) := Action_Param'unchecked_access;
    Arguments (2) := Strength_Param'unchecked_access;

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
    Arguments      : aliased godot_variant_ptr_array (1 .. 1) := (others => null);
    Action_Param   : aliased godot_variant;
    Call_Result    : aliased godot_variant_call_error := GODOT_CALL_ERROR_CALL_OK;
    Result         : godot_variant;
  begin
    if not Input_Map.Has_Action (Action) then
      Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
    end if;

    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);

    Action_Param := Variants.To_Godot (Action);

    Arguments (1) := Action_Param'unchecked_access;

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

  ---------------------
  -- Add_Joy_Mapping --
  ---------------------
  procedure Add_Joy_Mapping (Mapping : in Wide_String; Update_Existing : in Boolean := False) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  -----------------------
  -- Get_Accelerometer --
  -----------------------
  function Get_Accelerometer return Math.Vector3 is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Math.New_Vector3 (0.0, 0.0, 0.0);
  end;

  -------------------------
  -- Get_Action_Strength --
  -------------------------
  function Get_Action_Strength (Action : in Wide_String) return Percentage is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return 0.0;
  end;

  ---------------------------
  -- Get_Connected_Joypads --
  ---------------------------
  function Get_Connected_Joypads return JoypadArray is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Null_JoypadArray;
  end;

  ------------------------------
  -- Get_Current_Cursor_Shape --
  ------------------------------
  function Get_Current_Cursor_Shape return CursorShape is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Null_CursorShape;
  end;

  -----------------
  -- Get_Gravity --
  -----------------
  function Get_Gravity return Math.Vector3 is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Math.New_Vector3 (0.0, 0.0, 0.0);
  end;

  -------------------
  -- Get_Gyroscope --
  -------------------
  function Get_Gyroscope return Math.Vector3 is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Math.New_Vector3 (0.0, 0.0, 0.0);
  end;

  ------------------
  -- Get_Joy_Axis --
  ------------------
  function Get_Joy_Axis (Device : in Integer; Axis : in Integer) return Long_Float is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return 0.0;
  end;

  ------------------------------------
  -- Get_Joy_Axis_Index_From_String --
  ------------------------------------
  function Get_Joy_Axis_Index_From_String (Axis : in String) return Integer is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return 0;
  end;

  -------------------------
  -- Get_Joy_Axis_String --
  -------------------------
  function Get_Joy_Axis_String (Axis_Index : in Integer) return String is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return "";
  end;

  --------------------------------------
  -- Get_Joy_Button_Index_From_String --
  --------------------------------------
  function Get_Joy_Button_Index_From_String (Button : in String) return Integer is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return 0;
  end;

  ---------------------------
  -- Get_Joy_Button_String --
  ---------------------------
  function Get_Joy_Button_String (Button_Index : in Integer) return String is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return "";
  end;

  ------------------
  -- Get_Joy_Guid --
  ------------------
  function Get_Joy_Guid (Device : in Integer) return String is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return "";
  end;

  ------------------
  -- Get_Joy_Name --
  ------------------
  function Get_Joy_Name (Device : in Integer) return String is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return "";
  end;

  --------------------------------
  -- Get_Joy_Vibration_Duration --
  --------------------------------
  function Get_Joy_Vibration_Duration (Device : in Integer) return Long_Float is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return 0.0;
  end;

  --------------------------------
  -- Get_Joy_Vibration_Strength --
  --------------------------------
  function Get_Joy_Vibration_Strength (Device : in Integer) return Math.Vector2 is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Math.New_Vector2 (0.0, 0.0);
  end;

  --------------------------
  -- Get_Last_Mouse_Speed --
  --------------------------
  function Get_Last_Mouse_Speed return Math.Vector2 is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Math.New_Vector2 (0.0, 0.0);
  end;

  ----------------------
  -- Get_Magnetometer --
  ----------------------
  function Get_Magnetometer return Math.Vector3 is begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Math.New_Vector3 (0.0, 0.0, 0.0);
  end;

  ---------------------------
  -- Get_Mouse_Button_Mask --
  ---------------------------
  function Get_Mouse_Button_Mask return Integer is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return 0;
  end;

  --------------------
  -- Get_Mouse_Mode --
  --------------------
  function Get_Mouse_Mode return MouseMode is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return Null_MouseMode;
  end;

  ----------------------------
  -- Is_Action_Just_Pressed --
  ----------------------------
  function Is_Action_Just_Pressed (Action : in String) return Boolean is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return False;
  end;

  -----------------------------
  -- Is_Action_Just_Released --
  -----------------------------
  function Is_Action_Just_Released (Action : in String) return Boolean is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return False;
  end;

  -----------------------
  -- Is_Action_Pressed --
  -----------------------
  function Is_Action_Pressed (Action : in String) return Boolean is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return False;
  end;

  ---------------------------
  -- Is_Joy_Button_Pressed --
  ---------------------------
  function Is_Joy_Button_Pressed (Device : in Integer; Button : in Integer) return Boolean is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return False;
  end;

  ------------------
  -- Is_Joy_Known --
  ------------------
  function Is_Joy_Known (Device : in Integer) return Boolean is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return False;
  end;

  --------------------
  -- Is_Key_Pressed --
  --------------------
  function Is_Key_Pressed (Scancode : in Integer) return Boolean is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return False;
  end;

  -----------------------------
  -- Is_Mouse_Button_Pressed --
  -----------------------------
  function Is_Mouse_Button_Pressed (Button : in Integer) return Boolean is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
    return False;
  end;

  ----------------------------
  -- Joy_Connection_Changed --
  ----------------------------
  procedure Joy_Connection_Changed (Device : in Integer; Connected : in Boolean; Name : in String; Guid : in String) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  -----------------------
  -- Parse_Input_Event --
  -----------------------
  procedure Parse_Input_Event (Event : in InputEvent) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  ------------------------
  -- Remove_Joy_Mapping --
  ------------------------
  procedure Remove_Joy_Mapping (Guid : in String) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  -----------------------------
  -- Set_Custom_Mouse_Cursor --
  -----------------------------
  procedure Set_Custom_Mouse_Cursor (Image : in Resource; Shape : in CursorShape; Hotspot : in Math.Vector2) is -- CursorShape := 0; Math.Vector2 := Math.Vector2 (0, 0)
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  ------------------------------
  -- Set_Default_Cursor_Shape --
  ------------------------------
  procedure Set_Default_Cursor_Shape (Shape : in CursorShape) is -- CursorShape := 0
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  --------------------
  -- Set_Mouse_Mode --
  --------------------
  procedure Set_Mouse_Mode (Mode : in MouseMode) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  -------------------------------
  -- Set_Use_Accumulated_Input --
  -------------------------------
  procedure Set_Use_Accumulated_Input (Enable : in Boolean) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  -------------------------
  -- Start_Joy_Vibration --
  -------------------------
  procedure Start_Joy_Vibration (Device : in Integer; Weak_Magnitude : in Long_Float; Strong_Magnitude : in Long_Float; Duration : in Long_Float := 0.0) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  ------------------------
  -- Stop_Joy_Vibration --
  ------------------------
  procedure Stop_Joy_Vibration (Device : in Integer) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  ----------------------
  -- Vibrate_Handheld --
  ----------------------
  procedure Vibrate_Handheld (Duration_Ms : in Integer := 500) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

  -------------------------
  -- Warp_Mouse_Position --
  -------------------------
  procedure Warp_Mouse_Position (To : in Math.Vector2) is
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    raise Unimplemented_Feature;
  end;

end;