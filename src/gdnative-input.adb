with Interfaces.C;
with Interfaces.C.Strings;

with GDNative.Thin;
with GDNative.Context;

package body GDNative.Input is

  package ICS renames Interfaces.C.Strings;

  ----------------------
  -- Get_Input_Method --
  ----------------------
  function Get_Input_Method (Input_Class_Name : ICS.chars_ptr; Method_Name : in String) return access Thin.godot_method_bind is
    C_Method_Name : ICS.chars_ptr := ICS.New_String (Method_Name);
    Result : access Thin.godot_method_bind;
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
    ICS.Free (Input_Class_Name);
  end;

  ------------------
  -- Action_Press --
  ------------------
  procedure Action_Press (Action : in String; Strength : in Long_Float := 1.0) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  --------------------
  -- Action_Release --
  --------------------
  procedure Action_Release (Action : in String) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  ---------------------
  -- Add_Joy_Mapping --
  ---------------------
  procedure Add_Joy_Mapping (Mapping : in String; Update_Existing : in Boolean := False) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  -----------------------
  -- Get_Accelerometer --
  -----------------------
  function Get_Accelerometer return Vector3 is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_Vector3;
  end;

  -------------------------
  -- Get_Action_Strength --
  -------------------------
  function Get_Action_Strength (Action : String) return Long_Float is
  begin
    raise Program_Error with "Unimplemented Feature";
    return 0.0;
  end;

  ---------------------------
  -- Get_Connected_Joypads --
  ---------------------------
  function Get_Connected_Joypads return JoypadArray is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_JoypadArray;
  end;

  ------------------------------
  -- Get_Current_Cursor_Shape --
  ------------------------------
  function Get_Current_Cursor_Shape return CursorShape is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_CursorShape;
  end;

  -----------------
  -- Get_Gravity --
  -----------------
  function Get_Gravity return Vector3 is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_Vector3;
  end;

  -------------------
  -- Get_Gyroscope --
  -------------------
  function Get_Gyroscope return Vector3 is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_Vector3;
  end;

  ------------------
  -- Get_Joy_Axis --
  ------------------
  function Get_Joy_Axis (Device : Integer; Axis : Integer) return Long_Float is
  begin
    raise Program_Error with "Unimplemented Feature";
    return 0.0;
  end;

  ------------------------------------
  -- Get_Joy_Axis_Index_From_String --
  ------------------------------------
  function Get_Joy_Axis_Index_From_String (Axis : String) return Integer is
  begin
    raise Program_Error with "Unimplemented Feature";
    return 0;
  end;

  -------------------------
  -- Get_Joy_Axis_String --
  -------------------------
  function Get_Joy_Axis_String (Axis_Index : Integer) return String is
  begin
    raise Program_Error with "Unimplemented Feature";
    return "";
  end;

  --------------------------------------
  -- Get_Joy_Button_Index_From_String --
  --------------------------------------
  function Get_Joy_Button_Index_From_String (Button : String) return Integer is
  begin
    raise Program_Error with "Unimplemented Feature";
    return 0;
  end;

  ---------------------------
  -- Get_Joy_Button_String --
  ---------------------------
  function Get_Joy_Button_String (Button_Index : Integer) return String is
  begin
    raise Program_Error with "Unimplemented Feature";
    return "";
  end;

  ------------------
  -- Get_Joy_Guid --
  ------------------
  function Get_Joy_Guid (Device : Integer) return String is
  begin
    raise Program_Error with "Unimplemented Feature";
    return "";
  end;

  ------------------
  -- Get_Joy_Name --
  ------------------
  function Get_Joy_Name (Device : Integer) return String is
  begin
    raise Program_Error with "Unimplemented Feature";
    return "";
  end;

  --------------------------------
  -- Get_Joy_Vibration_Duration --
  --------------------------------
  function Get_Joy_Vibration_Duration (Device : Integer) return Long_Float is
  begin
    raise Program_Error with "Unimplemented Feature";
    return 0.0;
  end;

  --------------------------------
  -- Get_Joy_Vibration_Strength --
  --------------------------------
  function Get_Joy_Vibration_Strength (Device : Integer) return Vector2 is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_Vector2;
  end;

  --------------------------
  -- Get_Last_Mouse_Speed --
  --------------------------
  function Get_Last_Mouse_Speed return Vector2 is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_Vector2;
  end;

  ----------------------
  -- Get_Magnetometer --
  ----------------------
  function Get_Magnetometer return Vector3 is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_Vector3;
  end;

  ---------------------------
  -- Get_Mouse_Button_Mask --
  ---------------------------
  function Get_Mouse_Button_Mask return Integer is
  begin
    raise Program_Error with "Unimplemented Feature";
    return 0;
  end;

  --------------------
  -- Get_Mouse_Mode --
  --------------------
  function Get_Mouse_Mode return MouseMode is
  begin
    raise Program_Error with "Unimplemented Feature";
    return Null_MouseMode;
  end;

  ----------------------------
  -- Is_Action_Just_Pressed --
  ----------------------------
  function Is_Action_Just_Pressed (Action : String) return Boolean is
  begin
    raise Program_Error with "Unimplemented Feature";
    return False;
  end;

  -----------------------------
  -- Is_Action_Just_Released --
  -----------------------------
  function Is_Action_Just_Released (Action : String) return Boolean is
  begin
    raise Program_Error with "Unimplemented Feature";
    return False;
  end;

  -----------------------
  -- Is_Action_Pressed --
  -----------------------
  function Is_Action_Pressed (Action : String) return Boolean is
  begin
    raise Program_Error with "Unimplemented Feature";
    return False;
  end;

  ---------------------------
  -- Is_Joy_Button_Pressed --
  ---------------------------
  function Is_Joy_Button_Pressed (Device : Integer; Button : Integer) return Boolean is
  begin
    raise Program_Error with "Unimplemented Feature";
    return False;
  end;

  ------------------
  -- Is_Joy_Known --
  ------------------
  function Is_Joy_Known (Device : Integer) return Boolean is
  begin
    raise Program_Error with "Unimplemented Feature";
    return False;
  end;

  --------------------
  -- Is_Key_Pressed --
  --------------------
  function Is_Key_Pressed (Scancode : Integer) return Boolean is
  begin
    raise Program_Error with "Unimplemented Feature";
    return False;
  end;

  -----------------------------
  -- Is_Mouse_Button_Pressed --
  -----------------------------
  function Is_Mouse_Button_Pressed (Button : Integer) return Boolean is
  begin
    raise Program_Error with "Unimplemented Feature";
    return False;
  end;

  ----------------------------
  -- Joy_Connection_Changed --
  ----------------------------
  procedure Joy_Connection_Changed (Device : Integer; Connected : Boolean; Name : String; Guid : String) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  -----------------------
  -- Parse_Input_Event --
  -----------------------
  procedure Parse_Input_Event (Event : InputEvent) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  ------------------------
  -- Remove_Joy_Mapping --
  ------------------------
  procedure Remove_Joy_Mapping (Guid : String) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  -----------------------------
  -- Set_Custom_Mouse_Cursor --
  -----------------------------
  procedure Set_Custom_Mouse_Cursor (Image : Resource; Shape : CursorShape; Hotspot : Vector2) is -- CursorShape := 0; Vector2 := Vector2 (0, 0)
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  ------------------------------
  -- Set_Default_Cursor_Shape --
  ------------------------------
  procedure Set_Default_Cursor_Shape (Shape : CursorShape) is -- CursorShape := 0
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  --------------------
  -- Set_Mouse_Mode --
  --------------------
  procedure Set_Mouse_Mode (Mode : MouseMode) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  -------------------------------
  -- Set_Use_Accumulated_Input --
  -------------------------------
  procedure Set_Use_Accumulated_Input (Enable : Boolean) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  -------------------------
  -- Start_Joy_Vibration --
  -------------------------
  procedure Start_Joy_Vibration (Device : Integer; Weak_Magnitude : Long_Float; Strong_Magnitude : Long_Float; Duration : Long_Float := 0.0) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  ------------------------
  -- Stop_Joy_Vibration --
  ------------------------
  procedure Stop_Joy_Vibration (Device : Integer) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  ----------------------
  -- Vibrate_Handheld --
  ----------------------
  procedure Vibrate_Handheld (Duration_Ms : Integer := 500) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

  -------------------------
  -- Warp_Mouse_Position --
  -------------------------
  procedure Warp_Mouse_Position (To : Vector2) is
  begin
    raise Program_Error with "Unimplemented Feature";
  end;

end;