with Interfaces.C.Strings;

with GDNative.Context;
with GDNative.Console;
with GDNative.Exceptions;
with GDNative.Variants;
with GDNative.Input_Map;

package body GDNative.Input is

  use GDNative.Thin;
  use GDNative.Variants;
  
  package ICS renames Interfaces.C.Strings;

  ----------------------
  -- Get_Input_Method --
  ----------------------
  function Get_Input_Method (Input_Class_Name : in ICS.chars_ptr; Method_Name : in String) return godot_method_bind_ptr is
    C_Method_Name : ICS.chars_ptr := ICS.New_String (Method_Name);
    Result        : godot_method_bind_ptr;
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
      Input_Singleton.Handle, 
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

  --------------------------
  -- Ensure Action Exists --
  --------------------------
  procedure Ensure_Action_Exists (Action : in Wide_String) is begin
    if Input_Map.Has_Action (Action) then return; end if;
    Exceptions.Put_Warning (Input_Map.INPUT_MAP_ACTION_DOES_NOT_EXIST & Action);
  end;

  ------------------
  -- Action Press --
  ------------------
  procedure Action_Press (Action : in Wide_String; Strength : in Real_64_Percent := 1.0) is
    Action_Variant   : Variant;
    Strength_Variant : Variant;
    Arguments        : aliased godot_variant_const_ptr_array (1 .. 2);
  begin
    Ensure_Action_Exists (Action);
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant,   Action);
    Initialize (Strength_Variant, Strength);
    Arguments := (Ref (Action_Variant), Ref (Strength_Variant));
    Writer_Bind_Call (Input_Singleton.Action_Press, Arguments (1)'unchecked_access, Arguments'length);
  end;

  --------------------
  -- Action_Release --
  --------------------
  procedure Action_Release (Action : in Wide_String) is
    Action_Variant   : Variant;
    Arguments        : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    Ensure_Action_Exists (Action);
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Writer_Bind_Call (Input_Singleton.Action_Release, Arguments (1)'unchecked_access, Arguments'length);
  end;

  ---------------------
  -- Add_Joy_Mapping --
  ---------------------
  -- Mapping is an SDL2 gamepad mapping string
  -- Get one from this tool: https://generalarcade.com/gamepadtool/
  -- Todo: create a package for generating these strings
  procedure Add_Joy_Mapping (Mapping : in Wide_String; Update_Existing : in Boolean := False) is
    Mapping_Variant : Variant;
    Update_Variant  : Variant;
    Arguments       : aliased godot_variant_const_ptr_array (1 .. 2);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Mapping_Variant, Mapping);
    Initialize (Update_Variant,  Update_Existing);
    Arguments := (Ref (Mapping_Variant), Ref (Update_Variant));
    Writer_Bind_Call (Input_Singleton.Add_Joy_Mapping, Arguments (1)'unchecked_access, Arguments'length);
  end;

  -----------------------
  -- Get_Accelerometer --
  -----------------------
  function Get_Accelerometer return Math.Vector3 is 
    Result : Variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Accelerometer, Result);
    return Value (Result);
  end;

  -------------------------
  -- Get_Action_Strength --
  -------------------------
  function Get_Action_Strength (Action : in Wide_String) return Real_64_Percent is
    Action_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    Ensure_Action_Exists (Action);
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Method_Bind_Call (Input_Singleton.Get_Action_Strength, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
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
    Result : Variant;
    Temp   : Int_64_Unsigned;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Current_Cursor_Shape, Result);
    Temp := Value (Result);
    return Cursor_Shape_Kind'Val (Temp);
  end;

  -----------------
  -- Get_Gravity --
  -----------------
  function Get_Gravity return Math.Vector3 is
    Result : Variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Gravity, Result);
    return Value (Result);
  end;

  -------------------
  -- Get_Gyroscope --
  -------------------
  function Get_Gyroscope return Math.Vector3 is
    Result : Variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Gyroscope, Result);
    return Value (Result);
  end;

  ------------------
  -- Get_Joy_Axis --
  ------------------
  function Get_Joy_Axis (Device : in Int_64_Unsigned; Axis : in Joypad_Axis_Kind) return Real_64_Percent is
    Device_Variant : Variant;
    Axis_Variant   : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 2);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Initialize (Axis_Variant,   Int_64_Unsigned (Joypad_Axis_Kind'Pos (Axis)));
    Arguments := (Ref (Device_Variant), Ref (Axis_Variant));
    Method_Bind_Call (Input_Singleton.Get_Joy_Axis, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  ------------------------------------
  -- Get_Joy_Axis_Index_From_String --
  ------------------------------------
  function Get_Joy_Axis_Index_From_String (Axis : in Wide_String) return Int_64_Unsigned is
    Axis_Variant : Variant;
    Result       : Variant;
    Arguments    : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Axis_Variant, Axis);
    Arguments (1) := Ref (Axis_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Axis_Index_From_String, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  -------------------------
  -- Get_Joy_Axis_String --
  -------------------------
  function Get_Joy_Axis_String (Axis_Index : in Int_64_Unsigned) return Wide_String is
    Axis_Variant : Variant;
    Result       : Variant;
    Arguments    : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Axis_Variant, Axis_Index);
    Arguments (1) := Ref (Axis_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Axis_String, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  --------------------------------------
  -- Get_Joy_Button_Index_From_String --
  --------------------------------------
  function Get_Joy_Button_Index_From_String (Button : in Wide_String) return Int_64_Unsigned is 
    Button_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Button_Variant, Button);
    Arguments (1) := Ref (Button_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Button_Index_From_String, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  ---------------------------
  -- Get_Joy_Button_String --
  ---------------------------
  function Get_Joy_Button_String (Button_Index : in Int_64_Unsigned) return Wide_String is
    Button_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Button_Variant, Button_Index);
    Arguments (1) := Ref (Button_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Button_String, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  ------------------
  -- Get_Joy_Guid --
  ------------------
  function Get_Joy_Guid (Device : in Int_64_Unsigned) return Wide_String is
    Device_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Arguments (1) := Ref (Device_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Guid, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  ------------------
  -- Get_Joy_Name --
  ------------------
  function Get_Joy_Name (Device : in Int_64_Unsigned) return Wide_String is
    Device_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Arguments (1) := Ref (Device_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Name, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  --------------------------------
  -- Get_Joy_Vibration_Duration --
  --------------------------------
  function Get_Joy_Vibration_Duration (Device : in Int_64_Unsigned) return Real_64 is
    Device_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Arguments (1) := Ref (Device_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Vibration_Duration, Arguments (1)'unchecked_access, Arguments'length, Result);
    return Value (Result);
  end;

  --------------------------------
  -- Get_Joy_Vibration_Strength --
  --------------------------------
  function Get_Joy_Vibration_Strength (Device : in Int_64_Unsigned) return Math.Vector2 is
    Device_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Arguments (1) := Ref (Device_Variant);
    Method_Bind_Call (Input_Singleton.Get_Joy_Vibration_Strength, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  --------------------------
  -- Get_Last_Mouse_Speed --
  --------------------------
  function Get_Last_Mouse_Speed return Math.Vector2 is
    Result : Variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Last_Mouse_Speed, Result);
    return Value (Result);
  end;

  ----------------------
  -- Get_Magnetometer --
  ----------------------
  function Get_Magnetometer return Math.Vector3 is
    Result : Variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Magnetometer, Result);
    return Value (Result);
  end;

  ---------------------------
  -- Get_Mouse_Button_Mask --
  ---------------------------
  function Get_Mouse_Button_Mask return Int_64_Unsigned is
    Result : Variant;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Mouse_Button_Mask, Result);
    return Value (Result);
  end;

  --------------------
  -- Get_Mouse_Mode --
  --------------------
  function Get_Mouse_Mode return Mouse_Mode_Kind is
    Result : Variant;
    Temp   : Int_64_Unsigned;
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Reader_Bind_Call (Input_Singleton.Get_Mouse_Button_Mask, Result);
    Temp := Value (Result);
    return Mouse_Mode_Kind'Val (Temp);
  end;

  ----------------------------
  -- Is_Action_Just_Pressed --
  ----------------------------
  function Is_Action_Just_Pressed (Action : in Wide_String) return Boolean is
    Action_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    Ensure_Action_Exists (Action);
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Method_Bind_Call (Input_Singleton.Is_Action_Just_Pressed, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  -----------------------------
  -- Is_Action_Just_Released --
  -----------------------------
  function Is_Action_Just_Released (Action : in Wide_String) return Boolean is
    Action_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    Ensure_Action_Exists (Action);
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Method_Bind_Call (Input_Singleton.Is_Action_Just_Released, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  -----------------------
  -- Is_Action_Pressed --
  -----------------------
  function Is_Action_Pressed (Action : in Wide_String) return Boolean is
    Action_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    Ensure_Action_Exists (Action);
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Action_Variant, Action);
    Arguments (1) := Ref (Action_Variant);
    Method_Bind_Call (Input_Singleton.Is_Action_Pressed, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  ---------------------------
  -- Is_Joy_Button_Pressed --
  ---------------------------
  function Is_Joy_Button_Pressed (Device : in Int_64_Unsigned; Button : in Int_64_Unsigned) return Boolean is
    Device_Variant : Variant;
    Button_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 2);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Initialize (Button_Variant, Button);
    Arguments := (Ref (Device_Variant), Ref (Button_Variant));
    Method_Bind_Call (Input_Singleton.Is_Joy_Button_Pressed, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  ------------------
  -- Is_Joy_Known --
  ------------------
  function Is_Joy_Known (Device : in Int_64_Unsigned) return Boolean is
    Device_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Arguments (1) := Ref (Device_Variant);
    Method_Bind_Call (Input_Singleton.Is_Joy_Known, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  --------------------
  -- Is_Key_Pressed --
  --------------------
  function Is_Key_Pressed (Scancode : in Int_64_Unsigned) return Boolean is
    Scancode_Variant : Variant;
    Result           : Variant;
    Arguments        : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Scancode_Variant, Scancode);
    Arguments (1) := Ref (Scancode_Variant);
    Method_Bind_Call (Input_Singleton.Is_Key_Pressed, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  -----------------------------
  -- Is_Mouse_Button_Pressed --
  -----------------------------
  function Is_Mouse_Button_Pressed (Button : in Int_64_Unsigned) return Boolean is
    Button_Variant : Variant;
    Result         : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Button_Variant, Button);
    Arguments (1) := Ref (Button_Variant);
    Method_Bind_Call (Input_Singleton.Is_Mouse_Button_Pressed, Arguments (1)'unchecked_access, Arguments'Length, Result);
    return Value (Result);
  end;

  ----------------------------
  -- Joy_Connection_Changed --
  ----------------------------
  procedure Joy_Connection_Changed (Device : in Int_64_Unsigned; Connected : in Boolean; Name : in Wide_String; Guid : in Wide_String) is
    Device_Variant    : Variant;
    Connected_Variant : Variant;
    Name_Variant      : Variant;
    Guid_Variant      : Variant;
    Arguments         : aliased godot_variant_const_ptr_array (1 .. 4);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Initialize (Connected_Variant, Connected);
    Initialize (Name_Variant, Name);
    Initialize (Guid_Variant, Guid);
    Arguments := (Ref (Device_Variant), Ref (Connected_Variant), Ref (Name_Variant), Ref (Guid_Variant));
    Writer_Bind_Call (Input_Singleton.Joy_Connection_Changed, Arguments (1)'unchecked_access, Arguments'Length);
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
    Guid_Variant : Variant;
    Arguments    : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Guid_Variant, Guid);
    Arguments (1) := Ref (Guid_Variant);
    Writer_Bind_Call (Input_Singleton.Remove_Joy_Mapping, Arguments (1)'unchecked_access, Arguments'Length);
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
    Shape_Variant : Variant;
    Arguments     : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Shape_Variant, Int_64_Unsigned (Cursor_Shape_Kind'Pos (Shape)));
    Arguments (1) := Ref (Shape_Variant);
    Writer_Bind_Call (Input_Singleton.Set_Default_Cursor_Shape, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  --------------------
  -- Set_Mouse_Mode --
  --------------------
  procedure Set_Mouse_Mode (Mode : in Mouse_Mode_Kind) is
    Mode_Variant : Variant;
    Arguments    : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Mode_Variant, Int_64_Unsigned (Mouse_Mode_Kind'Pos (Mode)));
    Arguments (1) := Ref (Mode_Variant);
    Writer_Bind_Call (Input_Singleton.Set_Mouse_Mode, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  -------------------------------
  -- Set_Use_Accumulated_Input --
  -------------------------------
  procedure Set_Use_Accumulated_Input (Enable : in Boolean) is
    Enable_Variant : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Enable_Variant, Enable);
    Arguments (1) := Ref (Enable_Variant);
    Writer_Bind_Call (Input_Singleton.Set_Use_Accumulated_Input, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  -------------------------
  -- Start_Joy_Vibration --
  -------------------------
  procedure Start_Joy_Vibration (Device : in Int_64_Unsigned; Weak_Magnitude : in Real_64; Strong_Magnitude : in Real_64; Duration : in Real_64 := 0.0) is
    Device_Variant   : Variant;
    Weak_Variant     : Variant;
    Strong_Variant   : Variant;
    Duration_Variant : Variant;
    Arguments        : aliased godot_variant_const_ptr_array (1 .. 4);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant,   Device);
    Initialize (Weak_Variant,     Weak_Magnitude);
    Initialize (Strong_Variant,   Strong_Magnitude);
    Initialize (Duration_Variant, Duration);
    Arguments := (Ref (Device_Variant), Ref (Weak_Variant), Ref (Strong_Variant), Ref (Duration_Variant));
    Writer_Bind_Call (Input_Singleton.Start_Joy_Vibration, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  ------------------------
  -- Stop_Joy_Vibration --
  ------------------------
  procedure Stop_Joy_Vibration (Device : in Int_64_Unsigned) is
    Device_Variant : Variant;
    Arguments      : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Device_Variant, Device);
    Arguments (1) := Ref (Device_Variant);
    Writer_Bind_Call (Input_Singleton.Stop_Joy_Vibration, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  ----------------------
  -- Vibrate_Handheld --
  ----------------------
  procedure Vibrate_Handheld (Duration_Ms : in Int_64_Unsigned := 500) is
    Duration_Variant : Variant;
    Arguments        : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (Duration_Variant, Duration_Ms);
    Arguments (1) := Ref (Duration_Variant);
    Writer_Bind_Call (Input_Singleton.Vibrate_Handheld, Arguments (1)'unchecked_access, Arguments'Length);
  end;

  -------------------------
  -- Warp_Mouse_Position --
  -------------------------
  procedure Warp_Mouse_Position (To : in Math.Vector2) is
    To_Variant : Variant;
    Arguments  : aliased godot_variant_const_ptr_array (1 .. 1);
  begin
    pragma Assert (Context.Core_Initialized,    CORE_UNINITIALIZED_ASSERT);
    pragma Assert (Input_Singleton.Initialized, INPUT_UNINITIALIZED_ASSERT);
    Initialize (To_Variant, To);
    Arguments (1) := Ref (To_Variant);
    Writer_Bind_Call (Input_Singleton.Warp_Mouse_Position, Arguments (1)'unchecked_access, Arguments'Length);
  end;
end;
