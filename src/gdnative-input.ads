with GDNative.Thin;
with GDNative.Math;
with GDNative.Strings;
with GDNative.Arrays;

package GDNative.Input is

  procedure Initialize;
  procedure Finalize;

  type JoypadArray is null record; -- TODO: JoypadArray
  type Resource is null record; -- TODO: Resource
  type InputEvent is null record; -- TODO: InputEvent

  Null_JoypadArray : JoypadArray; -- TODO: JoypadArray
  Null_Resource    : Resource; -- TODO: Resource
  Null_InputEvent  : InputEvent; -- TODO: InputEvent

  -- https://docs.godotengine.org/en/stable/classes/class_input.html#enum-input-cursorshape
  type Cursor_Shape_Kind is (
    Arrow_Shape,                  -- Standard, default pointing cursor.
    I_Beam_Shape,                 -- Usually used to show where the text cursor will appear when the mouse is clicked.
    Pointing_Hand_Shape,          -- Usually used to indicate the pointer is over a link or other interactable item.
    Cross_Shape,                  -- Typically appears over regions in which a drawing operation can be performed or for selections.
    Wait_Shape,                   -- Indicates that the application is busy performing an operation. This cursor shape denotes that the application is still usable during the operation.
    Busy_Shape,                   -- Indicates that the application is busy performing an operation. This cursor shape denotes that the application isn't usable during the operation (e.g. something is blocking its main thread).
    Drag_Shape,                   -- Usually displayed when dragging something.
    Can_Drop_Shape,               -- Usually displayed when dragging something to indicate that it can be dropped at the current position.
    Forbidden_Shape,              -- Indicates that the current action is forbidden (for example, when dragging something) or that the control at a position is disabled.
    Vertical_Size_Shape,          -- A double-headed vertical arrow. It tells the user they can resize the window or the panel vertically.
    Horizontal_Size_Shape,        -- A double-headed horizontal arrow. It tells the user they can resize the window or the panel horizontally.
    Backward_Diagonal_Size_Shape, -- The cursor is a double-headed arrow that goes from the bottom left to the top right. It tells the user they can resize the window or the panel both horizontally and vertically.
    Forward_Diagonal_Size_Shape,  -- The cursor is a double-headed arrow that goes from the top left to the bottom right, the opposite of CURSOR_BDIAGSIZE. It tells the user they can resize the window or the panel both horizontally and vertically.
    Move_Shape,                   -- Indicates that something can be moved.
    Vertical_Split_Shape,         -- On Windows, it's the same as CURSOR_VSIZE.
    Horizontal_Split_Shape,       -- On Windows, it's the same as CURSOR_HSIZE.
    Help_Shape);                  -- Usually a question mark.

  -- https://docs.godotengine.org/en/stable/classes/class_@globalscope.html#enum-globalscope-joysticklist
  type Joypad_Axis_Kind is (
    Gamepad_Left_Horizontal_Axis,
    Gamepad_Left_Vertical_Axis,
    Gamepad_Right_Horizontal_Axis,
    Gamepad_Right_Vertical_Axis,
    Generic_Gamepad_4_Axis,            -- ?
    Generic_Gamepad_5_Axis,            -- ?
    Gamepad_Left_Trigger_Analog_Axis,
    Gamepad_Right_Trigger_Analog_Axis,
    Generic_Gamepad_8_Axis,            -- ?
    Generic_Gamepad_9_Axis);           -- ?

  type Mouse_Mode_Kind is (
    Visible_Mode,   -- Makes the mouse cursor visible if it is hidden.
    Hidden_Mode,    -- Makes the mouse cursor hidden if it is visible.
    Captured_Mode); -- Captures the mouse. The mouse will be hidden and its position locked at the center of the screen

  procedure Action_Press (Action : in Wide_String; Strength : in Real_64_Percent := 1.0);
  procedure Action_Release (Action : in Wide_String);
  procedure Add_Joy_Mapping (Mapping : in Wide_String; Update_Existing : in Boolean := False);
  function  Get_Accelerometer return Math.Vector3;
  function  Get_Action_Strength (Action : in Wide_String) return Real_64_Percent;
  function  Get_Connected_Joypads return JoypadArray; -- TODO: JoypadArray
  function  Get_Current_Cursor_Shape return Cursor_Shape_Kind;
  function  Get_Gravity return Math.Vector3;
  function  Get_Gyroscope return Math.Vector3;
  function  Get_Joy_Axis (Device : in Int_64_Unsigned; Axis : in Joypad_Axis_Kind) return Real_64_Percent;
  function  Get_Joy_Axis_Index_From_String (Axis : in Wide_String) return Int_64_Unsigned; 
  function  Get_Joy_Axis_String (Axis_Index : in Int_64_Unsigned) return Wide_String;
  function  Get_Joy_Button_Index_From_String (Button : in Wide_String) return Int_64_Unsigned;
  function  Get_Joy_Button_String (Button_Index : in Int_64_Unsigned) return Wide_String;
  function  Get_Joy_Guid (Device : in Int_64_Unsigned) return Wide_String;
  function  Get_Joy_Name (Device : in Int_64_Unsigned) return Wide_String;
  function  Get_Joy_Vibration_Duration (Device : in Int_64_Unsigned) return Real_64;
  function  Get_Joy_Vibration_Strength (Device : in Int_64_Unsigned) return Math.Vector2;
  function  Get_Last_Mouse_Speed return Math.Vector2;
  function  Get_Magnetometer return Math.Vector3;
  function  Get_Mouse_Button_Mask return Int_64_Unsigned;
  function  Get_Mouse_Mode return Mouse_Mode_Kind;
  function  Is_Action_Just_Pressed (Action : in Wide_String) return Boolean;
  function  Is_Action_Just_Released (Action : in Wide_String) return Boolean;
  function  Is_Action_Pressed (Action : in Wide_String) return Boolean;
  function  Is_Joy_Button_Pressed (Device : in Int_64_Unsigned; Button : in Int_64_Unsigned) return Boolean;
  function  Is_Joy_Known (Device : in Int_64_Unsigned) return Boolean;
  function  Is_Key_Pressed (Scancode : in Int_64_Unsigned) return Boolean;
  function  Is_Mouse_Button_Pressed (Button : in Int_64_Unsigned) return Boolean;
  procedure Joy_Connection_Changed (Device : in Int_64_Unsigned; Connected : in Boolean; Name : in Wide_String; Guid : in Wide_String);
  procedure Parse_Input_Event (Event : in InputEvent); -- TODO: InputEvent
  procedure Remove_Joy_Mapping (Guid : in Wide_String);
  procedure Set_Custom_Mouse_Cursor (Image : in Resource; Shape : in Cursor_Shape_Kind := Arrow_Shape; Hotspot : in Math.Vector2 := Math.Zero); -- TODO: Resource
  procedure Set_Default_Cursor_Shape (Shape : in Cursor_Shape_Kind := Arrow_Shape);
  procedure Set_Mouse_Mode (Mode : in Mouse_Mode_Kind);
  procedure Set_Use_Accumulated_Input (Enable : in Boolean);
  procedure Start_Joy_Vibration (Device : in Int_64_Unsigned; Weak_Magnitude : in Real_64; Strong_Magnitude : in Real_64; Duration : in Real_64 := 0.0);
  procedure Stop_Joy_Vibration (Device : in Int_64_Unsigned);
  procedure Vibrate_Handheld (Duration_Ms : in Int_64_Unsigned := 500);
  procedure Warp_Mouse_Position (To : in Math.Vector2);

  INPUT_MULTIPLE_INITIALIZATION_ASSERT : constant String := "Multiple initializations of Input.";
  INPUT_UNINITIALIZED_ASSERT           : constant String := "Attempting to use without initializing Input.";
  INPUT_EARLY_FINALIZE_ASSERT          : constant String := "Finalizing without initializing Input.";

-------
private
-------

  type Input_Object is record
    Initialized                      : Boolean                    := False;
    Handle                           : Thin.godot_object          := Thin.Null_Godot_Object;
    Action_Press                     : Thin.godot_method_bind_ptr := null;
    Action_Release                   : Thin.godot_method_bind_ptr := null;
    Add_Joy_Mapping                  : Thin.godot_method_bind_ptr := null;
    Get_Accelerometer                : Thin.godot_method_bind_ptr := null;
    Get_Action_Strength              : Thin.godot_method_bind_ptr := null;
    Get_Connected_Joypads            : Thin.godot_method_bind_ptr := null;
    Get_Current_Cursor_Shape         : Thin.godot_method_bind_ptr := null;
    Get_Gravity                      : Thin.godot_method_bind_ptr := null;
    Get_Gyroscope                    : Thin.godot_method_bind_ptr := null;
    Get_Joy_Axis                     : Thin.godot_method_bind_ptr := null;
    Get_Joy_Axis_Index_From_String   : Thin.godot_method_bind_ptr := null;
    Get_Joy_Axis_String              : Thin.godot_method_bind_ptr := null;
    Get_Joy_Button_Index_From_String : Thin.godot_method_bind_ptr := null;
    Get_Joy_Button_String            : Thin.godot_method_bind_ptr := null;
    Get_Joy_Guid                     : Thin.godot_method_bind_ptr := null;
    Get_Joy_Name                     : Thin.godot_method_bind_ptr := null;
    Get_Joy_Vibration_Duration       : Thin.godot_method_bind_ptr := null;
    Get_Joy_Vibration_Strength       : Thin.godot_method_bind_ptr := null;
    Get_Last_Mouse_Speed             : Thin.godot_method_bind_ptr := null;
    Get_Magnetometer                 : Thin.godot_method_bind_ptr := null;
    Get_Mouse_Button_Mask            : Thin.godot_method_bind_ptr := null;
    Get_Mouse_Mode                   : Thin.godot_method_bind_ptr := null;
    Is_Action_Just_Pressed           : Thin.godot_method_bind_ptr := null;
    Is_Action_Just_Released          : Thin.godot_method_bind_ptr := null;
    Is_Action_Pressed                : Thin.godot_method_bind_ptr := null;
    Is_Joy_Button_Pressed            : Thin.godot_method_bind_ptr := null;
    Is_Joy_Known                     : Thin.godot_method_bind_ptr := null;
    Is_Key_Pressed                   : Thin.godot_method_bind_ptr := null;
    Is_Mouse_Button_Pressed          : Thin.godot_method_bind_ptr := null;
    Joy_Connection_Changed           : Thin.godot_method_bind_ptr := null;
    Parse_Input_Event                : Thin.godot_method_bind_ptr := null;
    Remove_Joy_Mapping               : Thin.godot_method_bind_ptr := null;
    Set_Custom_Mouse_Cursor          : Thin.godot_method_bind_ptr := null;
    Set_Default_Cursor_Shape         : Thin.godot_method_bind_ptr := null;
    Set_Mouse_Mode                   : Thin.godot_method_bind_ptr := null;
    Set_Use_Accumulated_Input        : Thin.godot_method_bind_ptr := null;
    Start_Joy_Vibration              : Thin.godot_method_bind_ptr := null;
    Stop_Joy_Vibration               : Thin.godot_method_bind_ptr := null;
    Vibrate_Handheld                 : Thin.godot_method_bind_ptr := null;
    Warp_Mouse_Position              : Thin.godot_method_bind_ptr := null;
  end record;

  Input_Singleton : Input_Object;

end;