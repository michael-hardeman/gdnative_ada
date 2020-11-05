with GDNative.Thin;
with GDNative.Math;

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

  procedure Action_Press (Action : in Wide_String; Strength : in Percentage := 1.0);
  procedure Action_Release (Action : in Wide_String);
  procedure Add_Joy_Mapping (Mapping : in Wide_String; Update_Existing : in Boolean := False);
  function  Get_Accelerometer return Math.Vector3;
  function  Get_Action_Strength (Action : in Wide_String) return Percentage;
  function  Get_Connected_Joypads return JoypadArray; -- TODO: JoypadArray
  function  Get_Current_Cursor_Shape return Cursor_Shape_Kind;
  function  Get_Gravity return Math.Vector3;
  function  Get_Gyroscope return Math.Vector3;
  function  Get_Joy_Axis (Device : in Long_Integer; Axis : in Joypad_Axis_Kind) return Percentage;
  function  Get_Joy_Axis_Index_From_String (Axis : in Wide_String) return Long_Integer; 
  function  Get_Joy_Axis_String (Axis_Index : in Long_Integer) return Wide_String;
  function  Get_Joy_Button_Index_From_String (Button : in Wide_String) return Long_Integer;
  function  Get_Joy_Button_String (Button_Index : in Long_Integer) return Wide_String;
  function  Get_Joy_Guid (Device : in Long_Integer) return Wide_String;
  function  Get_Joy_Name (Device : in Long_Integer) return Wide_String;
  function  Get_Joy_Vibration_Duration (Device : in Long_Integer) return Long_Float;
  function  Get_Joy_Vibration_Strength (Device : in Long_Integer) return Math.Vector2;
  function  Get_Last_Mouse_Speed return Math.Vector2;
  function  Get_Magnetometer return Math.Vector3;
  function  Get_Mouse_Button_Mask return Long_Integer;
  function  Get_Mouse_Mode return Mouse_Mode_Kind;
  function  Is_Action_Just_Pressed (Action : in Wide_String) return Boolean;
  function  Is_Action_Just_Released (Action : in Wide_String) return Boolean;
  function  Is_Action_Pressed (Action : in Wide_String) return Boolean;
  function  Is_Joy_Button_Pressed (Device : in Long_Integer; Button : in Long_Integer) return Boolean;
  function  Is_Joy_Known (Device : in Long_Integer) return Boolean;
  function  Is_Key_Pressed (Scancode : in Long_Integer) return Boolean;
  function  Is_Mouse_Button_Pressed (Button : in Long_Integer) return Boolean;
  procedure Joy_Connection_Changed (Device : in Long_Integer; Connected : in Boolean; Name : in Wide_String; Guid : in Wide_String);
  procedure Parse_Input_Event (Event : in InputEvent); -- TODO: InputEvent
  procedure Remove_Joy_Mapping (Guid : in Wide_String);
  procedure Set_Custom_Mouse_Cursor (Image : in Resource; Shape : in Cursor_Shape_Kind := Arrow_Shape; Hotspot : in Math.Vector2 := Math.Zero); -- TODO: Resource
  procedure Set_Default_Cursor_Shape (Shape : in Cursor_Shape_Kind := Arrow_Shape);
  procedure Set_Mouse_Mode (Mode : in Mouse_Mode_Kind);
  procedure Set_Use_Accumulated_Input (Enable : in Boolean);
  procedure Start_Joy_Vibration (Device : in Integer; Weak_Magnitude : in Long_Float; Strong_Magnitude : in Long_Float; Duration : in Long_Float := 0.0);
  procedure Stop_Joy_Vibration (Device : in Long_Integer);
  procedure Vibrate_Handheld (Duration_Ms : in Long_Integer := 500);
  procedure Warp_Mouse_Position (To : in Math.Vector2);

  INPUT_MULTIPLE_INITIALIZATION_ASSERT : constant String := "Multiple initializations of Input.";
  INPUT_UNINITIALIZED_ASSERT           : constant String := "Attempting to use without initializing Input.";
  INPUT_EARLY_FINALIZE_ASSERT          : constant String := "Finalizing without initializing Input.";

-------
private
-------

  type Input_Object is record
    Initialized                      : Boolean                       := False;
    Handle                           : Thin.godot_object             := Thin.Null_Godot_Object;
    Action_Press                     : access Thin.godot_method_bind := null;
    Action_Release                   : access Thin.godot_method_bind := null;
    Add_Joy_Mapping                  : access Thin.godot_method_bind := null;
    Get_Accelerometer                : access Thin.godot_method_bind := null;
    Get_Action_Strength              : access Thin.godot_method_bind := null;
    Get_Connected_Joypads            : access Thin.godot_method_bind := null;
    Get_Current_Cursor_Shape         : access Thin.godot_method_bind := null;
    Get_Gravity                      : access Thin.godot_method_bind := null;
    Get_Gyroscope                    : access Thin.godot_method_bind := null;
    Get_Joy_Axis                     : access Thin.godot_method_bind := null;
    Get_Joy_Axis_Index_From_String   : access Thin.godot_method_bind := null;
    Get_Joy_Axis_String              : access Thin.godot_method_bind := null;
    Get_Joy_Button_Index_From_String : access Thin.godot_method_bind := null;
    Get_Joy_Button_String            : access Thin.godot_method_bind := null;
    Get_Joy_Guid                     : access Thin.godot_method_bind := null;
    Get_Joy_Name                     : access Thin.godot_method_bind := null;
    Get_Joy_Vibration_Duration       : access Thin.godot_method_bind := null;
    Get_Joy_Vibration_Strength       : access Thin.godot_method_bind := null;
    Get_Last_Mouse_Speed             : access Thin.godot_method_bind := null;
    Get_Magnetometer                 : access Thin.godot_method_bind := null;
    Get_Mouse_Button_Mask            : access Thin.godot_method_bind := null;
    Get_Mouse_Mode                   : access Thin.godot_method_bind := null;
    Is_Action_Just_Pressed           : access Thin.godot_method_bind := null;
    Is_Action_Just_Released          : access Thin.godot_method_bind := null;
    Is_Action_Pressed                : access Thin.godot_method_bind := null;
    Is_Joy_Button_Pressed            : access Thin.godot_method_bind := null;
    Is_Joy_Known                     : access Thin.godot_method_bind := null;
    Is_Key_Pressed                   : access Thin.godot_method_bind := null;
    Is_Mouse_Button_Pressed          : access Thin.godot_method_bind := null;
    Joy_Connection_Changed           : access Thin.godot_method_bind := null;
    Parse_Input_Event                : access Thin.godot_method_bind := null;
    Remove_Joy_Mapping               : access Thin.godot_method_bind := null;
    Set_Custom_Mouse_Cursor          : access Thin.godot_method_bind := null;
    Set_Default_Cursor_Shape         : access Thin.godot_method_bind := null;
    Set_Mouse_Mode                   : access Thin.godot_method_bind := null;
    Set_Use_Accumulated_Input        : access Thin.godot_method_bind := null;
    Start_Joy_Vibration              : access Thin.godot_method_bind := null;
    Stop_Joy_Vibration               : access Thin.godot_method_bind := null;
    Vibrate_Handheld                 : access Thin.godot_method_bind := null;
    Warp_Mouse_Position              : access Thin.godot_method_bind := null;
  end record;

  Input_Singleton : Input_Object;

end;