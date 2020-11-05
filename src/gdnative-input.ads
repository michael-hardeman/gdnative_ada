with GDNative.Thin;
with GDNative.Math;

package GDNative.Input is

  procedure Initialize;
  procedure Finalize;

  -- TODO: This Stuff
  type JoypadArray is null record;
  type CursorShape is null record;
  type MouseMode is null record;
  type Resource is null record;
  type InputEvent is null record;

  Null_JoypadArray : JoypadArray;
  Null_CursorShape : CursorShape;
  Null_MouseMode   : MouseMode;
  Null_Resource    : Resource;
  Null_InputEvent  : InputEvent;

  procedure Action_Press (Action : in Wide_String; Strength : in Percentage := 1.0);
  procedure Action_Release (Action : in Wide_String);
  procedure Add_Joy_Mapping (Mapping : in Wide_String; Update_Existing : in Boolean := False);
  function  Get_Accelerometer return Math.Vector3;
  function  Get_Action_Strength (Action : in Wide_String) return Percentage;

  function  Get_Connected_Joypads return JoypadArray;
  function  Get_Current_Cursor_Shape return CursorShape;
  function  Get_Gravity return Math.Vector3;
  function  Get_Gyroscope return Math.Vector3;
  function  Get_Joy_Axis (Device : in Integer; Axis : in Integer) return Long_Float;
  function  Get_Joy_Axis_Index_From_String (Axis : in String) return Integer;
  function  Get_Joy_Axis_String (Axis_Index : in Integer) return String;
  function  Get_Joy_Button_Index_From_String (Button : in String) return Integer;
  function  Get_Joy_Button_String (Button_Index : in Integer) return String;
  function  Get_Joy_Guid (Device : in Integer) return String;
  function  Get_Joy_Name (Device : in Integer) return String;
  function  Get_Joy_Vibration_Duration (Device : in Integer) return Long_Float;
  function  Get_Joy_Vibration_Strength (Device : in Integer) return Math.Vector2;
  function  Get_Last_Mouse_Speed return Math.Vector2;
  function  Get_Magnetometer return Math.Vector3;
  function  Get_Mouse_Button_Mask return Integer;
  function  Get_Mouse_Mode return MouseMode;
  function  Is_Action_Just_Pressed (Action : in String) return Boolean;
  function  Is_Action_Just_Released (Action : in String) return Boolean;
  function  Is_Action_Pressed (Action : in String) return Boolean;
  function  Is_Joy_Button_Pressed (Device : in Integer; Button : in Integer) return Boolean;
  function  Is_Joy_Known (Device : in Integer) return Boolean;
  function  Is_Key_Pressed (Scancode : in Integer) return Boolean;
  function  Is_Mouse_Button_Pressed (Button : in Integer) return Boolean;
  procedure Joy_Connection_Changed (Device : in Integer; Connected : in Boolean; Name : in String; Guid : in String);
  procedure Parse_Input_Event (Event : in InputEvent);
  procedure Remove_Joy_Mapping (Guid : in String);
  procedure Set_Custom_Mouse_Cursor (Image : in Resource; Shape : in CursorShape; Hotspot : in Math.Vector2); -- CursorShape := 0; Vetor2 := Math.Vector2 (0, 0)
  procedure Set_Default_Cursor_Shape (Shape : in CursorShape); -- CursorShape := 0
  procedure Set_Mouse_Mode (Mode : in MouseMode);
  procedure Set_Use_Accumulated_Input (Enable : in Boolean);
  procedure Start_Joy_Vibration (Device : in Integer; Weak_Magnitude : in Long_Float; Strong_Magnitude : in Long_Float; Duration : in Long_Float := 0.0);

  procedure Stop_Joy_Vibration (Device : in Integer);
  procedure Vibrate_Handheld (Duration_Ms : in Integer := 500);
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