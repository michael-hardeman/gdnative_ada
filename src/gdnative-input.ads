with GDNative.Thin;

package GDNative.Input is

  type Input_Object is private;

  procedure Initialize;

  -- Stubs for types I need to implement
  type JoypadArray is null record;
  type CursorShape is null record;
  type Vector3 is null record;
  type Vector2 is null record;
  type MouseMode is null record;
  type Resource is null record;
  type InputEvent is null record;

  Null_JoypadArray : JoypadArray;
  Null_CursorShape : CursorShape;
  Null_Vector3     : Vector3;
  Null_Vector2     : Vector2;
  Null_MouseMode   : MouseMode;
  Null_Resource    : Resource;
  Null_InputEvent  : InputEvent;

  procedure Action_Press (Action : in String; Strength : in Long_Float := 1.0);
  procedure Action_Release (Action : in String);
  procedure Add_Joy_Mapping (Mapping : in String; Update_Existing : in Boolean := False);
  function  Get_Accelerometer return Vector3;
  function  Get_Action_Strength (Action : String) return Long_Float;
  function  Get_Connected_Joypads return JoypadArray;
  function  Get_Current_Cursor_Shape return CursorShape;
  function  Get_Gravity return Vector3;
  function  Get_Gyroscope return Vector3;
  function  Get_Joy_Axis (Device : Integer; Axis : Integer) return Long_Float;
  function  Get_Joy_Axis_Index_From_String (Axis : String) return Integer;
  function  Get_Joy_Axis_String (Axis_Index : Integer) return String;
  function  Get_Joy_Button_Index_From_String (Button : String) return Integer;
  function  Get_Joy_Button_String (Button_Index : Integer) return String;
  function  Get_Joy_Guid (Device : Integer) return String;
  function  Get_Joy_Name (Device : Integer) return String;
  function  Get_Joy_Vibration_Duration (Device : Integer) return Long_Float;
  function  Get_Joy_Vibration_Strength (Device : Integer) return Vector2;
  function  Get_Last_Mouse_Speed return Vector2;
  function  Get_Magnetometer return Vector3;
  function  Get_Mouse_Button_Mask return Integer;
  function  Get_Mouse_Mode return MouseMode;
  function  Is_Action_Just_Pressed (Action : String) return Boolean;
  function  Is_Action_Just_Released (Action : String) return Boolean;
  function  Is_Action_Pressed (Action : String) return Boolean;
  function  Is_Joy_Button_Pressed (Device : Integer; Button : Integer) return Boolean;
  function  Is_Joy_Known (Device : Integer) return Boolean;
  function  Is_Key_Pressed (Scancode : Integer) return Boolean;
  function  Is_Mouse_Button_Pressed (Button : Integer) return Boolean;
  procedure Joy_Connection_Changed (Device : Integer; Connected : Boolean; Name : String; Guid : String);
  procedure Parse_Input_Event (Event : InputEvent);
  procedure Remove_Joy_Mapping (Guid : String);
  procedure Set_Custom_Mouse_Cursor (Image : Resource; Shape : CursorShape; Hotspot : Vector2); -- CursorShape := 0; Vetor2 := Vector2 (0, 0)
  procedure Set_Default_Cursor_Shape (Shape : CursorShape); -- CursorShape := 0
  procedure Set_Mouse_Mode (Mode : MouseMode);
  procedure Set_Use_Accumulated_Input (Enable : Boolean);
  procedure Start_Joy_Vibration (Device : Integer; Weak_Magnitude : Long_Float; Strong_Magnitude : Long_Float; Duration : Long_Float := 0.0);
  procedure Stop_Joy_Vibration (Device : Integer);
  procedure Vibrate_Handheld (Duration_Ms : Integer := 500);
  procedure Warp_Mouse_Position (To : Vector2);

-------
private
-------

  type Input_Object is record
    Initialized                      : Boolean                       := False;
    Handle                           : Thin.Singleton_Handle         := Thin.NULL_SINGLETON_HANDLE;
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