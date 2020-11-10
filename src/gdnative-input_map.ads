with GDNative.Thin;
with GDNative.Arrays;

package GDNative.Input_Map is
  
  procedure Initialize;
  procedure Finalize;

  type InputEvent is null record; -- TODO: InputEvent

  procedure Action_Add_Event    (Action : in Wide_String; Event : in InputEvent); -- TODO: InputEvent
  procedure Action_Erase_Event  (Action : in Wide_String; Event : in InputEvent); -- TODO: InputEvent
  procedure Action_Erase_Events (Action : in Wide_String);
  function  Action_Has_Event    (Action : in Wide_String; Event : in InputEvent) return Boolean; -- TODO: InputEvent
  procedure Action_Set_Deadzone (Action : in Wide_String; Deadzone : in Real_64_Percent);
  procedure Add_Action          (Action : in Wide_String; Deadzone : in Real_64_Percent := 0.5);
  procedure Erase_Action        (Action : in Wide_String);
  procedure Event_Is_Action     (Event  : in InputEvent; Action : in Wide_String); -- TODO: InputEvent
  procedure Get_Action_List     (Action : in Wide_String; Result : out Arrays.GArray);
  procedure Get_Actions         (Result : out Arrays.GArray);
  function  Has_Action          (Action : in Wide_String) return Boolean;
  procedure Load_From_Globals;

  INPUT_MAP_MULTIPLE_INITIALIZATION_ASSERT : constant String := "Multiple initializations of Input_Map.";
  INPUT_MAP_UNINITIALIZED_ASSERT           : constant String := "Attempting to use without initializing Input_Map.";
  INPUT_MAP_EARLY_FINALIZE_ASSERT          : constant String := "Finalizing without initializing Input_Map.";

  INPUT_MAP_ACTION_DOES_NOT_EXIST          : constant Wide_String := "Input_Map does not have action: ";
  
-------
private
-------

  type Input_Map_Object is record
    Initialized         : Boolean                    := False;
    Handle              : Thin.godot_object          := Thin.Null_Godot_Object;
    Action_Add_Event    : Thin.godot_method_bind_ptr := null;
    Action_Erase_Event  : Thin.godot_method_bind_ptr := null;
    Action_Erase_Events : Thin.godot_method_bind_ptr := null;
    Action_Has_Event    : Thin.godot_method_bind_ptr := null;
    Action_Set_Deadzone : Thin.godot_method_bind_ptr := null;
    Add_Action          : Thin.godot_method_bind_ptr := null;
    Erase_Action        : Thin.godot_method_bind_ptr := null;
    Event_Is_Action     : Thin.godot_method_bind_ptr := null;
    Get_Action_List     : Thin.godot_method_bind_ptr := null;
    Get_Actions         : Thin.godot_method_bind_ptr := null;
    Has_Action          : Thin.godot_method_bind_ptr := null;
    Load_From_Globals   : Thin.godot_method_bind_ptr := null;
  end record;

  Input_Map_Singleton : Input_Map_Object;
end;