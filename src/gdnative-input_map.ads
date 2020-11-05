with GDnative.Thin;
with GDNative.Variants;

package GDNative.Input_Map is
  
  procedure Initialize;
  procedure Finalize;

  type InputEvent is null record; -- TODO: InputEvent

  procedure Action_Add_Event    (Action : in Wide_String; Event : in InputEvent); -- TODO: InputEvent
  procedure Action_Erase_Event  (Action : in Wide_String; Event : in InputEvent); -- TODO: InputEvent
  procedure Action_Erase_Events (Action : in Wide_String);
  function  Action_Has_Event    (Action : in Wide_String; Event : in InputEvent) return Boolean; -- TODO: InputEvent
  procedure Action_Set_Deadzone (Action : in Wide_String; Deadzone : in Percentage);
  procedure Add_Action          (Action : in Wide_String; Deadzone : in Percentage := 0.5);
  procedure Erase_Action        (Action : in Wide_String);
  procedure Event_Is_Action     (Event  : in InputEvent;  Action : in Wide_String); -- TODO: InputEvent
  function  Get_Action_List     (Action : in Wide_String) return Wide_String_Array;
  function  Get_Actions                                   return Wide_String_Array;
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
    Initialized         : Boolean                       := False;
    Handle              : Thin.godot_object             := Thin.Null_Godot_Object;
    Action_Add_Event    : access Thin.godot_method_bind := null;
    Action_Erase_Event  : access Thin.godot_method_bind := null;
    Action_Erase_Events : access Thin.godot_method_bind := null;
    Action_Has_Event    : access Thin.godot_method_bind := null;
    Action_Set_Deadzone : access Thin.godot_method_bind := null;
    Add_Action          : access Thin.godot_method_bind := null;
    Erase_Action        : access Thin.godot_method_bind := null;
    Event_Is_Action     : access Thin.godot_method_bind := null;
    Get_Action_List     : access Thin.godot_method_bind := null;
    Get_Actions         : access Thin.godot_method_bind := null;
    Has_Action          : access Thin.godot_method_bind := null;
    Load_From_Globals   : access Thin.godot_method_bind := null;
  end record;

  Input_Map_Singleton : Input_Map_Object;
end;