with GDNative.Console;
with GDNative.Input; 

package body Action_RPG is
  use GDNative;

  procedure Physics_Process (Self : in out Player; Delta_Time : in Long_Float) is begin
    if Input.Is_Action_Pressed ("ui_up") then
      Console.Put ("Up Pressed");
    elsif Input.Is_Action_Pressed ("ui_right") then
      Console.Put ("Right Pressed");
    elsif Input.Is_Action_Pressed ("ui_down") then
      Console.Put ("Down Pressed");
    elsif Input.Is_Action_Pressed ("ui_left") then
      Console.Put ("Left Pressed");
    end if;
  end;

  procedure Register_Classes is begin
    Player_Registration.Register_Class;
    Player_Registration.Register_Physics_Process;
  end;

end;