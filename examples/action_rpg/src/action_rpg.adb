with GDNative.Console;
with GDNative.Input; 

package body Action_RPG is
  use GDNative;

  ----------------
  -- Initialize --
  ----------------
  procedure Initialize is begin
    null;
  end;

  ----------------------
  -- Register Classes --
  ----------------------
  procedure Register_Classes is begin
    Player_Registration.Register_Class;
    Player_Registration.Register_Physics_Process;
  end;

  --------------
  -- Finalize --
  --------------
  procedure Finalize is begin
    null;
  end;

  ------------
  -- Player --
  ------------
  procedure Physics_Process (Self : in out Player; Delta_Time : in Real_64) is begin
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

end;