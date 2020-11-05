with GDNative.Console;
with GDNative.Input; 

package body Action_RPG is
  use GDNative;

  procedure Physics_Process (Self : in out Player; Delta_Time : in Long_Float) is begin
    Self.Health := Self.Health - 1;
    if Self.Health = 0 then
      Console.Put ("Player Died!");
      Console.Put ("Player Respawn");
      Input.Action_Press ("ui_up_does_not_exist", 1.0);
      Self.Health := 100;
    end if;
  end;

  procedure Register_Classes is begin
    Player_Registration.Register_Class;
    Player_Registration.Register_Physics_Process;
  end;

end;