with GDNative.Console; use GDNative.Console;

package body Action_RPG is

  procedure Physics_Process (Self : in out Player; Delta_Time : in Long_Float) is begin
    Self.Health := Self.Health - 1;
    if Self.Health = 0 then
      Put ("Player Died!");
      Put ("Player Respawn");
      Self.Health := 100;
    end if;
  end;

  procedure Register_Classes is begin
    Player_Registration.Register_Class;
    Player_Registration.Register_Physics_Process;
  end;

end;