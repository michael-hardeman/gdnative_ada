with GDNative.Thick.Console; use GDNative.Thick.Console;
with GDNative.Thick.Exceptions; use GDNative.Thick.Exceptions;

package body Adventure is

  overriding procedure Process (Item : in out Player; Delta_Time : in Long_Float) is begin
    Player.Health := Player.Health - 1;
    if Player.Health = 0 then
      Put ("Player Died!");
      Put ("Player Respawn");
      Player.Health = 100;
    end if;
  end;

  procedure Register_Classes is begin
    Player_Class_Registration.Register_Class;
    Player_Node_Registration.Register_Process;
  end;

end;