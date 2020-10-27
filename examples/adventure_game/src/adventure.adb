with GDNative.Console; use GDNative.Console;

package body Adventure is

  overriding procedure Process (Item : in out Player; Delta_Time : in Long_Float) is begin
    Item.Health := Item.Health - 1;
    if Item.Health = 0 then
      Put ("Player Died!");
      Put ("Player Respawn");
      Item.Health := 100;
    end if;
  end;

  procedure Register_Classes is begin
    Player_Class_Registration.Register_Class;
    Player_Node_Registration.Register_Process;
  end;

end;