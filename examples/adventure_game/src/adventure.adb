with GDNative.Thick.Console; use GDNative.Thick.Console;

package body Adventure is

  overriding procedure Process (Item : in Player; Delta_Time : in Long_Float) is begin
    Put ("Player Delta_Time: " & Delta_Time'Wide_Image & " Player Health: " & Item.Health'Wide_Image);
  end;

  procedure Register_Classes is begin
    Player_Class_Registration.Register_Class;
    Player_Node_Registration.Register_Process;
  end;

end;