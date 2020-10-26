with GDNative.Thick.Console; use GDNative.Thick.Console;
with GDNative.Thick.Exceptions; use GDNative.Thick.Exceptions;

package body Adventure is

  overriding procedure Process (Item : in Player; Delta_Time : in Long_Float) is begin
    Put ("Player Delta_Time: " & Delta_Time'Wide_Image & " Player Health: " & Item.Health'Wide_Image);
  end;

  procedure Register_Classes is begin
    Player_Class_Registration.Register_Class;
    Player_Node_Registration.Register_Process;
  exception
    when Occurrence : others => Exceptions.Put_Error (Occurrence);
  end;

end;