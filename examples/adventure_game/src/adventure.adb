package body Adventure is

  overriding procedure Process (Item : in Player; Delta_Time : in Long_Float) is begin
    Put ("Player Delta_Time: " & Delta_Time'Image);
  end;

  procedure Register_Classes is begin
    Register_Class (Player'Tag, Player'Size);
    Register_Process (Player'Tag, Process'access);
  end;

end;