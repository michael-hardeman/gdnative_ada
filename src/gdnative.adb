package body GDNative is

  ------------
  -- To Str --
  ------------
  function To_Str (S : Wide_String) return String is
    Result : String (S'first .. S'last);
  begin
    for I in S'range loop Result (I) := Character'val (Wide_Character'pos (S (I))); end loop;
    return Result;
  end;

  -------------
  -- To Wide --
  -------------
  function To_Wide (S : String) return Wide_String is
    Result : Wide_String (S'first .. S'last);
  begin
    for I in S'range loop Result (I) := Wide_Character'val (Character'pos (S (I))); end loop;
    return Result;
  end;

end; 