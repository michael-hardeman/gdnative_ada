with Interfaces.C;

with GDNative.Context;

package body GDNative.Strings is

  package IC renames Interfaces.C;

  ------------
  -- To Str --
  ------------
  function To_Str (S : in Wide_String) return String is
    Result : String (S'first .. S'last);
  begin
    for I in S'range loop Result (I) := Character'val (Wide_Character'pos (S (I))); end loop;
    return Result;
  end;

  -------------
  -- To Wide --
  -------------
  function To_Wide (S : in String) return Wide_String is
    Result : Wide_String (S'first .. S'last);
  begin
    for I in S'range loop Result (I) := Wide_Character'val (Character'pos (S (I))); end loop;
    return Result;
  end;

  ------------
  -- To_Ada --
  ------------
  function To_Ada (S : access Thin.godot_string) return Wide_String is
    function Impl (S : access Thin.godot_string) return Wide_String is
      Reference : Thin.wchar_T_Ptrs.Pointer := Context.Core_Api.godot_string_wide_str (S);
      Length    : IC.ptrdiff_t              := IC.ptrdiff_t (Context.Core_Api.godot_string_length (S));
    begin
      return IC.To_Ada (Thin.wchar_T_Ptrs.Value (Reference, Length));
    end;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Impl (S);
  end;

  ------------
  -- To_Ada --
  ------------
  function To_Ada (S : access Thin.godot_string) return String is begin
    return To_Str (To_Ada (S));
  end;

  --------------
  -- To_Godot --
  --------------
  function To_Godot (S : in Wide_String) return Thin.godot_string is
    C_String : IC.wchar_array := IC.To_C (S);
    G_String : aliased Thin.godot_string;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_string_new_with_wide_string (G_String'access, C_String (0)'access, C_String'Length);
    return G_String;
  end;

  --------------
  -- To_Godot --
  --------------
  function To_Godot (S : in String) return Thin.godot_string is begin
    return To_Godot (To_Wide (S));
  end;

end;
