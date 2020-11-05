with GDNative.Thin;

package GDNative.Strings is

  function To_Str    (S : in Wide_String)           return String;
  function To_Wide   (S : in String)                return Wide_String;
  function To_Ada    (S : access Thin.godot_string) return Wide_String;
  function To_Ada    (S : access Thin.godot_string) return String;
  function To_Godot  (S : in Wide_String)           return Thin.godot_string;
  function To_Godot  (S : in String)                return Thin.godot_string;

end;
