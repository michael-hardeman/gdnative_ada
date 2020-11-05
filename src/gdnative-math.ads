with GDNative.Thin;

package GDNative.Math is

  type Vector2     is private;
  type Vector3     is private;
  type Rect2       is private;
  type Transform2d is private;
  type Plane       is private;
  type Quat        is private;
  type AABB        is private;
  type Basis       is private;
  type Transform   is private;

  function To_Ada (Item : in Thin.godot_vector2)     return Vector2;
  function To_Ada (Item : in Thin.godot_vector3)     return Vector3;
  function To_Ada (Item : in Thin.godot_rect2)       return Rect2;
  function To_Ada (Item : in Thin.godot_transform2d) return Transform2d;
  function To_Ada (Item : in Thin.godot_plane)       return Plane;
  function To_Ada (Item : in Thin.godot_quat)        return Quat;
  function To_Ada (Item : in Thin.godot_aabb)        return AABB;
  function To_Ada (Item : in Thin.godot_basis)       return Basis;
  function To_Ada (Item : in Thin.godot_transform)   return Transform;

  function To_Godot (Item : in Vector2)     return Thin.godot_vector2;
  function To_Godot (Item : in Vector3)     return Thin.godot_vector3;
  function To_Godot (Item : in Rect2)       return Thin.godot_rect2;
  function To_Godot (Item : in Transform2d) return Thin.godot_transform2d;
  function To_Godot (Item : in Plane)       return Thin.godot_plane;
  function To_Godot (Item : in Quat)        return Thin.godot_quat;
  function To_Godot (Item : in AABB)        return Thin.godot_aabb;
  function To_Godot (Item : in Basis)       return Thin.godot_basis;
  function To_Godot (Item : in Transform)   return Thin.godot_transform;

  function Zero return Vector2;
  function Zero return Vector3;

  function New_Vector2 (X, Y    : in Float) return Vector2;
  function New_Vector3 (X, Y, Z : in Float) return Vector3;

-------
private
-------

  type Vector2     is new Thin.godot_vector2;
  type Vector3     is new Thin.godot_vector3;
  type Rect2       is new Thin.godot_rect2;
  type Transform2d is new Thin.godot_transform2d;
  type Plane       is new Thin.godot_plane;
  type Quat        is new Thin.godot_quat;
  type AABB        is new Thin.godot_aabb;
  type Basis       is new Thin.godot_basis;
  type Transform   is new Thin.godot_transform;

end;