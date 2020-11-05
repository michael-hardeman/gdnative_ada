
with GDNative.Context;

package body GDNative.Math is
  
  function To_Ada (Item : in Thin.godot_vector2)     return Vector2 is (Vector2 (Item));
  function To_Ada (Item : in Thin.godot_vector3)     return Vector3 is (Vector3 (Item));
  function To_Ada (Item : in Thin.godot_rect2)       return Rect2 is (Rect2 (Item));
  function To_Ada (Item : in Thin.godot_transform2d) return Transform2d is (Transform2d (Item));
  function To_Ada (Item : in Thin.godot_plane)       return Plane is (Plane (Item));
  function To_Ada (Item : in Thin.godot_quat)        return Quat is (Quat (Item));
  function To_Ada (Item : in Thin.godot_aabb)        return AABB is (AABB (Item));
  function To_Ada (Item : in Thin.godot_basis)       return Basis is (Basis (Item));
  function To_Ada (Item : in Thin.godot_transform)   return Transform is (Transform (Item));

  function To_Godot (Item : in Vector2)     return Thin.godot_vector2 is (Thin.godot_vector2 (Item));
  function To_Godot (Item : in Vector3)     return Thin.godot_vector3 is (Thin.godot_vector3 (Item));
  function To_Godot (Item : in Rect2)       return Thin.godot_rect2 is (Thin.godot_rect2 (Item));
  function To_Godot (Item : in Transform2d) return Thin.godot_transform2d is (Thin.godot_transform2d (Item));
  function To_Godot (Item : in Plane)       return Thin.godot_plane is (Thin.godot_plane (Item));
  function To_Godot (Item : in Quat)        return Thin.godot_quat is (Thin.godot_quat (Item));
  function To_Godot (Item : in AABB)        return Thin.godot_aabb is (Thin.godot_aabb (Item));
  function To_Godot (Item : in Basis)       return Thin.godot_basis is (Thin.godot_basis (Item));
  function To_Godot (Item : in Transform)   return Thin.godot_transform is (Thin.godot_transform (Item));

  -----------------
  -- New Vector2 --
  -----------------
  function New_Vector2 (X, Y : in Float) return Vector2 is
    Result : aliased Thin.godot_vector2;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_vector2_new (Result'access, Thin.godot_real (X), Thin.godot_real (Y));
    return To_Ada (Result);
  end;

  -----------------
  -- New Vector3 --
  -----------------
  function New_Vector3 (X, Y, Z : in Float) return Vector3 is 
    Result : aliased Thin.godot_vector3;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_vector3_new (Result'access, Thin.godot_real (X), Thin.godot_real (Y), Thin.godot_real (Y));
    return To_Ada (Result);
  end;

  function Zero return Vector2 is (New_Vector2 (0.0, 0.0));
  function Zero return Vector3 is (New_Vector3 (0.0, 0.0, 0.0));

end;