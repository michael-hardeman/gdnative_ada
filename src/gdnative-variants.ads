with GDNative.Thin;
with GDNative.Math;

package GDNative.Variants is 

  function To_Ada (Item : access Thin.godot_variant) return Boolean;
  function To_Ada (Item : access Thin.godot_variant) return Natural;
  function To_Ada (Item : access Thin.godot_variant) return Long_Integer;
  function To_Ada (Item : access Thin.godot_variant) return Long_Float;
  function To_Ada (Item : access Thin.godot_variant) return Math.Vector2;
  function To_Ada (Item : access Thin.godot_variant) return Math.Rect2;
  function To_Ada (Item : access Thin.godot_variant) return Math.Vector3;
  function To_Ada (Item : access Thin.godot_variant) return Math.Transform2d;
  function To_Ada (Item : access Thin.godot_variant) return Math.Plane;
  function To_Ada (Item : access Thin.godot_variant) return Math.Quat;
  function To_Ada (Item : access Thin.godot_variant) return Math.AABB;
  function To_Ada (Item : access Thin.godot_variant) return Math.Basis;
  function To_Ada (Item : access Thin.godot_variant) return Math.Transform;
  function To_Ada (Item : access Thin.godot_variant) return Wide_String;

  function To_Ada (Item : access Thin.godot_variant) return Wide_String_Array;
  -- godot_variant_as_color
  -- godot_variant_as_node_path
  -- godot_variant_as_rid
  -- godot_variant_as_object
  -- godot_variant_as_dictionary
  -- godot_variant_as_array

  function To_Godot (Item : in Boolean)          return Thin.godot_variant;
  function To_Godot (Item : in Natural)          return Thin.godot_variant;
  function To_Godot (Item : in Long_Integer)     return Thin.godot_variant;
  function To_Godot (Item : in Long_Float)       return Thin.godot_variant;
  function To_Godot (Item : in Wide_String)      return Thin.godot_variant;
  function To_Godot (Item : in Math.Vector2)     return Thin.godot_variant;
  function To_Godot (Item : in Math.Rect2)       return Thin.godot_variant;
  function To_Godot (Item : in Math.Vector3)     return Thin.godot_variant;
  function To_Godot (Item : in Math.Transform2d) return Thin.godot_variant;
  function To_Godot (Item : in Math.Plane)       return Thin.godot_variant;
  function To_Godot (Item : in Math.Quat)        return Thin.godot_variant;
  function To_Godot (Item : in Math.AABB)        return Thin.godot_variant;
  function To_Godot (Item : in Math.Basis)       return Thin.godot_variant;
  function To_Godot (Item : in Math.Transform)   return Thin.godot_variant;
  -- godot_variant_as_color
  -- godot_variant_as_node_path
  -- godot_variant_as_rid
  -- godot_variant_as_object
  -- godot_variant_as_dictionary
  -- godot_variant_as_array

end;