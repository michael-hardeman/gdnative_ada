with Ada.Finalization;

with GDNative.Thin;
with GDNative.Math;
with GDNative.Strings;
with GDNative.Arrays;

package GDNative.Variants is 

  type Variant is new Ada.Finalization.Controlled with private;

  -- godot_variant_destroy
  overriding procedure Finalize (Object : in out Variant);

  function Ref (Item : in Variant) return Thin.godot_variant_const_ptr;

  type Variant_Kind is (
    Nil_Kind,
    Bool_Kind,
    Int_Kind,
    Real_Kind,
    String_Kind,
    Vector2_Kind,
    Rect2_Kind,
    Vector3_Kind,
    Transform2d_Kind,
    Plane_Kind,
    Quat_Kind,
    AABB_Kind,
    Basis_Kind,
    Transform_Kind,
    Color_Kind,
    Node_path_Kind,
    Rid_Kind,
    Object_Kind,
    Dictionary_Kind,
    Array_Kind,
    Pool_Byte_Array_Kind,
    Pool_Int_Array_Kind,
    Pool_Real_Array_Kind,
    Pool_String_Array_Kind,
    Pool_Vector2_Array_Kind,
    Pool_Vector3_Array_Kind,
    Pool_Color_Array_Kind);

  -- godot_variant_get_type
  function Kind (Item : in Variant) return Variant_Kind;

  -- godot_variant_new_copy
  procedure Initialize (Item : in out Variant; Value : in Thin.godot_variant_ptr);
  -- godot_variant_new_bool
  procedure Initialize (Item : in out Variant; Value : in Boolean);
  -- godot_variant_new_uint
  procedure Initialize (Item : in out Variant; Value : in Int_64_Unsigned);
  -- godot_variant_new_int
  procedure Initialize (Item : in out Variant; Value : in Int_64_Signed);
  -- godot_variant_new_real
  procedure Initialize (Item : in out Variant; Value : in Real_64);
  -- godot_variant_new_string
  procedure Initialize (Item : in out Variant; Value : in Strings.GString);
  procedure Initialize (Item : in out Variant; Value : in Wide_String);
  -- godot_variant_new_vector2
  procedure Initialize (Item : in out Variant; Value : in Math.Vector2);
  -- godot_variant_new_rect2
  procedure Initialize (Item : in out Variant; Value : in Math.Rect2);
  -- godot_variant_new_vector3
  procedure Initialize (Item : in out Variant; Value : in Math.Vector3);
  -- godot_variant_new_transform2d
  procedure Initialize (Item : in out Variant; Value : in Math.Transform2d);
  -- godot_variant_new_plane
  procedure Initialize (Item : in out Variant; Value : in Math.Plane);
  -- godot_variant_new_quat
  procedure Initialize (Item : in out Variant; Value : in Math.Quat);
  -- godot_variant_new_aabb
  procedure Initialize (Item : in out Variant; Value : in Math.AABB);
  -- godot_variant_new_basis
  procedure Initialize (Item : in out Variant; Value : in Math.Basis);
  -- godot_variant_new_transform
  procedure Initialize (Item : in out Variant; Value : in Math.Transform);
  -- TODO: godot_variant_new_color
  -- TODO: godot_variant_new_node_path
  -- TODO: godot_variant_new_rid
  -- TODO: godot_variant_new_object
  -- TODO: godot_variant_new_dictionary
  -- TODO: godot_variant_new_array
  -- TODO: godot_variant_new_pool_byte_array
  -- TODO: godot_variant_new_pool_int_array
  -- TODO: godot_variant_new_pool_real_array
  -- TODO: godot_variant_new_pool_string_array
  -- TODO: godot_variant_new_pool_vector2_array
  -- TODO: godot_variant_new_pool_vector3_array
  -- TODO: godot_variant_new_pool_color_array

  -- godot_variant_as_bool
  function Value (Item : in Variant) return Boolean;
  -- godot_variant_as_uint
  function Value (Item : in Variant) return Int_64_Unsigned;
  -- godot_variant_as_int
  function Value (Item : in Variant) return Int_64_Signed;
  -- godot_variant_as_real
  function Value (Item : in Variant) return Real_64;
  -- godot_variant_as_string
  procedure Value (Item : in Variant; Result : out Strings.GString);
  function Value (Item : in Variant) return Wide_String;
  -- godot_variant_as_vector2
  function Value (Item : in Variant) return Math.Vector2;
  -- godot_variant_as_rect2
  function Value (Item : in Variant) return Math.Rect2;
  -- godot_variant_as_vector3
  function Value (Item : in Variant) return Math.Vector3;
  -- godot_variant_as_transform2d
  function Value (Item : in Variant) return Math.Transform2d;
  -- godot_variant_as_plane
  function Value (Item : in Variant) return Math.Plane;
  -- godot_variant_as_quat
  function Value (Item : in Variant) return Math.Quat;
  -- godot_variant_as_aabb
  function Value (Item : in Variant) return Math.AABB;
  -- godot_variant_as_basis
  function Value (Item : in Variant) return Math.Basis;
  -- godot_variant_as_transform
  function Value (Item : in Variant) return Math.Transform;
  -- TODO: godot_variant_as_color
  -- TODO: godot_variant_as_node_path
  -- TODO: godot_variant_as_rid
  -- TODO: godot_variant_as_object
  -- TODO: godot_variant_as_dictionary
  -- TODO: godot_variant_as_array
  -- TODO: godot_variant_as_pool_byte_array
  -- TODO: godot_variant_as_pool_int_array
  -- TODO: godot_variant_as_pool_real_array
  -- TODO: godot_variant_as_pool_string_array
  -- TODO: godot_variant_as_pool_vector2_array
  -- TODO: godot_variant_as_pool_vector3_array
  -- TODO: godot_variant_as_pool_color_array

  -- TODO: godot_variant_call
  -- TODO: godot_variant_has_method
  -- TODO: godot_variant_operator_equal 
  -- TODO: godot_variant_operator_less
  -- TODO: godot_variant_hash_compare
  -- TODO: godot_variant_booleanize

-------
private
-------

  type Variant is new Ada.Finalization.Controlled with record
    Low : aliased Thin.godot_variant;
  end record;
end;