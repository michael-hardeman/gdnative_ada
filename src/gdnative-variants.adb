with Interfaces.C;
with Interfaces.C.Extensions;

with GDNative.Context;

package body GDNative.Variants is
  
  package IC  renames Interfaces.C;
  package ICE renames Interfaces.C.Extensions;

  --------------
  -- Finalize --
  --------------
  procedure Finalize (Object : in out Variant) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_destroy (Object.Low'access);
  end;

  ---------
  -- Ref --
  ---------
  function Ref (Item : in Variant) return Thin.godot_variant_const_ptr is (Item.Low'unchecked_access);

  ----------
  -- Kind --
  ----------
  function Kind (Item : in Variant) return Variant_Kind is begin
    return Variant_Kind'Val (Thin.godot_variant_type'Pos (Context.Core_Api.godot_variant_get_type (Item.Low'access)));
  end;

  ----------------
  -- Initialize --
  ----------------
  procedure Initialize (Item : in out Variant; Value : in Thin.godot_variant_ptr) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_copy (Item.Low'access, Value);
  end;

  -------------
  -- Boolean --
  -------------
  -- godot_variant_new_bool
  procedure Initialize (Item : in out Variant; Value : in Boolean) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_bool (Item.Low'access, Thin.godot_bool (Value));
  end;

  ---------------------
  -- Int 64 Unsigned --
  ---------------------
  -- godot_variant_new_uint
  procedure Initialize (Item : in out Variant; Value : in Int_64_Unsigned) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_uint (Item.Low'access, ICE.long_long (Value));
  end;

  -------------------
  -- Int 64 Signed --
  -------------------
  -- godot_variant_new_int
  procedure Initialize (Item : in out Variant; Value : in Int_64_Signed) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_int (Item.Low'access, IC.long (Value));
  end;

  -------------
  -- Real 64 --
  -------------
  -- godot_variant_new_real
  procedure Initialize (Item : in out Variant; Value : in Real_64) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_real (Item.Low'access, IC.double (Value));
  end;

  -------------
  -- GString --
  -------------
  -- godot_variant_new_string
  procedure Initialize (Item : in out Variant; Value : in Strings.GString) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_string (Item.Low'access, Strings.Ref (Value));
  end;

  -----------------
  -- Wide_String --
  -----------------
  procedure Initialize (Item : in out Variant; Value : in Wide_String) is
    Temp : Strings.GString;
  begin
    Strings.Initialize (Temp, Value);
    Initialize (Item, Temp);
  end;
  
  -------------
  -- Vector2 --
  -------------
  -- godot_variant_new_vector2
  procedure Initialize (Item : in out Variant; Value : in Math.Vector2) is
    Temp : aliased Thin.godot_vector2;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_vector2 (Item.Low'access, Temp'access);
  end;

  -----------
  -- Rect2 --
  -----------
  -- godot_variant_new_rect2
  procedure Initialize (Item : in out Variant; Value : in Math.Rect2) is
    Temp : aliased Thin.godot_rect2;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_rect2 (Item.Low'access, Temp'access);
  end;

  -------------
  -- Vector3 --
  -------------
  -- godot_variant_new_vector3
  procedure Initialize (Item : in out Variant; Value : in Math.Vector3) is
    Temp : aliased Thin.godot_vector3;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_vector3 (Item.Low'access, Temp'access);
  end;

  -----------------
  -- Transform2d --
  -----------------
  -- godot_variant_new_transform2d
  procedure Initialize (Item : in out Variant; Value : in Math.Transform2d) is 
    Temp : aliased Thin.godot_transform2d;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_transform2d (Item.Low'access, Temp'access);
  end;

  -----------
  -- Plane --
  -----------
  -- godot_variant_new_plane
  procedure Initialize (Item : in out Variant; Value : in Math.Plane) is
    Temp : aliased Thin.godot_plane;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_plane (Item.Low'access, Temp'access);
  end;

  ----------
  -- Quat --
  ----------
  -- godot_variant_new_quat
  procedure Initialize (Item : in out Variant; Value : in Math.Quat) is
    Temp : aliased Thin.godot_quat;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_quat (Item.Low'access, Temp'access);
  end;
  
  ----------
  -- AABB --
  ----------
  -- godot_variant_new_aabb
  procedure Initialize (Item : in out Variant; Value : in Math.AABB) is 
    Temp : aliased Thin.godot_aabb;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_aabb (Item.Low'access, Temp'access);
  end;
  
  -----------
  -- Basis --
  -----------
  -- godot_variant_new_basis
  procedure Initialize (Item : in out Variant; Value : in Math.Basis) is
    Temp : aliased Thin.godot_basis;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_basis (Item.Low'access, Temp'access);
  end;
  
  ---------------
  -- Transform --
  ---------------
  -- godot_variant_new_transform
  procedure Initialize (Item : in out Variant; Value : in Math.Transform) is
    Temp : aliased Thin.godot_transform;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Temp := Math.To_Godot (Value);
    Context.Core_Api.godot_variant_new_transform (Item.Low'access, Temp'access);
  end;

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

  -----------
  -- Value --
  -----------
  -----------------------
  -- Assert_Kind_Match --
  -----------------------
  procedure Assert_Kind_Match (Actual : in Variant; Target : in Variant_Kind) is
    Actual_Kind : Variant_Kind := Kind (Actual);
  begin
    if Actual_Kind = Target then return; end if;
    raise Program_Error with "Invalid value access : Target = " & Target'Image & " Actual = " & Actual_Kind'Image;
  end;

  -------------
  -- Boolean --
  -------------
  -- godot_variant_as_bool
  function Value (Item : in Variant) return Boolean is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Bool_Kind);
    return Boolean (Context.Core_Api.godot_variant_as_bool (Item.Low'access));
  end;

  ---------------------
  -- Int_64_Unsigned --
  ---------------------
  -- godot_variant_as_uint
  function Value (Item : in Variant) return Int_64_Unsigned is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Int_Kind);
    return Int_64_Unsigned (Context.Core_Api.godot_variant_as_uint (Item.Low'access));
  end;

  -------------------
  -- Int_64_Signed --
  -------------------
  -- TODO: godot_variant_as_int
  function Value (Item : in Variant) return Int_64_Signed is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Int_Kind);
    return Int_64_Signed (Context.Core_Api.godot_variant_as_int (Item.Low'access));
  end;

  -------------
  -- Real_64 --
  -------------
  -- TODO: godot_variant_as_real
  function Value (Item : in Variant) return Real_64 is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Real_Kind);
    return Real_64 (Context.Core_Api.godot_variant_as_real (Item.Low'access));
  end;

  -------------
  -- GString --
  -------------
  -- TODO: godot_variant_as_string
  procedure Value (Item : in Variant; Result : out Strings.GString) is 
    Temp : aliased Thin.godot_string;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, String_Kind);
    Temp := Context.Core_Api.godot_variant_as_string (Item.Low'access);
    Strings.Copy (Result, Temp'unchecked_access);
  end;

  -----------------
  -- Wide_String --
  -----------------
  function Value (Item : in Variant) return Wide_String is
    Temp : Strings.GString;
  begin
    Value (Item, Temp);
    return Strings.Image (Temp);
  end;

  -------------
  -- Vector2 --
  -------------
  -- godot_variant_as_vector2
  function Value (Item : in Variant) return Math.Vector2 is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Vector2_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_vector2 (Item.Low'access));
  end;

  -----------
  -- Rect2 --
  -----------
  -- godot_variant_as_rect2
  function Value (Item : in Variant) return Math.Rect2 is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Rect2_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_rect2 (Item.Low'access));
  end;

  -------------
  -- Vector3 --
  -------------
  -- godot_variant_as_vector3
  function Value (Item : in Variant) return Math.Vector3 is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Vector3_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_vector3 (Item.Low'access));
  end;

  -----------------
  -- Transform2d --
  -----------------
  -- godot_variant_as_transform2d
  function Value (Item : in Variant) return Math.Transform2d is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Transform2d_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_transform2d (Item.Low'access));
  end;

  -----------
  -- Plane --
  -----------
  -- godot_variant_as_plane
  function Value (Item : in Variant) return Math.Plane is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Plane_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_plane (Item.Low'access));
  end;

  ----------
  -- Quat --
  ----------
  -- godot_variant_as_quat
  function Value (Item : in Variant) return Math.Quat is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Quat_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_quat (Item.Low'access));
  end;

  ----------
  -- AABB --
  ----------
  -- godot_variant_as_aabb
  function Value (Item : in Variant) return Math.AABB is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, AABB_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_aabb (Item.Low'access));
  end;

  -----------
  -- Basis --
  -----------
  -- godot_variant_as_basis
  function Value (Item : in Variant) return Math.Basis is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Basis_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_basis (Item.Low'access));
  end;

  ---------------
  -- Transform --
  ---------------
  -- godot_variant_as_transform
  function Value (Item : in Variant) return Math.Transform is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Assert_Kind_Match (Item, Transform_Kind);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_transform (Item.Low'access));
  end;

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
end;
