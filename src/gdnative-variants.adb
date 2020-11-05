with Interfaces.C;
with Interfaces.C.Extensions;

with Ada.Strings.Wide_Unbounded;

with GDNative.Context;
with GDNative.Strings;

package body GDNative.Variants is
  
  package IC   renames Interfaces.C;
  package ICE  renames Interfaces.C.Extensions;
  package ASWU renames Ada.Strings.Wide_Unbounded;

  -------------
  -- Boolean --
  -------------
  function To_Ada (Item : access Thin.godot_variant) return Boolean is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Boolean (Context.Core_Api.godot_variant_as_bool (Item));
  end;

  -------------
  -- Natural --
  -------------
  function To_Ada (Item : access Thin.godot_variant) return Natural is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Natural (Context.Core_Api.godot_variant_as_uint (Item));
  end;

  ------------------
  -- Long Integer --
  ------------------
  function To_Ada (Item : access Thin.godot_variant) return Long_Integer is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Long_Integer (Context.Core_Api.godot_variant_as_int (Item));
  end;

  ----------------
  -- Long Float --
  ----------------
  function To_Ada (Item : access Thin.godot_variant) return Long_Float is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Long_Float (Context.Core_Api.godot_variant_as_real (Item));
  end;

  -------------
  -- Vector2 --
  -------------
  function To_Ada (Item : access Thin.godot_variant) return Math.Vector2 is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_vector2 (Item));
  end;

  -----------
  -- Rect2 --
  -----------
  function To_Ada (Item : access Thin.godot_variant) return Math.Rect2 is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_rect2 (Item));
  end;

  -------------
  -- Vector3 --
  -------------
  function To_Ada (Item : access Thin.godot_variant) return Math.Vector3 is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_vector3 (Item));
  end;

  -----------------
  -- Transform2d --
  -----------------
  function To_Ada (Item : access Thin.godot_variant) return Math.Transform2d is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_transform2d (Item));
  end;

  -----------
  -- Plane --
  -----------
  function To_Ada (Item : access Thin.godot_variant) return Math.Plane is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_plane (Item));
  end;

  ----------
  -- Quat --
  ----------
  function To_Ada (Item : access Thin.godot_variant) return Math.Quat is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_quat (Item));
  end;

  ----------
  -- AABB --
  ----------
  function To_Ada (Item : access Thin.godot_variant) return Math.AABB is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_aabb (Item));
  end;

  -----------
  -- Basis --
  -----------
  function To_Ada (Item : access Thin.godot_variant) return Math.Basis is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_basis (Item));
  end;

  ---------------
  -- Transform --
  ---------------
  function To_Ada (Item : access Thin.godot_variant) return Math.Transform is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Math.To_Ada (Context.Core_Api.godot_variant_as_transform (Item));
  end;

  -----------------
  -- Wide String --
  -----------------
  function To_Ada (Item : access Thin.godot_variant) return Wide_String is
    function Impl (Item : access Thin.godot_variant) return Wide_String is
      G_String : aliased Thin.godot_string := Context.Core_Api.godot_variant_as_string (Item);
      Result   : Wide_String               := Strings.To_Ada (G_String'access);
    begin
      Context.Core_Api.godot_string_destroy (G_String'access);
      return Result;
    end;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Impl (Item);
  end;

  --------------------------
  -- Array of Wide String --
  --------------------------
  function To_Ada (Item : access Thin.godot_variant) return Wide_String_Array is
    function Impl (Item : access Thin.godot_variant) return Wide_String_Array is
      use type Thin.godot_int;
      G_Array : aliased Thin.godot_array := Context.Core_Api.godot_variant_as_array (Item);
      Length  : Thin.godot_int           := (Context.Core_Api.godot_array_size (G_Array'access));
      Current : aliased Thin.godot_variant;
      Result  : Wide_String_Array (1 .. Natural (Length));
      Result_I: Natural := 1;
    begin
      if Length = 0 then return Result; end if;
      for I in 0 .. Length - 1 loop
        Current := Context.Core_Api.godot_array_get (G_Array'access, I);
        declare
          Temp : Wide_String := To_Ada (Current'access);
        begin
          Result (Result_I) := ASWU.To_Unbounded_Wide_String (Temp);
        end;
        Context.Core_Api.godot_variant_destroy (Current'access);
        Result_I := Result_I + 1;
      end loop;
      return Result;
    end;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Impl (Item);
  end;

  -------------
  -- Boolean --
  -------------
  function To_Godot (Item : in Boolean) return Thin.godot_variant is 
    Result : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_bool (Result'access, Thin.godot_bool (Item));
    return Result;
  end;

  -------------
  -- Natural --
  -------------
  function To_Godot (Item : in Natural) return Thin.godot_variant is 
    Result : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_uint (Result'access, ICE.long_long (Item));
    return Result;
  end;

  ------------------
  -- Long Integer --
  ------------------
  function To_Godot (Item : in Long_Integer) return Thin.godot_variant is 
    Result : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_int (Result'access, IC.long (Item));
    return Result;
  end;

  ----------------
  -- Long Float --
  ----------------
  function To_Godot (Item : in Long_Float) return Thin.godot_variant is 
    Result : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_real (Result'access, IC.double (Item));
    return Result;
  end;

  -----------------
  -- Wide_String --
  -----------------
  function To_Godot (Item : in Wide_String) return Thin.godot_variant is 
    G_String : aliased Thin.godot_string := Strings.To_Godot (Item);
    Result   : aliased Thin.godot_variant; 
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_string (Result'access, G_String'access);
    Context.Core_Api.godot_string_destroy (G_String'access);
    return Result;
  end;

  -------------
  -- Vector2 --
  -------------
  function To_Godot (Item : in Math.Vector2) return Thin.godot_variant is 
    G_Vector2 : aliased Thin.godot_vector2 := Math.To_Godot (Item);
    Result    : aliased Thin.godot_variant;
  begin 
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_vector2 (Result'access, G_Vector2'access);
    return Result;
  end;

  -------------
  -- Vector3 --
  -------------
  function To_Godot (Item : in Math.Vector3) return Thin.godot_variant is
    G_Vector3 : aliased Thin.godot_vector3 := Math.To_Godot (Item);
    Result    : aliased Thin.godot_variant;
  begin 
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_vector3 (Result'access, G_Vector3'access);
    return Result;
  end;

  -----------
  -- Rect2 --
  -----------
  function To_Godot (Item : in Math.Rect2) return Thin.godot_variant is
    G_Rect2 : aliased Thin.godot_rect2 := Math.To_Godot (Item);
    Result  : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_rect2 (Result'access, G_Rect2'access);
    return Result;
  end;

  -----------------
  -- Transform2d --
  -----------------
  function To_Godot (Item : in Math.Transform2d) return Thin.godot_variant is
    G_Transform2d : aliased Thin.godot_transform2d := Math.To_Godot (Item);
    Result        : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_transform2d (Result'access, G_Transform2d'access);
    return Result;
  end;

  -----------
  -- Plane --
  -----------
  function To_Godot (Item : in Math.Plane) return Thin.godot_variant is
    G_Plane : aliased Thin.godot_plane := Math.To_Godot (Item);
    Result  : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_plane (Result'access, G_Plane'access);
    return Result;
  end;

  ----------
  -- Quat --
  ----------
  function To_Godot (Item : in Math.Quat) return Thin.godot_variant is
    G_Quat : aliased Thin.godot_quat := Math.To_Godot (Item);
    Result : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_quat (Result'access, G_Quat'access);
    return Result;
  end;

  ----------
  -- AABB --
  ----------
  function To_Godot (Item : in Math.AABB) return Thin.godot_variant is
    G_AABB : aliased Thin.godot_aabb := Math.To_Godot (Item);
    Result : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_aabb (Result'access, G_AABB'access);
    return Result;
  end;

  -----------
  -- Basis --
  -----------
  function To_Godot (Item : in Math.Basis) return Thin.godot_variant is
    G_Basis : aliased Thin.godot_basis := Math.To_Godot (Item);
    Result  : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_basis (Result'access, G_Basis'access);
    return Result;
  end;

  ---------------
  -- Transform --
  ---------------
  function To_Godot (Item : in Math.Transform) return Thin.godot_variant is
    G_Transform : aliased Thin.godot_transform := Math.To_Godot (Item);
    Result      : aliased Thin.godot_variant;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_variant_new_transform (Result'access, G_Transform'access);
    return Result;
  end;

end;