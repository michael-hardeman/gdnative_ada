with Interfaces.C;

with GDNative.Context;

package body GDNative.Strings is

  package IC renames Interfaces.C;

  ---------
  -- Ref --
  ---------
  function Ref (Item : in GString) return Thin.godot_string_const_ptr is (Item.Low'unchecked_access);

  ----------------
  -- Initialize --
  ----------------
  procedure Initialize (Item : in out GString; Value : in Wide_String) is
    C_String : IC.wchar_array := IC.To_C (Value);
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_string_new_with_wide_string (Item.Low'access, C_String (0)'access, C_String'Length);
  end;

  ----------
  -- Copy --
  ----------
  procedure Copy (Dest : in out GString; Source : in Thin.godot_string_const_ptr) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_string_new_copy (Dest.Low'access, Source);
  end;

  ----------
  -- Copy --
  ----------
  procedure Copy (Dest : in out GString; Source : in GString) is begin
    Copy (Dest, Source.Low'unchecked_access);
  end;

  -----------
  -- Image --
  -----------
  function Image (Item : in GString) return Wide_String is
    function Impl (Item : in Thin.godot_string_const_ptr) return Wide_String is
      Reference : Thin.wchar_T_Ptrs.Pointer := Context.Core_Api.godot_string_wide_str (Item);
      Length    : IC.ptrdiff_t              := IC.ptrdiff_t (Context.Core_Api.godot_string_length (Item));
    begin
      return IC.To_Ada (Thin.wchar_T_Ptrs.Value (Reference, Length));
    end;
  begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Impl (Item.Low'unchecked_access);
  end;


  ------------
  -- Length --
  ------------
  function Length (Item : in GString) return Int_64_Unsigned is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    return Int_64_Unsigned (Context.Core_Api.godot_string_length (Item.Low'access));
  end;


  --------------
  -- Finalize --
  --------------
  procedure Finalize (Object : in out GString) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_string_destroy (Object.Low'access);
  end;

end;
