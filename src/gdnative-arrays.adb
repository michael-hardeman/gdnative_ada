with GDNative.Context;

package body GDNative.Arrays is
  
  ----------------
  -- Initialize --
  ----------------
  procedure Initialize (Object : in out GArray) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_array_new (Object.Low'access);
  end;

  --------------
  -- Finalize --
  --------------
  procedure Finalize (Object : in out GArray) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Context.Core_Api.godot_array_destroy (Object.Low'access);
  end;

  ----------
  -- Copy --
  ----------
  procedure Copy (Target : in out GArray; Source : in Thin.godot_variant_const_ptr) is begin
    pragma Assert (Context.Core_Initialized, CORE_UNINITIALIZED_ASSERT);
    Target.Low := Context.Core_Api.godot_variant_as_array (Source);
  end;

end;