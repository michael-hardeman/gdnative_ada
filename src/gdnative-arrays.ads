with Ada.Finalization;

with GDNative.Thin;

package GDNative.Arrays is

  type GArray is new Ada.Finalization.Controlled with private;

  -- godot_array_new
  overriding procedure Initialize (Object : in out GArray);
  -- godot_array_destroy
  overriding procedure Finalize (Object : in out GArray);

  procedure Copy (Target : in out GArray; Source : in Thin.godot_variant_const_ptr);

-------
private
-------

  type GArray is new Ada.Finalization.Controlled with record
    Low : aliased Thin.godot_array;
  end record;

end;