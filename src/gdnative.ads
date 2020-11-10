with Interfaces; use Interfaces;

package GDNative is

  type Version_State is record
    Major : Unsigned_64;
    Minor : Unsigned_64;
    Patch : Unsigned_64;
  end record;
  Version : constant Version_State := (Major => 0, Minor => 1, Patch => 0);

  subtype Int_64_Unsigned is Unsigned_64;
  subtype Int_64_Signed   is Long_Long_Integer;
  subtype Int_64_Natural  is Int_64_Unsigned;
  subtype Int_64_Positive is Int_64_Signed range 1 .. Int_64_Signed'Last;

  subtype Real_64         is Long_Float;
  subtype Real_64_Percent is Real_64 range 0.0 .. 1.0;

  CORE_MULTIPLE_INITIALIZATION_ASSERT         : constant String := "Multiple initializations of Context.Core_API.";
  CORE_UNINITIALIZED_ASSERT                   : constant String := "Attempting to use without initializing Context.Core_API.";
  CORE_EARLY_FINALIZE_ASSERT                  : constant String := "Finalizing without initializing Context.Core_API.";
  NATIVESCRIPT_MULTIPLE_INITIALIZATION_ASSERT : constant String := "Finalizing without initializing Context.Nativescript_API.";
  NATIVESCRIP_UNINITIALIZED_ASSERT            : constant String := "Attempting to use without initializing Context.Core_API.";
  NATIVESCRIPT_EARLY_FINALIZE_ASSERT          : constant String := "Multiple initializations of Context.Nativescript_API.";

  Unimplemented_Feature : exception;

end;