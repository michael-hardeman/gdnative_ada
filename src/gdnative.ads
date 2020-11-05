with Ada.Strings.Wide_Unbounded;

package GDNative is
  type Percentage is new Long_Float range 0.0 .. 1.0;

  type Wide_String_Array is array (Positive range <>) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;

  CORE_MULTIPLE_INITIALIZATION_ASSERT         : constant String := "Multiple initializations of Context.Core_API.";
  CORE_UNINITIALIZED_ASSERT                   : constant String := "Attempting to use without initializing Context.Core_API.";
  CORE_EARLY_FINALIZE_ASSERT                  : constant String := "Finalizing without initializing Context.Core_API.";
  NATIVESCRIPT_MULTIPLE_INITIALIZATION_ASSERT : constant String := "Finalizing without initializing Context.Nativescript_API.";
  NATIVESCRIP_UNINITIALIZED_ASSERT            : constant String := "Attempting to use without initializing Context.Core_API.";
  NATIVESCRIPT_EARLY_FINALIZE_ASSERT          : constant String := "Multiple initializations of Context.Nativescript_API.";

  Unimplemented_Feature : exception;
end;