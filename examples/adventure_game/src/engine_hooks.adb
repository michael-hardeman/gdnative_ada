with GDNative.Thick;         use GDNative.Thick;
with GDNative.Thick.Console; use GDNative.Thick.Console;

with Adventure;

package body Engine_Hooks is

  procedure On_GDNative_Init (p_options : access godot_gdnative_init_options) is begin
    GDNative_Initialize (p_options);

    Put ("GDNative Initialized!");
  end;

  procedure On_GDNative_Terminate (p_options : access godot_gdnative_terminate_options) is begin
    Put ("GDNative Finalized!");

    GDNative_Finalize (p_options);
  end;

  procedure On_Nativescript_Init (p_handle : Nativescript_Handle) is begin
    Put ("Nativescript Initialzing");

    Nativescript_Initialize (p_handle);

    Adventure.Register_Classes;

    Put ("Nativescript Initialized!");
  end;
  
end;
