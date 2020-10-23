with GDNative.Thick; use GDNative.Thick;

with Adventure;

package body Engine_Hooks is

  procedure On_GDNative_Init (p_options : access godot_gdnative_init_options) is begin
    Initialize_GDNative (p_options);

    Put ("GDNative Initialized!");
  end;

  procedure On_GDNative_Terminate (p_options : access godot_gdnative_terminate_options) is begin
    Put ("GDNative Finalized!");

    Finalize_GDNative (p_options);
  end;

  procedure On_Nativescript_Init (p_handle : Nativescript_Handle) is begin
    Put ("Nativescript Initialzing");

    Initialize_Nativescript (p_handle);

    Adventure.Register_Classes;

    Put ("Nativescript Initialized!");
  end;
  
end;
