with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Exceptions;

with Example_Object;
with GDNative.Thick; use GDNative.Thick;

package body Engine_Hooks is

  Context : GDNative_Context;

  procedure On_GDNative_Init (p_options : access godot_gdnative_init_options) is 
    Intialize (Context, p_options);

    Put (Context, "GDNative Initialized!");
  end;

  procedure On_GDNative_Terminate (p_options : access godot_gdnative_terminate_options) is begin
    Put (Context, "GDNative Finalized!");

    Finalize (Context);
  end;

  procedure On_Nativescript_Init (p_handle : System.Address) is begin
    Put (Context, "Nativescript Initialzing");

    Initialize_Nativescript (Context, p_handle);
    Example_Object.Register (Context);

    Put (Context, "Nativescript Initialized!");
  end;
  
end;
