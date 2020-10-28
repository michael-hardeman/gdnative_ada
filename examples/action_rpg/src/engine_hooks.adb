with GDNative.Context;
with GDNative.Console;
with GDNative.Exceptions;

with Action_RPG;

package body Engine_Hooks is

  procedure On_GDNative_Init (p_options : access Thin.godot_gdnative_init_options) is begin
    Context.GDNative_Initialize (p_options);

    Console.Put ("GDNative Initialized!");
  end;

  procedure On_GDNative_Terminate (p_options : access Thin.godot_gdnative_terminate_options) is begin
    Console.Put ("GDNative Finalized!");

    Context.GDNative_Finalize (p_options);
  end;

  procedure On_Nativescript_Init (p_handle : Thin.Nativescript_Handle) is begin
    Console.Put ("Nativescript Initialzing");

    Context.Nativescript_Initialize (p_handle);

    Action_RPG.Register_Classes;

    Console.Put ("Nativescript Initialized!");
  exception
    when Error : others => Exceptions.Put_Error (Error);
  end;
  
end;
