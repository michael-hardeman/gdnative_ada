with System;
with Interfaces.C;

with GDnative.Thick; use GDNative.Thick;

package Example_Object is
  
  procedure Register (Con : in GDNative_Context);
 
-------
private 
-------

  function Constructor (
    p_instance    : System.Address;
    p_method_data : System.Address)
    return System.Address;
  pragma Convention(C, Constructor);

  procedure Destructor (
    p_instance    : System.Address;
    p_method_data : System.Address;
    p_user_data   : System.Address);
  pragma Convention(C, Destructor);
  
  function Process (
    p_instance    : System.Address;
    p_method_data : System.Address;
    p_user_data   : System.Address;
    p_num_args    : Interfaces.C.int;
    p_args        : Godot_Instance_Method_Args_Ptrs.Pointer) -- godot_variant **
    return GDNative.godot_variant;
  pragma Convention(C, Process);

  Context : GDNative_Context;

end;