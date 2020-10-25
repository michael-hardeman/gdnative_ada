with System;

with Interfaces.C;

with Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;

package GDNative.Thick.Objects is

    type No_Parameters is null record;

    type Base is interface;
    function Constructor (Parameters : not null access No_Parameters) return Base is abstract;

    ------------
    -- Object --
    ------------
    type Object is abstract new Base with null record;
    function Constructor (Parameters : not null access No_Parameters) return Object is abstract;

    ----------
    -- Node --
    ----------
    type Node is abstract new Object with null record;
    function Constructor (Parameters : not null access No_Parameters) return Node is abstract;
    procedure Process (Item : in Node; Delta_Time : in Long_Float) is null;

    type Process_Callback_Access is access procedure (Item : in Node'class; Delta_Time : in Long_Float);

    function Construct is new Ada.Tags.Generic_Dispatching_Constructor (T => Base, Parameters => No_Parameters, Constructor => Constructor);

    -------------------------
    -- Object Registration --
    -------------------------
    generic
      type T (<>) is new Object with private;
    package Object_Registration is
    
      procedure Register_Class;

    -------
    private
    -------
      function Create (
        p_instance    : System.Address;
        p_method_data : System.Address)
        return System.Address
        with Convention => C;
      procedure Destroy (
        p_instance    : System.Address;
        p_method_data : System.Address;
        p_user_data   : System.Address)
        with Convention => C;
    end;

    -----------------------
    -- Node Registration --
    -----------------------
    generic
      type T (<>) is new Node with private;
    package Node_Registration is

      procedure Register_Process;

    -------
    private
    -------

      function Process (
        p_instance    : System.Address;
        p_method_data : System.Address;
        p_user_data   : System.Address;
        p_num_args    : Interfaces.C.int;
        p_args        : Godot_Instance_Method_Args_Ptrs.Pointer) -- godot_variant **
        return godot_variant 
        with Convention => C;

    end;

end;