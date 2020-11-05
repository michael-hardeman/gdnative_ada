with System;
with Interfaces.C;
with GDNative.Thin;

package GDNative.Objects is

  ------------
  -- Object --
  ------------
  type Object is abstract tagged null record;
  function Initialize return Object is abstract;

  ----------
  -- Node --
  ----------
  type Node is abstract new Object with null record;
  procedure Enter_Tree      (Self : in out Node);
  procedure Exit_Tree       (Self : in out Node);
  procedure Ready           (Self : in out Node);
  procedure Process         (Self : in out Node; Delta_Time : in Long_Float);
  procedure Physics_Process (Self : in out Node; Delta_Time : in Long_Float);
  
  -------------------------
  -- Object Registration --
  -------------------------
  generic
    type New_Object (<>) is new Object with private;
  package Object_Registration is
    procedure Register_Class;
  end;

  -----------------------
  -- Node Registration --
  -----------------------
  generic
    type New_Node (<>) is new Node with private;
  package Node_Registration is
    procedure Register_Class;
    procedure Register_Enter_Tree;
    procedure Register_Exit_Tree;
    procedure Register_Ready;
    procedure Register_Process;
    procedure Register_Physics_Process;
  -------
  private
  -------
    generic
      Method_Name     : String;
      Instance_Method : Thin.access_godot_instance_method;
    procedure Register_Instance_Method;
  end;

-------
private
-------
  
  generic
    type New_Object (<>) is new Object with private;
    type Access_Object is access New_Object;
    with function Cast (Address : System.Address) return Access_Object;
    type Parameters_State is private;
    type Result_Type is private;
    with procedure Convert_Parameters (
      Parameters : in out Parameters_State; 
      p_args     : in     Thin.Godot_Instance_Method_Args_Ptrs.Pointer);
    with procedure Call_Callback (
      Instance   : in out New_Object;
      Result     : in out Result_Type;
      Parameters : in     Parameters_State);
    with function  Convert_Result (
      Result : in Result_Type) 
      return Thin.godot_variant;
  function Wrap_Instance_Method (
    p_instance    : System.Address;
    p_method_data : System.Address;
    p_user_data   : System.Address;
    p_num_args    : Interfaces.C.int;
    p_args        : Thin.Godot_Instance_Method_Args_Ptrs.Pointer) -- godot_variant **
    return Thin.godot_variant;

end;