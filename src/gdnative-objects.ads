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
  procedure Process (Self : in out Node; Delta_Time : in Long_Float) is null;

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
    procedure Register_Process;
  end;

end;