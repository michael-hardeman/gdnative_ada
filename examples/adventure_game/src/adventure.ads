with System;
with GDNative.Thick;         use GDNative.Thick;
with GDNative.Thick.Objects; use GDNative.Thick.Objects;

package Adventure is

  type Player is new Node with record
    Health : Natural;
  end record;
  overriding function Constructor (Parameters : not null access No_Parameters) return Player is (Player'(Health => 100));
  overriding procedure Process (Item : in Player; Delta_Time : in Long_Float);

  package Player_Class_Registration is new Object_Registration (T => Player);
  package Player_Node_Registration is new Node_Registration (T => Player);

  procedure Register_Classes;

end;
