with System;
with GDNative.Objects; use GDNative.Objects;

package Action_RPG is

  type Player is new Node with record
    Health : Integer;
  end record;
  for Player'External_Tag use "Player";
  overriding function Initialize return Player is (Player'(Health => 100));
  overriding procedure Physics_Process (Self : in out Player; Delta_Time : in Long_Float);
  package Player_Registration is new Node_Registration (Player);

  procedure Register_Classes;

end;
