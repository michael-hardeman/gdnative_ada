with GDNative.Objects; use GDNative.Objects;

package Action_RPG is

  procedure Initialize;
  procedure Register_Classes;
  procedure Finalize;

  type Player is new Node with record
    Health : Integer;
  end record with External_Tag => "Player";
  overriding function Initialize return Player is ((Health => 100));
  overriding procedure Physics_Process (Self : in out Player; Delta_Time : in GDNative.Real_64);

  package Player_Registration is new Node_Registration (Player);
end;
