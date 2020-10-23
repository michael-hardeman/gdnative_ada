with GDNative.Thick; use GDNative.Thick;

package Adventure is

  type Player is new Objects.Node with record
    Health : Natural := 100;
  end record;
  overriding function Constructor (Parameters : not null access Objects.No_Parameters) return Player is (Health => 100);
  overriding procedure Process (Item : in Player; Delta_Time : in Long_Float);

  procedure Register_Classes;

end;
