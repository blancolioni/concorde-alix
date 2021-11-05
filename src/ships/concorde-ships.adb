with Concorde.Calendar;
with Concorde.Identifiers;
with Concorde.Orbits;
with Concorde.Random;

with Concorde.Ship_Designs;

with Accord.Db;

package body Concorde.Ships is

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Owner   : Accord.Faction.Faction_Class;
      Home    : Accord.Colony.Colony_Class;
      World   : Accord.World.World_Class;
      Design  : Accord.Ship_Design.Ship_Design_Class;
      Manager : String;
      Name    : String)
   is
      use type Concorde.Calendar.Time;
      Orbit  : constant Non_Negative_Real :=
                 World.Radius
                   + (300.0 + Concorde.Random.Normal_Random * 10.0)
                 * 1000.0;
      Period : constant Non_Negative_Real :=
                 Concorde.Orbits.Period (World.Mass, Orbit);
      Epoch  : constant Concorde.Calendar.Time :=
                 Concorde.Calendar.Clock
                   + Concorde_Duration
        (Concorde.Random.Unit_Random * Period);
   begin
      Accord.Ship.Create
        (Name            => Name,
         Active          => True,
         Scheduled       => True,
         Next_Event      => Concorde.Calendar.Clock,
         Manager         => Manager,
         Primary_Massive => World,
         Semimajor_Axis  => Orbit,
         Epoch           => Epoch,
         Eccentricity    => 0.0,
         Period          => Period,
         Identifier      => Concorde.Identifiers.Next_Identifier,
         Mass            => Concorde.Ship_Designs.Dry_Mass (Design),
         Faction         => Owner,
         Home            => Home,
         World           => World,
         Status          => Accord.Db.Idle,
         Training        => 0.0,
         Fuel            => Design.Fuel_Tank,
         Destination     => Accord.World.Empty_Handle);
   end Create_Ship;

end Concorde.Ships;
