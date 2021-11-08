with Concorde.Calendar;
with Concorde.Identifiers;
with Concorde.Orbits;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Stock;

with Concorde.Ship_Designs;

with Accord.Db;

with Accord.Commodity;
with Accord.Manager;

with Accord.Ship_Design_Module;
with Accord.Jump_Drive;
with Accord.Jump_Module;

package body Concorde.Ships is

   procedure Create_Ship_Module
     (Ship          : Accord.Ship.Ship_Class;
      Design_Module : Accord.Ship_Design_Module.Ship_Design_Module_Class);

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
                   + (800.0 + Concorde.Random.Normal_Random * 10.0)
                 * 1000.0;
      Period : constant Concorde_Duration :=
                 Concorde.Orbits.Period (World.Mass, Orbit);
      Epoch  : constant Concorde.Calendar.Time :=
                 Concorde.Calendar.Clock
                   + Concorde_Duration
        (Concorde.Random.Unit_Random * Real (Period));
      Ship : constant Accord.Ship.Ship_Handle :=
               Accord.Ship.Create
                 (Name            => Name,
                  Primary_Massive => World,
                  Semimajor_Axis  => Orbit,
                  Epoch           => Epoch,
                  Eccentricity    => 0.0,
                  Period          => Real (Period),
                  Identifier      => Concorde.Identifiers.Next_Identifier,
                  Mass            => Concorde.Ship_Designs.Dry_Mass (Design),
                  Ship_Design     => Design,
                  Faction         => Owner,
                  Home            => Home,
                  World           => World,
                  Star_System     => World.Star_System,
                  Status          => Accord.Db.Idle,
                  Training        => 0.0,
                  Fuel            => Design.Fuel_Tank,
                  Destination     => Accord.World.Empty_Handle);
      Start_Delay : constant Concorde_Duration :=
                      Concorde_Duration
                        (Real (Concorde.Calendar.Days (1))
                         * Concorde.Random.Unit_Random);
   begin

      for Design_Module of
        Accord.Ship_Design_Module.Select_By_Ship_Design (Design)
      loop
         Create_Ship_Module (Ship, Design_Module);
      end loop;

      Accord.Manager.Create
        (Managed    => Ship,
         Active     => True,
         Scheduled  => True,
         Next_Event => Concorde.Calendar.Clock + Start_Delay,
         Manager    => Manager);
   end Create_Ship;

   ------------------------
   -- Create_Ship_Module --
   ------------------------

   procedure Create_Ship_Module
     (Ship          : Accord.Ship.Ship_Class;
      Design_Module : Accord.Ship_Design_Module.Ship_Design_Module_Class)
   is
      use all type Accord.Db.Record_Type;
   begin
      case Design_Module.Component.Top_Record is
         when R_Jump_Drive =>
            Accord.Jump_Module.Create
              (Ship       => Ship,
               Component  => Design_Module.Component,
               Condition  => 1.0,
               Size       => Design_Module.Size,
               Mass       => Design_Module.Mass,
               Jump_Drive =>
                  Accord.Jump_Drive.Get_From_Component
                 (Design_Module.Component));
         when others =>
            null;
      end case;
   end Create_Ship_Module;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass
     (Ship : Ship_Class)
      return Non_Negative_Real
   is
   begin
      return Mass : Non_Negative_Real :=
        Concorde.Ship_Designs.Dry_Mass (Ship.Ship_Design)
      do
         Mass := Mass + Ship.Fuel;

         declare
            procedure Add_Mass
              (Commodity : Accord.Commodity.Commodity_Class;
               Quantity  : Concorde.Quantities.Quantity_Type);

            --------------
            -- Add_Mass --
            --------------

            procedure Add_Mass
              (Commodity : Accord.Commodity.Commodity_Class;
               Quantity  : Concorde.Quantities.Quantity_Type)
            is
            begin
               Mass := Mass
                 + Commodity.Mass * Concorde.Quantities.To_Real (Quantity);
            end Add_Mass;

         begin
            Concorde.Stock.Iterate (Ship, Add_Mass'Access);
         end;
      end return;
   end Current_Mass;

   ----------------
   -- Jump_Speed --
   ----------------

   function Jump_Speed
     (Ship : Ship_Class)
      return Non_Negative_Real
   is
      Jump : constant Accord.Jump_Module.Jump_Module_Class :=
               Accord.Jump_Module.First_By_Ship (Ship);
   begin
      if not Jump.Has_Element then
         return 0.0;
      else
         return Jump.Jump_Drive.Jump
           * Jump.Condition ** 2
           * 100_000.0
           / Current_Mass (Ship);
      end if;
   end Jump_Speed;

end Concorde.Ships;
