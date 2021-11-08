with Ada.Containers.Doubly_Linked_Lists;
with Ada.Numerics;
with Ada.Text_IO;

with Concorde.Calendar;
with Concorde.Elementary_Functions;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Real_Images;

with Concorde.Constants;

with Concorde.Worlds;

with Concorde.Handles.Account;
with Concorde.Handles.Commodity;
with Concorde.Handles.Container_Component;
with Concorde.Handles.Drive_Component;
with Concorde.Handles.Faction;
with Concorde.Handles.Ship_Component;
with Concorde.Handles.Ship_Design;
with Concorde.Handles.Ship_Module;
with Concorde.Handles.Ship_Module_Design;
with Concorde.Handles.World;

package body Concorde.Ships is

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Owner   : Concorde.Handles.Faction.Faction_Handle;
      World   : Concorde.Handles.World_Reference;
      Design  : Concorde.Handles.Ship_Design_Reference;
      Manager : String;
      Name    : String)
   is
      World_Rec : constant Concorde.Handles.World.World_Type :=
                    Concorde.Handles.World.Get (World);
      Account      : constant Concorde.Handles.Account_Reference :=
                       Concorde.Handles.Account.Create
                         (Concorde.Handles.Null_Account_Reference,
                          Concorde.Money.Zero,
                          Concorde.Money.Zero);
      Ship         : constant Concorde.Handles.Ship_Reference :=
                       Concorde.Handles.Ship.Create
                         (Name              => Name,
                          Capacity          =>
                            Concorde.Handles.Ship_Design.Get (Design).Hold_Size,
                          Account           => Account,
                          Faction           => Owner,
                          Active            => True,
                          Scheduled         => False,
                          Next_Event        => Concorde.Calendar.Clock,
                          Manager           => Manager,
                          Owner             =>
                            Concorde.Handles.Faction.Get (Owner).Get_Owner_Reference,
                          World             => World,
                          Star_System       => World_Rec.Star_System,
                          Orbit             =>
                            World_Rec.Radius
                          + (300.0 + 100.0 * Concorde.Random.Unit_Random)
                          * 1000.0,
                          Inclination       =>
                            Concorde.Random.Unit_Random * 10.0 - 5.0,
                          Epoch             => Concorde.Calendar.Clock,
                          Start_Longitude   =>
                            Concorde.Random.Unit_Random * 360.0,
                          Ship_Design       => Design,
                          Alive             => True,
                          Training          => 0.0,
                          Fuel              => 0.0,
                          Departure         => Concorde.Calendar.Clock,
                          Arrival           => Concorde.Calendar.Clock,
                          Dest_Orbit        => 0.0,
                          Dest_Incl         => 0.0,
                          Dest_Epoch        => Concorde.Calendar.Clock,
                          Final_Destination => Concorde.Handles.Null_World_Reference,
                          Current_Order     => 0,
                          Cycle_Orders      => False);
   begin
      for Design_Component of
        Concorde.Handles.Ship_Module_Design.Select_By_Ship_Design
          (Design)
      loop
         Concorde.Handles.Ship_Module.Create
           (Ship           => Ship,
            Ship_Component => Design_Component.Ship_Component,
            Crew           => 0,
            Condition      => 0.0,
            Tec_Level      => 0.0);
      end loop;

      if False then
         declare
            S : constant Ship_Type := Get (Ship);
         begin
            Ada.Text_IO.Put_Line
              (S.Name & " in orbit above "
               & Concorde.Worlds.Name (S.World)
               & ": altitude "
               & Concorde.Real_Images.Approximate_Image
                 ((S.Orbit - Concorde.Worlds.Radius (S.World)) / 1_000.0)
               & "km inclination "
               & Concorde.Real_Images.Approximate_Image
                 (S.Inclination)
               & " deg longitude "
               & Concorde.Real_Images.Approximate_Image
                 (S.Current_Longitude)
               & " E");
         end;
      end if;
   end Create_Ship;

   -----------------------
   -- Current_Longitude --
   -----------------------

   function Current_Longitude
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
      use Concorde.Calendar;
      use Concorde.Elementary_Functions;
      Orbit       : constant Non_Negative_Real := Ship.Orbit;
      World_Mass  : constant Non_Negative_Real :=
                     Concorde.Worlds.Mass (Ship.World);
      Period      : constant Non_Negative_Real :=
                     Sqrt (4.0 * Ada.Numerics.Pi * Orbit ** 3
                           / Concorde.Constants.Gravitational_Constant
                           / World_Mass);
      Start       : constant Non_Negative_Real :=
                     Concorde.Handles.Ship.Get (Ship.Reference).Start_Longitude;
      Epoch       : constant Time :=
                     Concorde.Handles.Ship.Get (Ship.Reference).Epoch;
      Now         : constant Time := Clock;
      Elapsed     : constant Concorde_Duration := Now - Epoch;
      Orbit_Count : constant Non_Negative_Real := Real (Elapsed) / Period;
      Partial     : constant Unit_Real :=
                      Orbit_Count - Real'Truncation (Orbit_Count);
      Longitude   : Non_Negative_Real := Start + Partial * 360.0;
   begin
      if Longitude >= 360.0 then
         Longitude := Longitude - 360.0;
      end if;
      return Longitude;
   end Current_Longitude;

   ------------------
   -- Current_Mass --
   ------------------

   function Current_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Ship.Dry_Mass;
   end Current_Mass;

   -------------------------
   -- Design_Cargo_Volume --
   -------------------------

   function Design_Cargo_Volume
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real
   is
      Volume : Non_Negative_Real := 0.0;
   begin
      for Module of
        Concorde.Handles.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Concorde.Db, Concorde.Handles.Ship_Component;
            Rec_Type : constant Record_Type :=
                         Get (Module.Ship_Component).Top_Record;
         begin
            if Rec_Type = R_Container_Component then
               declare
                  use Concorde.Handles.Container_Component;
                  Container : constant Container_Component_Type :=
                                Get_Container_Component
                                  (Module.Ship_Component);
               begin
                  if not Container.Liquid
                    and then not Container.Cryo
                  then
                     Volume := Volume
                       + Concorde.Quantities.To_Real
                       (Container.Capacity);
                  end if;
               end;
            end if;
         end;
      end loop;
      return Volume;
   end Design_Cargo_Volume;

   --------------------
   -- Design_Delta_V --
   --------------------

   function Design_Delta_V
     (Design     : Concorde.Handles.Ship_Design_Reference;
      Cargo_Mass : Non_Negative_Real)
      return Non_Negative_Real
   is
      Ve : Non_Negative_Real := 0.0;
   begin
      for Module of
        Concorde.Handles.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Concorde.Db, Concorde.Handles.Ship_Component;
            Rec_Type : constant Record_Type :=
                         Get (Module.Ship_Component).Top_Record;
         begin
            if Rec_Type = R_Drive_Component then
               declare
                  use Concorde.Handles.Drive_Component;
                  Drive : constant Drive_Component_Type :=
                                Get_Drive_Component
                              (Module.Ship_Component);
               begin
                  Ve := Drive.Exhaust_Velocity;
                  exit;
               end;
            end if;
         end;
      end loop;

      declare
         use Concorde.Elementary_Functions;
         Dry_Mass : constant Non_Negative_Real :=
                      Design_Mass (Design) + Cargo_Mass;
         Fuel_Mass : constant Non_Negative_Real := Design_Fuel_Mass (Design);
         Full_Mass : constant Non_Negative_Real := Dry_Mass + Fuel_Mass;
      begin
         return Ve * Log (Full_Mass / Dry_Mass);
      end;
   end Design_Delta_V;

   ----------------------
   -- Design_Fuel_Mass --
   ----------------------

   function Design_Fuel_Mass
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real
   is
      type Consumption_Record is
         record
            Commodity       : Concorde.Handles.Commodity.Commodity_Class;
            Liquid          : Boolean;
            Cryo            : Boolean;
            Density         : Non_Negative_Real;
            Kg_Per_Second   : Non_Negative_Real;
            Relative_Mass   : Non_Negative_Real;
            Relative_Volume : Non_Negative_Real;
         end record;

      package Consumption_Record_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Consumption_Record);

      List : Consumption_Record_Lists.List;

      procedure Update_Fuel
        (Fuel          : Concorde.Handles.Commodity.Commodity_Class;
         Kg_Per_Second : Non_Negative_Real);

      -----------------
      -- Update_Fuel --
      -----------------

      procedure Update_Fuel
        (Fuel          : Concorde.Handles.Commodity.Commodity_Class;
         Kg_Per_Second : Non_Negative_Real)
      is
         use Concorde.Db;
         Rec : constant Concorde.Handles.Commodity.Commodity_Type :=
                 Commodity.Get (Fuel);
      begin
         for Item of List loop
            if Item.Commodity = Fuel then
               Item.Kg_Per_Second :=
                 Item.Kg_Per_Second + Kg_Per_Second;
               return;
            end if;
         end loop;
         List.Append
           (Consumption_Record'
              (Commodity       => Fuel,
               Liquid          => True,
               Cryo            => True,
               Density         => Rec.Density,
               Kg_Per_Second   => Kg_Per_Second,
               Relative_Mass   => 0.0,
               Relative_Volume => 0.0));
      end Update_Fuel;

      Liquid_Tank_Size : Non_Negative_Real := 0.0;
      Cryo_Tank_Size   : Non_Negative_Real := 0.0;
      Liquid_Fuel_Mass : Non_Negative_Real := 0.0;
      Cryo_Fuel_Mass   : Non_Negative_Real := 0.0;
   begin
      for Module of
        Concorde.Handles.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Concorde.Db, Concorde.Handles.Ship_Component;
            Rec_Type : constant Record_Type :=
                         Get (Module.Ship_Component).Top_Record;
         begin
            if Rec_Type = R_Drive_Component then
               declare
                  Drive : constant Drive_Component.Drive_Component_Type :=
                            Drive_Component.Get_Drive_Component
                              (Module.Ship_Component);
               begin
                  if Drive.Fuel /= Null_Commodity_Reference then
                     Update_Fuel (Drive.Fuel, Drive.Max_Fuel_Burn);
                  end if;

                  if Drive.Oxidiser /= Null_Commodity_Reference then
                     Update_Fuel (Drive.Oxidiser, Drive.Max_Oxidiser_Burn);
                  end if;
               end;
            elsif Rec_Type = R_Container_Component then
               declare
                  use Concorde.Handles.Container_Component;
                  Container : constant Container_Component_Type :=
                                Get_Container_Component
                                  (Module.Ship_Component);
               begin
                  if Container.Liquid then
                     if Container.Cryo then
                        Cryo_Tank_Size := Cryo_Tank_Size
                          + Concorde.Quantities.To_Real
                          (Container.Capacity);
                     else
                        Liquid_Tank_Size := Liquid_Tank_Size
                          + Concorde.Quantities.To_Real
                          (Container.Capacity);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;

      declare
         Total_Liquid_Mass_Consumption   : Non_Negative_Real := 0.0;
         Total_Cryo_Mass_Consumption     : Non_Negative_Real := 0.0;
         Total_Liquid_Volume_Consumption : Non_Negative_Real := 0.0;
         Total_Cryo_Volume_Consumption   : Non_Negative_Real := 0.0;
      begin
         for Item of List loop
            if Item.Liquid then
               if Item.Cryo then
                  Total_Cryo_Mass_Consumption :=
                    Total_Cryo_Mass_Consumption + Item.Kg_Per_Second;
                  Total_Cryo_Volume_Consumption :=
                    Total_Cryo_Volume_Consumption
                    + Item.Kg_Per_Second / Item.Density;
               else
                  Total_Liquid_Mass_Consumption :=
                    Total_Liquid_Mass_Consumption + Item.Kg_Per_Second;
                  Total_Liquid_Volume_Consumption :=
                    Total_Liquid_Volume_Consumption +
                      Item.Kg_Per_Second / Item.Density;
               end if;
            end if;
         end loop;

         for Item of List loop
            if Item.Liquid then
               if Item.Cryo then
                  declare
                     use Concorde.Real_Images;
                     function Img (X : Real) return String
                                   renames Approximate_Image;

                     This_Vol_Per_Second : constant Non_Negative_Real :=
                                             Item.Kg_Per_Second
                                               / Item.Density;
                     Partial_Volume      : constant Non_Negative_Real :=
                                             This_Vol_Per_Second
                                               / Total_Cryo_Volume_Consumption;
                     Required_Volume     : constant Non_Negative_Real :=
                                             Partial_Volume
                                               * Cryo_Tank_Size;
                     Required_Mass       : constant Non_Negative_Real :=
                                             Required_Volume
                                               * Item.Density;
                  begin
                     if False then
                        Ada.Text_IO.Put_Line
                          (Concorde.Handles.Commodity.Get (Item.Commodity).Tag
                           & ": density: "
                           & Img (Item.Density)
                           & "kg/m3; consumption: "
                           & Img (Item.Kg_Per_Second)
                           & "kg/s; vol. consumption: "
                           & Img (This_Vol_Per_Second)
                           & "m3/s; partial vol: "
                           & Img (Partial_Volume)
                           & " total vol: "
                           & Img (Required_Volume)
                           & " total mass: "
                           & Img (Required_Mass));
                     end if;

                     Cryo_Fuel_Mass := Cryo_Fuel_Mass + Required_Mass;
                  end;
               else
                  Liquid_Fuel_Mass :=
                    Liquid_Fuel_Mass
                      + Item.Kg_Per_Second / Item.Density
                    * Liquid_Tank_Size;
               end if;
            end if;
         end loop;

         if False then
            Ada.Text_IO.Put_Line
              ("cryo tank size "
               & Concorde.Real_Images.Approximate_Image (Cryo_Tank_Size)
               & "; cryo fuel mass "
               & Concorde.Real_Images.Approximate_Image (Cryo_Fuel_Mass));
         end if;

         return Cryo_Fuel_Mass + Liquid_Fuel_Mass;

      end;

   end Design_Fuel_Mass;

   -----------------
   -- Design_Mass --
   -----------------

   function Design_Mass
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real
   is
      Mass : Non_Negative_Real := 0.0;
   begin
      for Module of
        Concorde.Handles.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Concorde.Handles.Ship_Component;
            Component : constant Ship_Component_Type :=
                          Get (Module.Ship_Component);
         begin
            Mass := Mass + Component.Mass;
         end;
      end loop;
      return Mass;
   end Design_Mass;

   -------------------
   -- Design_Thrust --
   -------------------

   function Design_Thrust
     (Design : Concorde.Handles.Ship_Design_Reference)
      return Non_Negative_Real
   is
      Thrust : Non_Negative_Real := 0.0;
   begin
      for Module of
        Concorde.Handles.Ship_Module_Design.Select_By_Ship_Design (Design)
      loop
         declare
            use Concorde.Db;
            Component : constant Ship_Component.Ship_Component_Type :=
                          Ship_Component.Get (Module.Ship_Component);
         begin
            if Component.Top_Record = R_Drive_Component then
               declare
                  Drive : constant Drive_Component.Drive_Component_Type :=
                            Drive_Component.Get_Drive_Component
                              (Module.Ship_Component);
               begin
                  Thrust := Thrust + Drive.Maximum_Thrust;
               end;
            end if;
         end;
      end loop;
      return Thrust;
   end Design_Thrust;

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Design_Mass
        (Concorde.Handles.Ship.Get (Ship.Reference).Ship_Design);
   end Dry_Mass;

   ------------------
   -- Total_Thrust --
   ------------------

   function Total_Thrust
     (Ship : Ship_Type'Class)
      return Non_Negative_Real
   is
   begin
      return Design_Thrust
        (Concorde.Handles.Ship.Get (Ship.Reference).Ship_Design);
   end Total_Thrust;

end Concorde.Ships;
