with Concorde.Calendar;
with Concorde.Constants;
with Concorde.Elementary_Functions;
with Concorde.Logging;
with Concorde.Markets;
with Concorde.Money;
with Concorde.Orbits;
with Concorde.Quantities;
with Concorde.Random;
with Concorde.Real_Images;
with Concorde.Solar_System;
with Concorde.Star_Systems;
with Concorde.Trigonometry;

with Concorde.Colonies;

with Accord.Colony_Request;
with Accord.Colony_Supply;
with Accord.Star_System;

package body Concorde.Ships.Managers is

   use type Concorde.Calendar.Time;

   AU : constant := Concorde.Solar_System.Earth_Orbit;

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                  renames Concorde.Elementary_Functions.Sqrt;

   type Default_Trader is new Root_Ship_Manager with
      record
         null;
      end record;

   overriding function Name
     (Manager : Default_Trader)
      return String
   is (Manager.Ship.Name & "/default-trader");

   overriding procedure Activate
     (Manager : in out Default_Trader);

   procedure Accept_Request
     (Manager : in out Default_Trader'Class;
      Request : Accord.Colony_Request.Colony_Request_Class);

   procedure Add_Trade_Goods_Route
     (Manager     : in out Default_Trader'Class;
      Destination : Accord.Colony.Colony_Class);

   procedure Start_Stellar_Journey
     (Manager        : in out Root_Ship_Manager'Class;
      New_Status     : Accord.Db.Ship_Status;
      From_Orbit     : Non_Negative_Real;
      From_Longitude : Concorde.Trigonometry.Angle;
      From_Latitude  : Concorde.Trigonometry.Angle;
      To_Orbit       : Non_Negative_Real;
      To_Longitude   : Concorde.Trigonometry.Angle;
      To_Latitude    : Concorde.Trigonometry.Angle);

   --------------------
   -- Accept_Request --
   --------------------

   procedure Accept_Request
     (Manager : in out Default_Trader'Class;
      Request : Accord.Colony_Request.Colony_Request_Class)
   is
      use Concorde.Quantities;
      Available_Space : constant Quantity_Type :=
                          To_Quantity (Manager.Ship.Ship_Design.Cargo_Space);
      Transport       : constant Quantity_Type :=
                          Min (Available_Space, Request.Remaining);
   begin
      Request.Update_Colony_Request
        .Set_Remaining (Request.Remaining - Transport)
        .Done;
      Accord.Colony_Supply.Create
        (Colony_Request => Request,
         Quantity       => Transport,
         Price          => Request.Offer);
      Manager.Set_Destination (Request.Colony.World);
   end Accept_Request;

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Manager : in out Default_Trader)
   is
      use Concorde.Money, Concorde.Quantities;

      Current_Star_System : constant Accord.Star_System.Star_System_Class :=
                              Manager.Ship.World.Star_System;
      Current_Colony      : constant Accord.Colony.Colony_Class :=
                              Accord.Colony.First_By_World
                                (Manager.Ship.World);
      Available_Space     : constant Quantity_Type :=
                          To_Quantity (Manager.Ship.Ship_Design.Cargo_Space);

      function Score
        (Request : Accord.Colony_Request.Colony_Request_Class)
         return Non_Negative_Real;

      -----------
      -- Score --
      -----------

      function Score
        (Request : Accord.Colony_Request.Colony_Request_Class)
         return Non_Negative_Real
      is
         Destination : constant Accord.Star_System.Star_System_Class :=
                         Request.Colony.World.Star_System;
         Distance    : constant Non_Negative_Real :=
                         (Destination.X - Current_Star_System.X) ** 2
                         + (Destination.Y - Current_Star_System.Y) ** 2
                         + (Destination.Z - Current_Star_System.Z) ** 2;
         Transport   : constant Quantity_Type :=
                         Min (Available_Space, Request.Remaining);
         Earn        : constant Money_Type :=
                         Total (Request.Offer, Transport);
      begin
         if Request.Colony.World.Identifier
           = Manager.Ship.World.Identifier
           or else Concorde.Colonies.Current_Supply (Current_Colony,
                                                     Request.Commodity)
           < Available_Space
         then
            return 0.0;
         else
            return To_Real (Earn) / (0.1 + Distance);
         end if;
      end Score;

      Best_Request : Accord.Colony_Request.Colony_Request_Handle;
      Best_Score   : Non_Negative_Real := 0.0;
   begin
      Manager.Log ("scanning colony requests");
      for Request of
        Accord.Colony_Request.Select_By_Active (True)
      loop
         declare
            This_Score : constant Non_Negative_Real :=
                           Score (Request);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Best_Request := Request.To_Colony_Request_Handle;
            end if;
         end;
      end loop;

      if Best_Request.Has_Element then

         Manager.Log ("accepting: "
                      & Show (Best_Request.Quantity)
                      & " "
                      & Best_Request.Commodity.Tag
                      & " to "
                      & Best_Request.Colony.Faction.Adjective
                      & " colony on "
                      & Best_Request.Colony.World.Name);

         Manager.Accept_Request (Best_Request);
         return;
      end if;

      Manager.Log ("scanning general trade goods");

      declare
         Classification : constant Concorde.Markets.Trade_Classification :=
                            Concorde.Markets.Classify_Colony
                              (Accord.Colony.First_By_World
                                 (Manager.Ship.World));
         Cost_Of_Goods  : constant Concorde.Money.Price_Type :=
                            Concorde.Markets.Cost_Of_Goods (Current_Colony);

         Best_Score     : Non_Negative_Real := 0.0;
         Best_Destination : Accord.Colony.Colony_Handle;

         function Score
           (Colony : Accord.Colony.Colony_Class)
            return Non_Negative_Real;

         -----------
         -- Score --
         -----------

         function Score
           (Colony : Accord.Colony.Colony_Class)
            return Non_Negative_Real
         is
            use type Concorde.Money.Price_Type;
            Base_Price : constant Concorde.Money.Price_Type :=
                           Concorde.Markets.Base_Price_Of_Goods
                             (Colony, Classification);
         begin
            Manager.Log
              ("destination "
               & Colony.World.Name
               & ": classification "
               & Concorde.Markets.Show (Classification)
               & "; price of goods "
               & Concorde.Money.Show (Base_Price));
            if Base_Price > Cost_Of_Goods then
               return Concorde.Money.To_Real (Base_Price)
                 / Concorde.Money.To_Real (Cost_Of_Goods);
            else
               return 0.0;
            end if;
         end Score;

      begin
         Manager.Log ("current classification: "
                      & Concorde.Markets.Show (Classification));
         Manager.Log ("cost of goods: "
                      & Concorde.Money.Show (Cost_Of_Goods));

         for Colony of Accord.Colony.Scan_By_Top_Record loop
            if Colony.Identifier /= Current_Colony.Identifier then
               declare
                  This_Score : constant Non_Negative_Real :=
                                 Score (Colony);
               begin
                  if This_Score > Best_Score then
                     Best_Score := This_Score;
                     Best_Destination := Colony.To_Colony_Handle;
                  end if;
               end;
            end if;
         end loop;

         if Best_Destination.Has_Element then
            Manager.Log ("best destination:       "
                         & Concorde.Markets.Show
                           (Concorde.Markets.Classify_Colony
                              (Best_Destination)));
            Manager.Log ("trading to " & Best_Destination.World.Name);
            Manager.Add_Trade_Goods_Route (Best_Destination);
            return;
         end if;
      end;

      Manager.Log ("no good destinations");

   end Activate;

   ---------------------------
   -- Add_Trade_Goods_Route --
   ---------------------------

   procedure Add_Trade_Goods_Route
     (Manager     : in out Default_Trader'Class;
      Destination : Accord.Colony.Colony_Class)
   is
      use Concorde.Quantities;
      Available_Space : constant Quantity_Type :=
                          To_Quantity (Manager.Ship.Ship_Design.Cargo_Space)
                          with Unreferenced;
   begin
      Manager.Set_Destination (Destination.World);
   end Add_Trade_Goods_Route;

   ---------------------
   -- Arrive_At_World --
   ---------------------

   procedure Arrive_At_World
     (Manager : in out Root_Ship_Manager'Class)
   is
      World : constant Accord.World.World_Class :=
                Manager.Ship.Destination;
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
   begin
      Manager.Ship.Update_Ship
        .Set_World (Manager.Ship.Destination)
        .Set_Destination (Accord.World.Empty_Handle)
        .Set_Semimajor_Axis (Orbit)
        .Set_Epoch (Epoch)
        .Set_Period (Real (Period))
        .Done;
      Manager.Idle;
   end Arrive_At_World;

   ---------------------------
   -- Create_Default_Trader --
   ---------------------------

   function Create_Default_Trader
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
   is
   begin
      return Default_Trader'
        (Concorde.Managers.Root_Concorde_Manager with
           Ship        => Accord.Ship.Get_From_Managed (Managed),
           Destination => Accord.World.Empty_Handle);
   end Create_Default_Trader;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Manager : in out Root_Ship_Manager) is
      use all type Accord.Db.Ship_Status;
   begin

      Manager.Log ("executing: status = " & Manager.Ship.Status'Image);

      case Manager.Ship.Status is
         when Idle =>
            Root_Ship_Manager'Class (Manager).Activate;

         when Activating =>
            if Manager.Destination.Has_Element then
               Manager.Move_To_Jump_Point;
            else
               Manager.Idle;
            end if;

         when Moving_To_Jump_Point =>
            Manager.Start_Jump;

         when Jumping =>
            Manager.Move_To_World;

         when Moving_To_World =>
            Manager.Arrive_At_World;

         when Surveying =>
            null;

         when Training =>
            null;

         when Repairing =>
            null;

         when Destroyed =>
            null;
      end case;

   end Execute;

   ----------
   -- Idle --
   ----------

   procedure Idle
     (Manager : in out Root_Ship_Manager'Class)
   is
   begin
      Manager.Log ("idling");
      Manager.Ship.Update_Ship
        .Set_Status (Accord.Db.Idle)
        .Done;
      Manager.Update_With_Delay (Concorde.Calendar.Hours (1));
   end Idle;

   ------------------------
   -- Move_To_Jump_Point --
   ------------------------

   procedure Move_To_Jump_Point
     (Manager : in out Root_Ship_Manager'Class)
   is
      use Concorde.Constants;
      use Concorde.Solar_System;

      use Concorde.Trigonometry;

      World : constant Accord.World.World_Class :=
                Manager.Ship.World;
      Primary_Mass : constant Non_Negative_Real :=
                       World.Primary.Mass;
      Jump_Distance : constant Non_Negative_Real :=
                        Sqrt (Gravitational_Constant * Primary_Mass
                              / 0.001);
      Current_Distance : constant Non_Negative_Real :=
                           Manager.Ship.World.Semimajor_Axis;
      Target_Distance  : constant Non_Negative_Real :=
                           (if Current_Distance < Jump_Distance
                            then Jump_Distance
                            else Current_Distance
                            + Sqrt (Gravitational_Constant * World.Mass
                              / 0.001));
      Journey_Time     : constant Non_Negative_Real :=
                           Sqrt (2.0 * (Target_Distance - Current_Distance)
                                 / 10.0);
      Journey_Duration : constant Concorde_Duration :=
                           Concorde_Duration (Journey_Time);
      Stellar_Lat, Stellar_Long : Angle;
   begin
      Manager.Log
        ("moving to jump point: current orbit "
         & Image (Current_Distance / Earth_Orbit)
         & "AU; target orbit "
         & Image (Target_Distance / Earth_Orbit)
         & "AU; distance "
         & Image ((Target_Distance - Current_Distance) / Earth_Orbit)
         & "AU; journey time "
         & Image (Journey_Time / 3600.0)
         & "h"
         & "; arriving "
         & Concorde.Calendar.Image
           (Concorde.Calendar.Clock + Journey_Duration, True));

      Concorde.Orbits.Calculate_Current_Position
        (Object    => World,
         Latitude  => Stellar_Lat,
         Longitude => Stellar_Long);

      Manager.Ship.Update_Ship
        .Set_Status (Accord.Db.Moving_To_Jump_Point)
        .Set_Departure (Concorde.Calendar.Clock)
        .Set_Arrival (Concorde.Calendar.Clock + Journey_Duration)
        .Set_World (Accord.World.Empty_Handle)
        .Set_Semimajor_Axis (0.0)
        .Set_Period (0.0)
        .Set_From_X (Cos (Stellar_Long) * Cos (Stellar_Lat) * Current_Distance)
        .Set_From_Y (Sin (Stellar_Long) * Cos (Stellar_Lat) * Current_Distance)
        .Set_From_Z (Sin (Stellar_Lat) * Current_Distance)
        .Set_To_X (Cos (Stellar_Long) * Cos (Stellar_Lat) * Target_Distance)
        .Set_To_Y (Sin (Stellar_Long) * Cos (Stellar_Lat) * Target_Distance)
        .Set_To_Z (Sin (Stellar_Lat) * Target_Distance)
        .Done;
      Manager.Update_With_Delay (Journey_Duration);
   end Move_To_Jump_Point;

   -------------------
   -- Move_To_World --
   -------------------

   procedure Move_To_World
     (Manager : in out Root_Ship_Manager'Class)
   is
      use Concorde.Trigonometry;
      G : constant := Concorde.Constants.Gravitational_Constant;
      World            : constant Accord.World.World_Class :=
                           Manager.Ship.Destination;
      Primary_Mass     : constant Non_Negative_Real :=
                           World.Primary.Mass;
      Current_Orbit    : constant Non_Negative_Real :=
                           Sqrt (G * Primary_Mass / 0.001);
      Target_Orbit     : constant Non_Negative_Real :=
                           World.Semimajor_Axis;
      Current_Longitude : constant Angle :=
                            From_Degrees
                              (360.0 * Concorde.Random.Unit_Random);
      Target_Longitude  : Angle :=
                            Concorde.Orbits.Current_Longitude
                              (World);
      Distance          : Non_Negative_Real :=
                            Sqrt (Current_Orbit ** 2
                                  + Target_Orbit ** 2
                                  - 2.0 * Current_Orbit * Target_Orbit
                                  * Cos (Current_Longitude
                                    - Target_Longitude));
      Journey_Time      : Non_Negative_Real :=
                            2.0 * Sqrt (Distance / 10.0);
      Dest_Longitude    : Angle :=
                            Concorde.Orbits.Get_Longitude_At_Time
                              (World,
                               Concorde.Calendar.Clock
                               + Concorde_Duration (Journey_Time));
   begin
      while abs (To_Degrees (Target_Longitude)
                 - To_Degrees (Dest_Longitude))
        > 0.1
      loop
         Manager.Log
           ("iterating: "
            & " target longitude "
            & Image (Target_Longitude)
            & "; distance "
            & Image (Distance / AU) & "AU"
            & "; journey time "
            & Image (Journey_Time / 3600.0) & " hours"
            & "; new target "
            & Image (Dest_Longitude));
         Target_Longitude := Dest_Longitude;
         Distance := Sqrt (Current_Orbit ** 2
                           + Target_Orbit ** 2
                           - 2.0 * Current_Orbit * Target_Orbit
                           * Cos (Current_Longitude
                             - Target_Longitude));
         Journey_Time :=
           2.0 * Sqrt (Distance / 10.0);

         Dest_Longitude :=
           Concorde.Orbits.Get_Longitude_At_Time
             (World,
              Concorde.Calendar.Clock + Concorde_Duration (Journey_Time));
      end loop;

      Manager.Log ("moving to destination: " & Manager.Destination.Name
                   & ": current orbit ("
                   & Image (Current_Orbit / AU)
                   & "," & Image (To_Degrees (Current_Longitude))
                   & ")"
                   & "; target orbit ("
                   & Image (Target_Orbit / AU)
                   & "," & Image (To_Degrees (Target_Longitude))
                   & ")"
                   & "; distance "
                   & Image (Distance / AU)
                   & "AU; journey time "
                   & Image (Journey_Time / 3600.0)
                   & "h"
                   & "; arriving "
                   & Concorde.Calendar.Image
                     (Concorde.Calendar.Clock
                      + Concorde_Duration (Journey_Time), True));

      Start_Stellar_Journey
        (Manager        => Manager,
         New_Status     => Accord.Db.Moving_To_World,
         From_Orbit     => Current_Orbit,
         From_Longitude => Current_Longitude,
         From_Latitude  => From_Degrees (0.0),
         To_Orbit       => Target_Orbit,
         To_Longitude   => Target_Longitude,
         To_Latitude    => From_Degrees (0.0));
   end Move_To_World;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Manager     : in out Root_Ship_Manager'Class;
      Destination :        Accord.World.World_Class)
   is
   begin
      Manager.Ship.Update_Ship
        .Set_Destination (Destination)
        .Set_Status (Accord.Db.Activating)
        .Done;
      Manager.Destination := Destination.To_World_Handle;
      Manager.Start_Activation;
   end Set_Destination;

   ----------------------
   -- Start_Activation --
   ----------------------

   procedure Start_Activation
     (Manager : in out Root_Ship_Manager'Class)
   is
   begin
      Manager.Ship.Update_Ship
        .Set_Status (Accord.Db.Activating)
        .Done;
      Manager.Update_With_Delay (Concorde.Calendar.Hours (1));
   end Start_Activation;

   ----------------
   -- Start_Jump --
   ----------------

   procedure Start_Jump
     (Manager : in out Root_Ship_Manager'Class)
   is
      From_System : constant Accord.Star_System.Star_System_Class :=
                      Manager.Ship.Star_System;
      To_System   : constant Accord.Star_System.Star_System_Class :=
                      Manager.Ship.Destination.Star_System;
      Distance    : constant Non_Negative_Real :=
                      Concorde.Star_Systems.Distance
                        (From_System, To_System);
      Journey_Time : constant Non_Negative_Real :=
                       Distance * 365.25 * 86_400.0
                         / Concorde.Ships.Jump_Speed (Manager.Ship);
      Journey_Duration : constant Concorde_Duration :=
                           Concorde_Duration (Journey_Time);
   begin
      Manager.Log ("starting jump to "
                   & Manager.Destination.Star_System.Name
                   & "; distance "
                   & Image (Distance) & " light years"
                   & "; jump speed "
                   & Image (Jump_Speed (Manager.Ship))
                   & "; journey time "
                   & Image (Real (Journey_Duration / Calendar.Days (1)))
                   & " days"
                   & "; arriving "
                   & Concorde.Calendar.Image
                     (Concorde.Calendar.Clock + Journey_Duration, True));

      Manager.Ship.Update_Ship
        .Set_Status (Accord.Db.Jumping)
        .Set_Departure (Concorde.Calendar.Clock)
        .Set_Arrival (Concorde.Calendar.Clock + Journey_Duration)
        .Set_Star_System (Accord.Star_System.Empty_Handle)
        .Set_From_X (From_System.X)
        .Set_From_Y (From_System.Y)
        .Set_From_Z (From_System.Z)
        .Set_To_X (To_System.X)
        .Set_To_Y (To_System.Y)
        .Set_To_Z (To_System.Z)
        .Done;

      Manager.Update_With_Delay (Journey_Duration);

   end Start_Jump;

   ---------------------------
   -- Start_Stellar_Journey --
   ---------------------------

   procedure Start_Stellar_Journey
     (Manager        : in out Root_Ship_Manager'Class;
      New_Status     : Accord.Db.Ship_Status;
      From_Orbit     : Non_Negative_Real;
      From_Longitude : Concorde.Trigonometry.Angle;
      From_Latitude  : Concorde.Trigonometry.Angle;
      To_Orbit       : Non_Negative_Real;
      To_Longitude   : Concorde.Trigonometry.Angle;
      To_Latitude    : Concorde.Trigonometry.Angle)
   is
      use Concorde.Trigonometry;
      Ship : Accord.Ship.Ship_Handle renames Manager.Ship;
      X1 : constant Real :=
             Cos (From_Longitude) * Cos (From_Latitude) * From_Orbit;
      X2 : constant Real :=
             Cos (To_Longitude) * Cos (To_Latitude) * To_Orbit;
      Y1 : constant Real :=
             Sin (From_Longitude) * Cos (From_Latitude) * From_Orbit;
      Y2 : constant Real :=
             Sin (To_Longitude) * Cos (To_Latitude) * To_Orbit;
      Z1 : constant Real :=
             Sin (From_Latitude) * From_Orbit;
      Z2 : constant Real :=
             Sin (To_Latitude) * To_Orbit;
      Distance          : constant Non_Negative_Real :=
                            Sqrt ((X1 - X2) ** 2
                                  + (Y1 - Y2) ** 2
                                  + (Z1 - Z2) ** 2);
      Journey_Time      : constant Non_Negative_Real :=
                            2.0 * Sqrt (Distance / 10.0);
      Journey_Duration  : constant Concorde_Duration :=
                            Concorde_Duration (Journey_Time);
   begin
      Concorde.Logging.Log
        (Ship.Name,
         "start stellar journey ("
         & Image (X1 / AU) & ","
         & Image (Y1 / AU) & ","
         & Image (Z1 / AU) & ")"
         & " --> ("
         & Image (X2 / AU) & ","
         & Image (Y2 / AU) & ","
         & Image (Z2 / AU) & ")"
         & "; distance "
         & Image (Distance / AU) & "AU"
         & "; duration "
         & Image (Journey_Time / 3600.0) & " hours"
         & "; arrival at "
         & Concorde.Calendar.Image
           (Concorde.Calendar.Clock + Journey_Duration, True));

      Ship.Update_Ship
        .Set_Status (New_Status)
        .Set_Departure (Concorde.Calendar.Clock)
        .Set_Arrival (Concorde.Calendar.Clock + Journey_Duration)
        .Set_World (Accord.World.Empty_Handle)
        .Set_Semimajor_Axis (0.0)
        .Set_Period (0.0)
        .Set_From_X (X1)
        .Set_From_Y (Y1)
        .Set_From_Z (Z1)
        .Set_To_X (X2)
        .Set_To_Y (Y2)
        .Set_To_Z (Z2)
        .Done;

      Manager.Update_With_Delay (Journey_Duration);

   end Start_Stellar_Journey;

end Concorde.Ships.Managers;
