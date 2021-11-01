with Ada.Text_IO;

with Concorde.Constants;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Real_Images;
with Concorde.Solar_System;

with Concorde.Ships;

with Accord.Commodity;
with Accord.Container_Component;
with Accord.Drive_Component;
with Accord.Power_Component;

with Accord.Attachment;
with Accord.Ship_Component;
with Accord.Ship_Design;
with Accord.Ship_Module_Design;

package body Concorde.Configure.Ships is

   procedure Configure_Component
     (Component_Config : Tropos.Configuration);

   procedure Configure_Design
     (Design_Config : Tropos.Configuration);

   -------------------------
   -- Configure_Component --
   -------------------------

   procedure Configure_Component
     (Component_Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Real (Float'(Component_Config.Get (Name, 0.0))));

      function Get
        (Child   : String;
         Name    : String;
         Default : Real := 0.0)
         return Real
      is (Real (Float'(Component_Config.Child (Child).Get
                       (Name, Float (Default)))));

      function Get
        (Child : String;
         Index : Positive)
         return Real
      is (if Component_Config.Child (Child).Child_Count >= Index
          then Real (Float'(Component_Config.Child (Child).Get (Index)))
          else 0.0);

      function Get
        (Child : String;
         Name  : String)
         return Boolean
      is (Component_Config.Child (Child).Get (Name));

      function Get
        (Name  : String)
         return Boolean
      is (Component_Config.Get (Name));

      Tag : constant String :=
              Component_Config.Config_Name;
      Mass : constant Non_Negative_Real := Get ("mass");
      Idle_Power : constant Non_Negative_Real := Get ("idle-power");
      Max_Power  : constant Non_Negative_Real := Get ("max-power");
      Linear_Accel_Limit : constant Non_Negative_Real :=
                             Get ("acceleration-limit", "linear");
      Angular_Accel_Limit : constant Non_Negative_Real :=
                              Get ("acceleration-limit", "angular");
      Normal_Temperature  : constant Non_Negative_Real :=
                              Get ("temperature-limit", "normal")
                              + Concorde.Constants.Freezing_Point_Of_Water;
      Fail_Temperature : constant Non_Negative_Real :=
                           Get ("temperature-limit", "failure")
                           + Concorde.Constants.Freezing_Point_Of_Water;
      Explosion_Power  : constant Non_Negative_Real :=
                              Get ("explosion");
      Throttled           : constant Boolean := Get ("throttle")
        or else Component_Config.Contains ("throttled");
      Min_Throttle        : constant Real := Get ("throttle", "min");
      Max_Throttle        : constant Real := Get ("throttle", "max", 1.0);
      Operating_Heat      : constant Real := Get ("heat", "operating");
      Heat_Sink           : constant Real := Get ("heat", "sink");
      X1 : constant Real := Get ("bounding-box", 1);
      X2 : constant Real := Get ("bounding-box", 2);
      Y1 : constant Real := Get ("bounding-box", 3);
      Y2 : constant Real := Get ("bounding-box", 4);
      Z1 : constant Real := Get ("bounding-box", 5);
      Z2 : constant Real := Get ("bounding-box", 6);

   begin
      if Component_Config.Contains ("cargo") then

         Ada.Text_IO.Put_Line ("configure: " & Component_Config.Config_Name);

         Accord.Container_Component.Create
           (Idle_Power          => Idle_Power,
            Max_Power           => Max_Power,
            Linear_Accel_Limit  => Linear_Accel_Limit,
            Angular_Accel_Limit => Angular_Accel_Limit,
            Max_Normal_Temp     => Normal_Temperature,
            Failure_Temp        => Fail_Temperature,
            Throttled           => Throttled,
            Min_Throttle        => Min_Throttle,
            Max_Throttle        => Max_Throttle,
            Operating_Heat      => Operating_Heat,
            Heat_Sink           => Heat_Sink,
            Explosion_Power     => Explosion_Power,
            Tag                 => Tag,
            Enabled_By          => Accord.Null_Technology_Reference,
            Available           => True,
            Initial_Price       => Concorde.Money.Zero,
            Mass                => Mass,
            Density             => 0.0,
            X1                  => X1,
            X2                  => X2,
            Y1                  => Y1,
            Y2                  => Y2,
            Z1                  => Z1,
            Z2                  => Z2,
            Capacity            =>
              Concorde.Quantities.To_Quantity
                (Get ("cargo", "volume")),
            Liquid              => Get ("cargo", "liquid"),
            Gas                 => Get ("cargo", "gas"),
            Cryo                => Get ("cargo", "cryo"));

      elsif Component_Config.Contains ("thrust") then

         Ada.Text_IO.Put_Line ("configure: " & Component_Config.Config_Name);

         Accord.Drive_Component.Create
           (Idle_Power          => Idle_Power,
            Max_Power           => Max_Power,
            Linear_Accel_Limit  => Linear_Accel_Limit,
            Angular_Accel_Limit => Angular_Accel_Limit,
            Max_Normal_Temp     => Normal_Temperature,
            Failure_Temp        => Fail_Temperature,
            Throttled           => Throttled,
            Min_Throttle        => Min_Throttle,
            Max_Throttle        => Max_Throttle,
            Operating_Heat      => Operating_Heat,
            Heat_Sink           => Heat_Sink,
            Explosion_Power     => Explosion_Power,
            Tag                 => Tag,
            Enabled_By          => Accord.Null_Technology_Reference,
            Available           => True,
            Initial_Price       => Concorde.Money.Zero,
            Mass                => Mass,
            Density             => 0.0,
            X1                  => X1,
            X2                  => X2,
            Y1                  => Y1,
            Y2                  => Y2,
            Z1                  => Z1,
            Z2                  => Z2,
            Minimum_Thrust      => Get ("thrust", "minimum"),
            Maximum_Thrust      => Get ("thrust", "maximum"),
            Fuel                =>
              Accord.Commodity.Get_By_Tag
                (Component_Config.Get ("fuel", "")),
            Oxidiser            =>
              Accord.Commodity.Get_By_Tag
                (Component_Config.Get ("oxidiser", "")),
            Max_Fuel_Burn       => Get ("max_fuel_burn"),
            Max_Oxidiser_Burn   => Get ("max_oxidiser_burn"),
            Fuel_Propellant     => Component_Config.Get ("propellent-fuel"),
            Propellent          =>
              Accord.Commodity.Get_By_Tag
                (Component_Config.Get ("propellent", "")),
            Exhaust_Velocity    => Get ("ve"));
      elsif Component_Config.Get ("class", "") = "power" then

         Ada.Text_IO.Put_Line ("configure: " & Component_Config.Config_Name);

         Accord.Power_Component.Create
           (Idle_Power          => Idle_Power,
            Max_Power           => Max_Power,
            Linear_Accel_Limit  => Linear_Accel_Limit,
            Angular_Accel_Limit => Angular_Accel_Limit,
            Max_Normal_Temp     => Normal_Temperature,
            Failure_Temp        => Fail_Temperature,
            Throttled           => Throttled,
            Min_Throttle        => Min_Throttle,
            Max_Throttle        => Max_Throttle,
            Operating_Heat      => Operating_Heat,
            Heat_Sink           => Heat_Sink,
            Explosion_Power     => Explosion_Power,
            Tag                 => Tag,
            Enabled_By          => Accord.Null_Technology_Reference,
            Available           => True,
            Initial_Price       => Concorde.Money.Zero,
            Mass                => Mass,
            Density             => 0.0,
            X1                  => X1,
            X2                  => X2,
            Y1                  => Y1,
            Y2                  => Y2,
            Z1                  => Z1,
            Z2                  => Z2,
            Power_Output        => Get ("generate", "power"));
      end if;

      declare
         Component : constant Accord.Ship_Component_Reference :=
                       Accord.Ship_Component.Get_By_Tag
                         (Component_Config.Config_Name);
      begin
         for Attach_Config of Component_Config.Child ("attachments") loop
            Accord.Attachment.Create
              (Ship_Component => Component,
               Name           => Attach_Config.Config_Name,
               X              => Attach_Config.Get (1),
               Y              => Attach_Config.Get (2),
               Z              => Attach_Config.Get (3));
         end loop;
      end;

   end Configure_Component;

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design
     (Design_Config : Tropos.Configuration)
   is
      Design : constant Accord.Ship_Design_Reference :=
                 Accord.Ship_Design.Create
                   (Name      => Design_Config.Config_Name,
                    Hold_Size => Concorde.Quantities.Zero,
                    Default_Manager =>
                      Design_Config.Get ("default-manager", "ship-default"));
   begin
      for Component_Config of Design_Config.Child ("components") loop
         declare
            use Accord.Db;
            Component : constant Accord.Ship_Component_Reference :=
                          Accord.Ship_Component.Get_By_Tag
                            (Component_Config.Config_Name);
            Count     : constant Positive :=
                          (if Component_Config.Child_Count = 1
                           then Component_Config.Value
                           else 1);
         begin
            if Component = Null_Ship_Component_Reference then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "in design '" & Design_Config.Config_Name
                  & "': no such component: "
                  & Component_Config.Config_Name);
            else
               for I in 1 .. Count loop
                  Accord.Ship_Module_Design.Create
                    (Ship_Design    => Design,
                     Ship_Component => Component);
               end loop;
            end if;
         end;
      end loop;

      declare
         Empty_Mass : constant Non_Negative_Real :=
                        Concorde.Ships.Design_Mass (Design);
         Fuel_Mass  : constant Non_Negative_Real :=
                        Concorde.Ships.Design_Fuel_Mass (Design);
         Cargo_Volume : constant Non_Negative_Real :=
                          Concorde.Ships.Design_Cargo_Volume (Design);
         Thrust     : constant Non_Negative_Real :=
                        Concorde.Ships.Design_Thrust (Design);
      begin
         Ada.Text_IO.Put_Line
           ("Name:          " & Design_Config.Config_Name);
         Ada.Text_IO.Put_Line
           ("Mass (empty):  "
            & Concorde.Real_Images.Approximate_Image
              (Empty_Mass / 1000.0)
            & "t");
         Ada.Text_IO.Put_Line
           ("Mass (fueled): "
            & Concorde.Real_Images.Approximate_Image
              ((Empty_Mass + Fuel_Mass) / 1000.0)
            & "t");
         Ada.Text_IO.Put_Line
           ("Mass (loaded): "
            & Concorde.Real_Images.Approximate_Image
              ((Empty_Mass + Fuel_Mass) / 1000.0 + Cargo_Volume)
            & "t");
         Ada.Text_IO.Put_Line
           ("Accel (fueled): "
            & Concorde.Real_Images.Approximate_Image
              (Thrust / (Empty_Mass + Fuel_Mass)
               / Concorde.Solar_System.Earth_Gravity)
            & "g");
         Ada.Text_IO.Put_Line
           ("Accel (loaded): "
            & Concorde.Real_Images.Approximate_Image
              (Thrust / (Empty_Mass + Fuel_Mass + Cargo_Volume * 1000.0)
               / Concorde.Solar_System.Earth_Gravity)
            & "g");
         Ada.Text_IO.Put_Line
           ("Delta-v (loaded): "
            & Concorde.Real_Images.Approximate_Image
              (Concorde.Ships.Design_Delta_V (Design, Cargo_Volume * 1000.0))
            & "m/s");

      end;
   end Configure_Design;

   -------------------------------
   -- Configure_Ship_Components --
   -------------------------------

   procedure Configure_Ship_Components
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "ships/components",
         File_Class_Name => "component",
         Process         => Configure_Component'Access);
   end Configure_Ship_Components;

   ----------------------------
   -- Configure_Ship_Designs --
   ----------------------------

   procedure Configure_Ship_Designs
     (Scenario_Name : String)
   is
   begin
      Load_Scenario_Files
        (Scenario_Name   => Scenario_Name,
         Directory_Name  => "ships/designs",
         File_Class_Name => "design",
         Process         => Configure_Design'Access);
   end Configure_Ship_Designs;

end Concorde.Configure.Ships;
