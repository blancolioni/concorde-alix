with Tropos.Reader;

with Accord.Component;

with Accord.Hull_Armor;
with Accord.Hull_Configuration;

with Accord.Bridge;
with Accord.Computer;
with Accord.Engine;
with Accord.Generator;
with Accord.Jump_Drive;
with Accord.Reinforcement;
with Accord.Quarters;
with Accord.Sensor;
with Accord.Stealth;
with Accord.Weapon_Mount;

with Accord.Ship_Design;
with Accord.Ship_Design_Module;

with Accord.Technology;

with Accord.Db;

package body Concorde.Configure.Ships is

   Basic_Power_Per_Ton : constant := 0.2;

   function Get_Fraction
     (Config : Tropos.Configuration;
      Name   : String)
      return Real
   is (Real (Long_Float'(Config.Get (Name, 1.0))));

   function Get_Value
     (Config : Tropos.Configuration;
      Name   : String)
      return Real
   is (Real (Long_Float'(Config.Get (Name, 0.0))));

   procedure Configure_Design
     (Config : Tropos.Configuration);

   procedure Configure_Armor
     (Config : Tropos.Configuration);

   procedure Configure_Bridge
     (Config : Tropos.Configuration);

   procedure Configure_Computer
     (Config : Tropos.Configuration);

   procedure Configure_Generator
     (Config : Tropos.Configuration);

   procedure Configure_Hull
     (Config : Tropos.Configuration);

   procedure Configure_Engine
     (Config : Tropos.Configuration);

   procedure Configure_Jump_Drive
     (Config : Tropos.Configuration);

   procedure Configure_Quarters
     (Config : Tropos.Configuration);

   procedure Configure_Sensor
     (Config : Tropos.Configuration);

   procedure Configure_Weapon_Mount
     (Config : Tropos.Configuration);

   ---------------------
   -- Configure_Armor --
   ---------------------

   procedure Configure_Armor
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Accord.Hull_Armor.Create
        (Tag           => Config.Config_Name,
         Enabled_By    => Accord.Technology.Empty_Handle,
         Size_Fraction => Get ("consumed-size"),
         Mass_Fraction => Get ("mass-per-point"),
         Cost_Fraction => Get ("cost"),
         Max_Armor     => Config.Get ("max_armor"));
   end Configure_Armor;

   ----------------------
   -- Configure_Bridge --
   ----------------------

   procedure Configure_Bridge
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Size      : constant Non_Negative_Real := Get ("size");
      Ship_Size : constant Tropos.Configuration :=
                    Config.Child ("ship_size");
   begin
      Accord.Bridge.Create
        (Minimum_Size    => Size,
         Size            => Size,
         Mass_Per_Size   => Get ("mass"),
         Price_Per_Size  =>
           Concorde.Money.To_Price (Get ("price") / Size),
         Power_Per_Size  => Get ("power") / Size,
         Tag             => Config.Config_Name,
         Enabled_By      => Accord.Technology.Empty_Handle,
         Ship_Size_Low   => Real (Long_Float'(Ship_Size.Get (1))),
         Ship_Size_High  => Real (Long_Float'(Ship_Size.Get (2))));
   end Configure_Bridge;

   ------------------------
   -- Configure_Computer --
   ------------------------

   procedure Configure_Computer
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));
   begin
      Accord.Computer.Create
        (Power_Per_Size  => 0.2,
         Minimum_Size    => 1.0,
         Price_Per_Size  => Concorde.Money.To_Price (Get ("price")),
         Mass_Per_Size   => 1.0,
         Tag             => Config.Config_Name,
         Enabled_By      => Accord.Technology.Empty_Handle,
         Capacity        => Config.Get ("capacity"));
   end Configure_Computer;

   ----------------------
   -- Configure_Design --
   ----------------------

   procedure Configure_Design
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Hull                      : constant Accord.Hull_Configuration
        .Hull_Configuration_Class :=
               Accord.Hull_Configuration.Get_By_Tag
                 (Config.Get ("hull", "standard"));
      Armor : constant Accord.Hull_Armor.Hull_Armor_Class :=
                Accord.Hull_Armor.Get_By_Tag
                  (Config.Get ("armor", ""));

      Size : constant Non_Negative_Real := Get ("size");
      Hull_Points : constant Non_Negative_Real :=
                      Size / 2.5
                        + (if Size > 25_000.0
                           then (Size - 25_000.0) / 4.0 else 0.0)
                        + (if Size > 100_000.0
                           then (Size - 100_000.0) / 2.0 else 0.0);
      Dry_Mass    : constant Non_Negative_Real := Hull_Points;
      Fuel_Tank   : constant Non_Negative_Real :=
                      Get_Value (Config, "fuel_tank");

      Firm_Points : constant Natural :=
                      (if Size < 35.0 then 1
                       elsif Size < 70.0 then 2
                       elsif Size < 100.0 then 3
                       else 0);
      Hard_Points : constant Natural :=
                      Natural (Real'Truncation (Size / 100.0));

      Design : constant Accord.Ship_Design.Ship_Design_Class :=
                 Accord.Ship_Design.Create
                   (Name               =>
                                 Config.Get ("name", Config.Config_Name),
                    Hull_Configuration => Hull,
                    Hull_Armor         => Armor,
                    Stealth            =>
                      Accord.Stealth.Empty_Handle,
                    Reinforcement      =>
                      Accord.Reinforcement.Empty_Handle,
                    Size               => Size,
                    Dry_Mass           => Dry_Mass,
                    Hull_Points        => Hull_Points,
                    Fuel_Tank          => Fuel_Tank,
                    Cargo_Space        => 0.0,
                    Basic_Power        => 0.0,
                    Engine_Power       => 0.0,
                    Jump_Power         => 0.0,
                    Firmpoints         => Firm_Points,
                    Hardpoints         => Hard_Points);

      procedure Configure_Bridge_Design
        (Config : Tropos.Configuration);

      procedure Configure_Computer_Design
        (Config : Tropos.Configuration);

      procedure Configure_Engine_Design
        (Engine : Accord.Engine.Engine_Class);

      procedure Configure_Generator_Design
        (Gen_Config : Tropos.Configuration);

      procedure Configure_Jump_Design
        (Jump_Drive : Accord.Jump_Drive.Jump_Drive_Class);

      procedure Configure_Quarters_Design
        (Config : Tropos.Configuration);

      procedure Configure_Sensor_Design
        (Config : Tropos.Configuration);

      procedure Configure_Weapon_Mount_Design
        (Config : Tropos.Configuration);

      -----------------------------
      -- Configure_Bridge_Design --
      -----------------------------

      procedure Configure_Bridge_Design
        (Config : Tropos.Configuration)
      is
         Handle : constant Accord.Bridge.Bridge_Handle :=
                    Accord.Bridge.Get_By_Tag
                      (Config.Get ("type"));
      begin
         Accord.Ship_Design_Module.Create
           (Ship_Design => Design,
            Component   => Handle,
            Size        => Handle.Size,
            Mass        => Handle.Mass_Per_Size * Handle.Size,
            Concealed   => False);
      end Configure_Bridge_Design;

      -------------------------------
      -- Configure_Computer_Design --
      -------------------------------

      procedure Configure_Computer_Design
        (Config : Tropos.Configuration)
      is
         Handle : constant Accord.Computer.Computer_Handle :=
                    Accord.Computer.Get_By_Tag
                      (Config.Get ("type"));
      begin
         Accord.Ship_Design_Module.Create
           (Ship_Design => Design,
            Component   => Handle,
            Size        => 1.0,
            Mass        => Handle.Mass_Per_Size * Handle.Size,
            Concealed   => False);
      end Configure_Computer_Design;

      -----------------------------
      -- Configure_Engine_Design --
      -----------------------------

      procedure Configure_Engine_Design
        (Engine : Accord.Engine.Engine_Class)
      is
         Component_Size : constant Non_Negative_Real :=
                            Non_Negative_Real'Max
                              (Size * Engine.Hull_Fraction,
                               Engine.Minimum_Size);
      begin
         Accord.Ship_Design_Module.Create
           (Ship_Design => Design,
            Component   => Engine,
            Size        => Component_Size,
            Mass        => Engine.Mass_Per_Size * Component_Size,
            Concealed   => False);
      end Configure_Engine_Design;

      --------------------------------
      -- Configure_Generator_Design --
      --------------------------------

      procedure Configure_Generator_Design
        (Gen_Config : Tropos.Configuration)
      is
         Handle : constant Accord.Generator.Generator_Handle :=
                    Accord.Generator.Get_By_Tag
                      (Gen_Config.Get ("type"));
         Size   : constant Non_Negative_Real :=
                    Get_Value (Gen_Config, "size");
      begin
         pragma Assert (Handle.Has_Element,
                        "no such generator: "
                        & Gen_Config.Get ("type"));
         Accord.Ship_Design_Module.Create
           (Ship_Design => Design,
            Component   => Handle,
            Size        => Size,
            Mass        => Handle.Mass_Per_Size * Size,
            Concealed   => False);
      end Configure_Generator_Design;

      ---------------------------
      -- Configure_Jump_Design --
      ---------------------------

      procedure Configure_Jump_Design
        (Jump_Drive : Accord.Jump_Drive.Jump_Drive_Class)
      is
         Component_Size : constant Non_Negative_Real :=
                            Non_Negative_Real'Max
                              (Size * Jump_Drive.Hull_Fraction,
                               Jump_Drive.Minimum_Size);
      begin
         Accord.Ship_Design_Module.Create
           (Ship_Design => Design,
            Component   => Jump_Drive,
            Size        => Component_Size,
            Mass        => Jump_Drive.Mass_Per_Size * Component_Size);
      end Configure_Jump_Design;

      -------------------------------
      -- Configure_Quarters_Design --
      -------------------------------

      procedure Configure_Quarters_Design
        (Config : Tropos.Configuration)
      is
         Handle : constant Accord.Quarters.Quarters_Handle :=
                    Accord.Quarters.Get_By_Tag
                      (Config.Config_Name);
         Count  : constant Positive :=
                    (if Config.Child_Count = 0
                     then 1
                     else Config.Value);
      begin
         for I in 1 .. Count loop
            Accord.Ship_Design_Module.Create
              (Ship_Design => Design,
               Component   => Handle,
               Size        => Handle.Size,
               Mass        => Handle.Size * Handle.Mass_Per_Size,
               Concealed   => False);
         end loop;
      end Configure_Quarters_Design;

      -----------------------------
      -- Configure_Sensor_Design --
      -----------------------------

      procedure Configure_Sensor_Design
        (Config : Tropos.Configuration)
      is
         Handle : constant Accord.Sensor.Sensor_Handle :=
                    Accord.Sensor.Get_By_Tag
                      (Config.Get ("type", "basic"));
      begin
         Accord.Ship_Design_Module.Create
           (Ship_Design => Design,
            Component   => Handle,
            Size        => Handle.Size,
            Mass        => Handle.Size * Handle.Mass_Per_Size,
            Concealed   => False);
      end Configure_Sensor_Design;

      -----------------------------------
      -- Configure_Weapon_Mount_Design --
      -----------------------------------

      procedure Configure_Weapon_Mount_Design
        (Config : Tropos.Configuration)
      is
         Handle : constant Accord.Weapon_Mount.Weapon_Mount_Handle :=
                    Accord.Weapon_Mount.Get_By_Tag
                      (Config.Config_Name);
      begin
         pragma Assert (Handle.Has_Element);
         Accord.Ship_Design_Module.Create
           (Ship_Design => Design,
            Component   => Handle,
            Size        => Handle.Size,
            Mass        => Handle.Size * Handle.Mass_Per_Size,
            Concealed   => Config.Get ("concealed"));
      end Configure_Weapon_Mount_Design;

   begin
      Configure_Engine_Design
        (Accord.Engine.Get_By_Tag (Config.Get ("engine")));

      if Config.Contains ("jump") then
         Configure_Jump_Design
           (Accord.Jump_Drive.Get_By_Tag
              (Config.Get ("jump")));
      end if;

      Configure_Generator_Design (Config.Child ("generator"));
      Configure_Bridge_Design (Config.Child ("bridge"));
      Configure_Computer_Design (Config.Child ("computer"));
      Configure_Sensor_Design (Config.Child ("sensor"));

      for Weapon_Mount_Config of Config.Child ("weapon-mounts") loop
         Configure_Weapon_Mount_Design (Weapon_Mount_Config);
      end loop;

      for Quarters_Config of Config.Child ("quarters") loop
         Configure_Quarters_Design (Quarters_Config);
      end loop;

      declare
         Space        : Non_Negative_Real := Size - Fuel_Tank;
         Basic_Power  : constant Non_Negative_Real :=
                          Size * Basic_Power_Per_Ton;
         Engine_Power : Non_Negative_Real := 0.0;
         Jump_Power   : Non_Negative_Real := 0.0;
      begin
         for Module of
           Accord.Ship_Design_Module.Select_By_Ship_Design
             (Design)
         loop
            Space := Space - Module.Size;

            declare
               use Accord.Db;
            begin
               if Module.Component.Top_Record = R_Engine then
                  declare
                     Engine : constant Accord.Engine.Engine_Class :=
                                Accord.Engine.Get_From_Component
                                  (Module.Component);
                  begin
                     Engine_Power := Engine_Power
                       + Engine.Power_Per_Size * Module.Size;
                  end;
               elsif Module.Component.Top_Record = R_Jump_Drive then
                  declare
                     Jump               : constant Accord.Jump_Drive
                       .Jump_Drive_Handle :=
                         Accord.Jump_Drive.Get_From_Component
                           (Module.Component);
                  begin
                     Jump_Power := Jump_Power
                       + Jump.Power_Per_Size * Module.Size;
                  end;
               end if;
            end;
         end loop;

         Accord.Ship_Design.Update_Ship_Design (Design)
           .Set_Cargo_Space (Space)
           .Set_Basic_Power (Basic_Power)
           .Set_Engine_Power (Engine_Power)
           .Set_Jump_Power (Jump_Power)
           .Done;
      end;
   end Configure_Design;

   ----------------------
   -- Configure_Engine --
   ----------------------

   procedure Configure_Engine
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Accord.Engine.Create
        (Minimum_Size     => Get_Value (Config, "minimum_size"),
         Power_Per_Size   => Get ("power_per_size"),
         Price_Per_Size   => Concorde.Money.To_Price (Get ("cost")),
         Mass_Per_Size    => Get ("mass_per_size"),
         Tag              => Config.Config_Name,
         Enabled_By       => Accord.Technology.Empty_Handle,
         Hull_Fraction    => Get ("hull"),
         Impulse          => Get ("impulse"));
   end Configure_Engine;

   -------------------------
   -- Configure_Generator --
   -------------------------

   procedure Configure_Generator
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

   begin
      Accord.Generator.Create
        (Minimum_Size    => Get_Value (Config, "minimum_size"),
         Fuel_Per_Size   => Get ("fuel_per_size_per_day"),
         Price_Per_Size  => Concorde.Money.To_Price (Get ("price_per_size")),
         Mass_Per_Size   => Get ("mass_per_size"),
         Tag             => Config.Config_Name,
         Enabled_By      => Accord.Technology.Empty_Handle,
         Power_Per_Ton   => Get ("power_per_ton"));
   end Configure_Generator;

   --------------------
   -- Configure_Hull --
   --------------------

   procedure Configure_Hull
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Accord.Hull_Configuration.Create
        (Tag           => Config.Config_Name,
         Streamlining  => Get ("streamlining"),
         Hull_Points   => Get ("hull_points"),
         Cost          => Get ("cost"),
         Armor_Size => Get ("armor_size"));
   end Configure_Hull;

   --------------------------
   -- Configure_Jump_Drive --
   --------------------------

   procedure Configure_Jump_Drive
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Fraction (Config, Name));

   begin
      Accord.Jump_Drive.Create
        (Minimum_Size    => Get_Value (Config, "minimum_size"),
         Power_Per_Size  => Get ("power_per_size"),
         Price_Per_Size  => Concorde.Money.To_Price (Get ("cost")),
         Mass_Per_Size   => Get ("mass_per_size"),
         Tag             => Config.Config_Name,
         Enabled_By      => Accord.Technology.Empty_Handle,
         Hull_Fraction   => Get ("hull"),
         Jump            => Get ("jump"));
   end Configure_Jump_Drive;

   ------------------------
   -- Configure_Quarters --
   ------------------------

   procedure Configure_Quarters
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Size : constant Non_Negative_Real := Get ("size");
   begin
      Accord.Quarters.Create
        (Power_Per_Size     => Get ("power_per_size"),
         Minimum_Size       => Size,
         Price_Per_Size     =>
           Concorde.Money.To_Price (Get ("price_per_size")),
         Mass_Per_Size      => Get ("mass_per_size"),
         Tag                => Config.Config_Name,
         Enabled_By         => Accord.Technology.Empty_Handle,
         Size            => Size,
         Comfort_Level      => Config.Get ("comfort"),
         Standard_Occupants => Config.Get ("occupancy"),
         Max_Occupants      => Config.Get ("max_occupancy"));
   end Configure_Quarters;

   ----------------------
   -- Configure_Sensor --
   ----------------------

   procedure Configure_Sensor
     (Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      Size : constant Non_Negative_Real := Get ("size");
   begin
      Accord.Sensor.Create
        (Minimum_Size    => Get_Value (Config, "minimum_size"),
         Power_Per_Size  => Get ("power") / Real'Max (Size, 1.0),
         Mass_Per_Size   => Get ("mass") / Real'Max (Size, 1.0),
         Price_Per_Size  => Concorde.Money.To_Price (Get ("price")
           / Real'Max (Size, 1.0)),
         Tag             => Config.Config_Name,
         Enabled_By      => Accord.Technology.Empty_Handle,
         Size            => Size,
         Modifier        => Config.Get ("modifier"));
   end Configure_Sensor;

   ---------------------
   -- Configure_Ships --
   ---------------------

   procedure Configure_Ships (Scenario_Name : String) is

      procedure Configure
        (Category_Name : String;
         Extension     : String;
         Process       : not null access
           procedure (Config : Tropos.Configuration));

      ---------------
      -- Configure --
      ---------------

      procedure Configure
        (Category_Name : String;
         Extension     : String;
         Process       : not null access
           procedure (Config : Tropos.Configuration))
      is
      begin
         Tropos.Reader.Read_Config
           (Path      =>
              Scenario_File (Scenario_Name, "ships", Category_Name),
            Extension => Extension,
            Configure => Process);
      end Configure;

   begin
      Configure ("hulls", "hull", Configure_Hull'Access);
      Configure ("armor", "armor", Configure_Armor'Access);
      Configure ("engines", "engine", Configure_Engine'Access);
      Configure ("jump-drives", "jump", Configure_Jump_Drive'Access);
      Configure ("generators", "generator", Configure_Generator'Access);
      Configure ("bridges", "bridge", Configure_Bridge'Access);
      Configure ("computers", "computer", Configure_Computer'Access);
      Configure ("quarters", "quarters", Configure_Quarters'Access);
      Configure ("sensors", "sensor", Configure_Sensor'Access);
      Configure ("weapon-mounts", "mount", Configure_Weapon_Mount'Access);

      Configure ("designs", "design", Configure_Design'Access);

   end Configure_Ships;

   ----------------------------
   -- Configure_Weapon_Mount --
   ----------------------------

   procedure Configure_Weapon_Mount
     (Config : Tropos.Configuration)
   is

      function Get (Name : String) return Real
      is (Get_Value (Config, Name));

      function Get (Name : String) return Concorde.Money.Price_Type
      is (Concorde.Money.To_Price (Get (Name)));

      function Get (Name : String) return Accord.Db.Weapon_Mount_Category
      is (Accord.Db.Weapon_Mount_Category'Value
          (Config.Get (Name)));

   begin
      Accord.Weapon_Mount.Create
        (Tag              => Config.Config_Name,
         Enabled_By       => Accord.Technology.Empty_Handle,
         Power_Per_Size   => Get ("power"),
         Mass_Per_Size    => Get ("mass"),
         Minimum_Size     => Get ("size"),
         Price_Per_Size   => Get ("price"),
         Category         => Get ("category"),
         Fixed            => Config.Get ("fixed"),
         Hardpoints       => Config.Get ("hardpoints"),
         Weapon_Count     => Config.Get ("weapons"),
         Size             => Get ("size"));
   end Configure_Weapon_Mount;

end Concorde.Configure.Ships;
