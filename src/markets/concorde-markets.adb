with Ada.Strings.Unbounded;

with WL.Random;

with Concorde.Colonies;
with Concorde.Quantities;

with Accord.Colony_Sector;
with Accord.Colony_Sector_Module;
with Accord.Trade_Goods;

with Accord.Db;

package body Concorde.Markets is

   type Classifier is access
     function (Colony : Accord.Colony.Colony_Class) return Boolean;

   function Is_Agricultural
     (Colony : Accord.Colony.Colony_Class)
      return Boolean;

   function Is_Desert
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is (Colony.World.Hydrosphere < 0.1);

   function Is_High_Population
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is (Concorde.Quantities.To_Real
       (Concorde.Colonies.Population (Colony))
       > 5000.0);

   function Is_Low_Population
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is (Concorde.Quantities.To_Real
       (Concorde.Colonies.Population (Colony))
       < 500.0);

   function Is_Non_Agricultural
     (Colony : Accord.Colony.Colony_Class)
      return Boolean;

   function Is_Non_Industrial
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is (Concorde.Quantities.To_Real
       (Concorde.Colonies.Population (Colony))
       < 1000.0);

   function Is_Poor
     (Colony : Accord.Colony.Colony_Class)
      return Boolean;

   function Is_Rich
     (Colony : Accord.Colony.Colony_Class)
      return Boolean;

   Classifiers : constant array (Trade_Classification_Element)
     of Classifier
       := (Agricultural     => Is_Agricultural'Access,
           Desert_World     => Is_Desert'Access,
           High_Population  => Is_High_Population'Access,
           Low_Population   => Is_Low_Population'Access,
           Non_Agricultural => Is_Non_Agricultural'Access,
           Non_Industrial   => Is_Non_Industrial'Access,
           Poor             => Is_Poor'Access,
           Rich             => Is_Rich'Access,
           others           => null);

   Class_Code : constant array (Trade_Classification_Element)
     of String (1 .. 2)
     := (Agricultural     => "Ag",
         Asteroid_Belt    => "As",
         Barren_World     => "Ba",
         Desert_World     => "De",
         Fluid_Oceans     => "Fl",
         High_Population  => "Hi",
         Ice_Capped       => "Ic",
         Industrial       => "In",
         Low_Population   => "Lo",
         Non_Agricultural => "Na",
         Non_Industrial   => "Ni",
         Poor             => "Po",
         Rich             => "Ri",
         Vacuum_World     => "Va",
         Water_World      => "Wa");

   Cost_Modifier : constant array (Trade_Classification_Element) of Real :=
                     (Agricultural     => -1.0,
                      Asteroid_Belt    => -1.0,
                      Barren_World     => 1.0,
                      Desert_World     => 1.0,
                      Fluid_Oceans     => 1.0,
                      High_Population  => -1.0,
                      Ice_Capped       => 0.0,
                      Industrial       => -1.0,
                      Low_Population   => 1.0,
                      Non_Agricultural => 0.0,
                      Non_Industrial   => 1.0,
                      Poor             => -1.0,
                      Rich             => 1.0,
                      Vacuum_World     => 1.0,
                      Water_World      => 0.0);

   Market_Price_Table : constant array (Trade_Classification_Element,
                                        Trade_Classification_Element)
     of Real :=
       (Agricultural => (Agricultural => 1.0, Asteroid_Belt => 1.0,
                         Desert_World => 1.0, High_Population => 1.0,
                         Industrial   => 1.0, Low_Population => 1.0,
                         Non_Agricultural => 1.0, Rich => 1.0,
                         others           => 0.0),
        Asteroid_Belt => (Asteroid_Belt => 1.0, Industrial => 1.0,
                          Non_Agricultural => 1.0, Rich => 1.0,
                          Vacuum_World     => 1.0, others => 0.0),
        Barren_World  => (Agricultural => 1.0, Industrial => 1.0,
                          others       => 0.0),
        High_Population => (High_Population | Low_Population | Rich => 1.0,
                            others                                  => 1.0),
        Low_Population  => (Industrial | Rich => 1.0, others => 0.0),
        Non_Industrial  => (Industrial => 1.0, Non_Industrial => -1.0,
                            others => 0.0),
        Rich          => (Agricultural | Desert_World
                            | High_Population | Industrial
                            | Non_Agricultural | Rich => 1.0,
                          others                      => 0.0),
        others        => (others => 0.0));

   Actual_Price_Table : constant array (2 .. 15) of Non_Negative_Real :=
                          (2 => 0.4,
                           3 => 0.5,
                           4 => 0.7,
                           5 => 0.8,
                           6 => 0.9,
                           7 => 1.0,
                           8 => 1.1,
                           9 => 1.2,
                           10 => 1.3,
                           11 => 1.5,
                           12 => 1.7,
                           13 => 2.0,
                           14 => 3.0,
                           15 => 4.0);

   ---------------------------
   -- Actual_Price_Of_Goods --
   ---------------------------

   function Actual_Price_Of_Goods
     (Colony         : Accord.Colony.Colony_Class;
      Classification : Trade_Classification;
      Broker_Level   : Natural)
      return Concorde.Money.Price_Type
   is
      Roll : constant Positive :=
               WL.Random.Random_Number (1, 6)
               + WL.Random.Random_Number (1, 6);
      Index : constant Positive := Positive'Min (Roll + Broker_Level,
                                                 Actual_Price_Table'Last);
   begin
      return Concorde.Money.Adjust_Price
        (Base_Price_Of_Goods (Colony, Classification),
         Actual_Price_Table (Index));
   end Actual_Price_Of_Goods;

   -------------------------
   -- Base_Price_Of_Goods --
   -------------------------

   function Base_Price_Of_Goods
     (Colony         : Accord.Colony.Colony_Class;
      Classification : Trade_Classification)
      return Concorde.Money.Price_Type
   is
      Local_Class : constant Trade_Classification :=
                      Classify_Colony (Colony);
      Cost        : Real := 5.0;
   begin
      for Source_Element of Classification loop
         for Market_Element of Local_Class loop
            Cost := Cost + Market_Price_Table (Source_Element, Market_Element);
         end loop;
      end loop;

      for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
         for Sector_Module of
           Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
         loop
            Cost := Cost + Sector_Module.Module.Price_Modifier;
         end loop;
      end loop;

      return Concorde.Money.To_Price (Cost);
   end Base_Price_Of_Goods;

   ---------------------
   -- Classify_Colony --
   ---------------------

   function Classify_Colony
     (Colony : Accord.Colony.Colony_Class) return Trade_Classification
   is
      Count  : constant Positive :=
                 Trade_Classification_Element'Pos
                   (Trade_Classification_Element'Last)
                   + 1;
      Result : Trade_Classification (1 .. Count);
      Last   : Natural := 0;
   begin
      for Element in Trade_Classification_Element loop
         if Classifiers (Element) /= null
           and then Classifiers (Element) (Colony)
         then
            Last := Last + 1;
            Result (Last) := Element;
         end if;
      end loop;
      return Result (1 .. Last);
   end Classify_Colony;

   -------------------
   -- Cost_Of_Goods --
   -------------------

   function Cost_Of_Goods
     (Colony : Accord.Colony.Colony_Class)
      return Concorde.Money.Price_Type
   is
      Classification : constant Trade_Classification :=
                         Classify_Colony (Colony);
      Cost           : Real := 4.0;
   begin
      for Class of Classification loop
         Cost := Cost + Cost_Modifier (Class);
      end loop;

      for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
         for Sector_Module of
           Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
         loop
            Cost := Cost + Sector_Module.Module.Cost_Modifier;
         end loop;
      end loop;

      return Concorde.Money.To_Price (Cost);
   end Cost_Of_Goods;

   ---------------------
   -- Is_Agricultural --
   ---------------------

   function Is_Agricultural
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is
      use type Accord.Db.Module_Category;
      Agricultural_Slots     : Natural := 0;
      Non_Agricultural_Slots : Natural := 0;
   begin
      for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
         for Sector_Module of
           Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
         loop
            if Sector_Module.Module.Module_Group.Category
              = Accord.Db.Agriculture
            then
               Agricultural_Slots := Agricultural_Slots + 1;
            else
               Non_Agricultural_Slots := Non_Agricultural_Slots + 1;
            end if;
         end loop;
      end loop;
      return Agricultural_Slots >= Non_Agricultural_Slots / 2;
   end Is_Agricultural;

   -------------------------
   -- Is_Non_Agricultural --
   -------------------------

   function Is_Non_Agricultural
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is
      use type Accord.Db.Module_Category;
      Agricultural_Slots     : Natural := 0;
      Non_Agricultural_Slots : Natural := 0;
   begin
      for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
         for Sector_Module of
           Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
         loop
            if Sector_Module.Module.Module_Group.Category
              = Accord.Db.Agriculture
            then
               Agricultural_Slots := Agricultural_Slots + 1;
            else
               Non_Agricultural_Slots := Non_Agricultural_Slots + 1;
            end if;
         end loop;
      end loop;

      return Agricultural_Slots < Non_Agricultural_Slots / 10;
   end Is_Non_Agricultural;

   -------------
   -- Is_Poor --
   -------------

   function Is_Poor
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is
      use Concorde.Money;
   begin
      return Concorde.Colonies.Gdp_Per_Capita (Colony) < To_Price (1.0);
   end Is_Poor;

   -------------
   -- Is_Rich --
   -------------

   function Is_Rich
     (Colony : Accord.Colony.Colony_Class)
      return Boolean
   is
      use Concorde.Money;
   begin
      return Concorde.Colonies.Gdp_Per_Capita (Colony) > To_Price (2.0);
   end Is_Rich;

   ------------------------------
   -- Is_Trade_Goods_Commodity --
   ------------------------------

   function Is_Trade_Goods_Commodity
     (Commodity : Accord.Commodity.Commodity_Class)
      return Boolean
   is
      use type Accord.Db.Record_Type;
   begin
      return Commodity.Top_Record = Accord.Db.R_Trade_Goods;
   end Is_Trade_Goods_Commodity;

   ----------
   -- Show --
   ----------

   function Show (Classification : Trade_Classification) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Element of Classification loop
         Result := Result & " " & Class_Code (Element);
      end loop;
      declare
         S : constant String := To_String (Result);
      begin
         return S (S'First + 1 .. S'Last);
      end;

   end Show;

   ------------------
   -- To_Commodity --
   ------------------

   function To_Commodity
     (Classification : Trade_Classification)
      return Accord.Commodity.Commodity_Class
   is
      Value : Natural := 0;
   begin
      for Element of Classification loop
         Value := Value + 2 ** Trade_Classification_Element'Pos (Element);
      end loop;
      declare
         Trade_Good : constant Accord.Trade_Goods.Trade_Goods_Handle :=
                        Accord.Trade_Goods.Get_By_Classification
                          (Value);
      begin
         if Trade_Good.Has_Element then
            return Trade_Good;
         else
            return Accord.Trade_Goods.Create
              (Mass           => 1.0,
               Base_Price     => Concorde.Money.To_Price (4.0),
               Complexity     => 1.0,
               Transient      => False,
               Employment     => False,
               Tag            => "tg" & Integer'Image (-Value),
               Classification => Value);
         end if;
      end;
   end To_Commodity;

   -----------------------------
   -- To_Trade_Classification --
   -----------------------------

   function To_Trade_Classification
     (Commodity : Accord.Commodity.Commodity_Class)
      return Trade_Classification
   is
      use type Accord.Db.Record_Type;
      pragma Assert (Commodity.Top_Record = Accord.Db.R_Trade_Goods);
      Trade_Goods : constant Accord.Trade_Goods.Trade_Goods_Handle :=
                      Accord.Trade_Goods.Get_From_Commodity (Commodity);
      Result : Trade_Classification
        (1 .. Trade_Classification_Element'Pos
           (Trade_Classification_Element'Last) + 1);
      Last   : Natural := 0;
      It     : Natural := Trade_Goods.Classification;
   begin
      for Element in Trade_Classification_Element loop
         if It mod 2 = 1 then
            Last := Last + 1;
            Result (Last) := Element;
         end if;
         It := It / 2;
      end loop;
      return Result (1 .. Last);
   end To_Trade_Classification;

end Concorde.Markets;
