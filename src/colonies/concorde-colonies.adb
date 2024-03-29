with WL.Random;
with WL.String_Sets;

with Concorde.Attributes;
with Concorde.Calculations;
with Concorde.Elementary_Functions;
with Concorde.Individuals;
with Concorde.Logging;
with Concorde.Sectors;
with Concorde.Stock;

with Accord.Colony_Edict;
with Accord.Colony_Request;
with Accord.Colony_Sector;
with Accord.Colony_Sector_Module;
with Accord.Office;
with Accord.Office_Vacancy;

package body Concorde.Colonies is

   Colony_Level_Attributes : WL.String_Sets.Set;

   function Calculate_Attribute
     (Colony         : Colony_Class;
      Attribute_Name : String)
      return Natural;

   function Capability
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute (Colony, "capability"));

   function Culture
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "culture"));

   function Defense
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "defense"));

   function Economy
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "economy"));

   function Law
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "law"));

   function Loyalty
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "loyalty"));

   function Stability
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "stability"));

   function Supply
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "supply"));

   function Unrest
     (Colony : Colony_Class)
      return Natural
   is (Calculate_Attribute
       (Colony, "unrest"));

   ----------------
   -- Add_Module --
   ----------------

   procedure Add_Module
     (To     : Colony_Class;
      Sector : Accord.World_Sector.World_Sector_Class;
      Tag    : String)
   is
      Module : constant Accord.Module.Module_Class :=
                 Accord.Module.Get_By_Tag (Tag);
   begin
      pragma Assert (Module.Has_Element,
                     "no such module: " & Tag);
      Add_Module (To, Sector, Module);
   end Add_Module;

   ----------------
   -- Add_Module --
   ----------------

   procedure Add_Module
     (To     : Colony_Class;
      Sector : Accord.World_Sector.World_Sector_Class;
      Module : Accord.Module.Module_Class)
   is
      Colony_Sector : constant Accord.Colony_Sector.Colony_Sector_Handle :=
                        Accord.Colony_Sector.Get_By_World_Sector
                          (Sector);
   begin
      pragma Assert (Colony_Sector.Has_Element,
                     To.Faction.Name
                     & " colony on " & To.World.Name
                     & " has no sector " & Sector.Identifier);
      pragma Assert (Colony_Sector.Slot_Count + Module.Slots
                     <= Colony_Sector.Max_Slots,
                     "insufficient slots available for " & Module.Tag);
      Accord.Colony_Sector_Module.Create
        (Colony_Sector => Colony_Sector,
         Module        => Module);
      Colony_Sector.Update_Colony_Sector
        .Set_Slot_Count (Colony_Sector.Slot_Count + Module.Slots)
        .Done;

   end Add_Module;

   -------------------------
   -- Calculate_Attribute --
   -------------------------

   function Calculate_Attribute
     (Colony         : Colony_Class;
      Attribute_Name : String)
      return Natural
   is
      Calculation : Concorde.Calculations.Calculation :=
                      Concorde.Calculations.Calculate
                        (Tag   => Attribute_Name,
                         Start =>
                           (if Colony_Level_Attributes
                            .Contains (Attribute_Name)
                            then Level (Colony)
                            else Concorde.Attributes.Get
                              (Colony, Attribute_Name)));
   begin
      for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
         for Sector_Module of
           Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
         loop
            Concorde.Calculations.Apply
              (Item           => Calculation,
               Tag            => Sector_Module.Module.Tag,
               Has_Attributes => Sector_Module.Module,
               Attribute_Name => Attribute_Name);
         end loop;
      end loop;

      for Office of Accord.Office.Scan_By_Rank loop
         declare
            Individual : constant Accord.Individual.Individual_Handle :=
                           Accord.Individual.First_By_Faction_Office
                             (Colony.Faction, Office);
         begin
            if Individual.Has_Element then
               if Individual.World_Sector.World.Identifier
                 = Colony.World.Identifier
               then
                  Concorde.Calculations.Apply
                    (Item           => Calculation,
                     Tag            =>
                       Concorde.Individuals.Full_Name_And_Title (Individual),
                     Has_Attributes => Office,
                     Attribute_Name => Attribute_Name,
                     Add_Factor     =>
                       Concorde.Individuals.Ruling_Bonus (Individual, Office));
               end if;
            else
               declare
                  Vacancy : constant Accord.Office_Vacancy
                    .Office_Vacancy_Handle :=
                      Accord.Office_Vacancy.Get_By_Office (Office);
               begin
                  if Vacancy.Has_Element then
                     Concorde.Calculations.Apply
                       (Item           => Calculation,
                        Tag            => "vacant," & Office.Tag,
                        Has_Attributes => Vacancy,
                        Attribute_Name => Attribute_Name);
                  end if;
               end;
            end if;
         end;
      end loop;

      for Colony_Edict of
        Accord.Colony_Edict.Select_By_Colony (Colony)
      loop
         Concorde.Calculations.Apply
           (Item           => Calculation,
            Tag            =>
              Colony_Edict.Edict_Group.Tag & "-" & Colony_Edict.Edict.Tag,
            Has_Attributes => Colony_Edict.Edict,
            Attribute_Name => Attribute_Name);
      end loop;

      if False then
         Concorde.Logging.Log
           (Colony.World.Name,
            Concorde.Calculations.Show (Calculation));
      end if;

      declare
         Base_Value : constant Non_Negative_Real :=
                        Non_Negative_Real
                          (Integer'Max
                             (Concorde.Calculations.Total (Calculation), 0));
         Pop        : constant Non_Negative_Real :=
                        Concorde.Quantities.To_Real (Population (Colony));
         Pop_Points : constant Non_Negative_Real :=
                        Concorde.Elementary_Functions.Log (Pop, 10.0);
      begin
         return Natural (Base_Value / Pop_Points);
      end;

   end Calculate_Attribute;

   -----------
   -- Check --
   -----------

   function Check
     (Colony        : Colony_Class;
      Check_Tag     : String;
      Checker       : Accord.Individual.Individual_Class;
      Attribute     : not null Colony_Attribute_Function;
      Attribute_Tag : String;
      Bonus         : Natural := 0;
      Bonus_Tag     : String := "";
      Penalty       : Natural := 0;
      Penalty_Tag   : String := "")
      return Check_Result
   is
      Roll : constant Positive := WL.Random.Random_Number (1, 20);
      Ruling_Bonus : constant Natural :=
                       (if Checker.Has_Element
                        then Concorde.Individuals.Ruling_Bonus
                          (Checker, Checker.Office)
                        else 0);
      Total        : constant Natural :=
                       Integer'Max
                         (Attribute (Colony) + Ruling_Bonus
                          + Bonus + Roll - Penalty,
                          0);
   begin
      if not Checker.Has_Element then
         return Result : constant Check_Result :=
           Check_Result'
             (Roll       => 1,
              Total      => Total,
              Difficulty => Ruling_Difficulty (Colony))
         do
            Log (Colony, "check with no ruler fails");
         end return;
      end if;

      return Result : constant Check_Result :=
        Check_Result'
          (Roll       => Roll,
           Total      => Total,
           Difficulty => Ruling_Difficulty (Colony))
      do
         Log (Colony,
              "check: " & Check_Tag & " by "
              & Concorde.Individuals.Full_Name_And_Title (Checker)
              & ": " & Attribute_Tag & Attribute (Colony)'Image
              & " + ruling" & Ruling_Bonus'Image
              & (if Bonus_Tag = "" then ""
                else " + " & Bonus_Tag & Bonus'Image)
              & (if Penalty_Tag = "" then ""
                else " - " & Penalty_Tag & Penalty'Image)
              & " + roll" & Roll'Image
              & " =" & Total'Image
              & " against difficulty" & Result.Difficulty'Image
              & ": "
              & (if Major_Success (Result)
                then "major success"
                elsif Major_Failure (Result)
                then "major failure"
                elsif Success (Result)
                then "success"
                else "failure"));
      end return;
   end Check;

   ---------------------
   -- Current_Request --
   ---------------------

   function Current_Request
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
      Request : constant Accord.Colony_Request.Colony_Request_Class :=
                  Accord.Colony_Request.Get_By_Colony_Request
                    (Colony, Commodity, True);
   begin
      if Request.Has_Element then
         return Request.Quantity;
      else
         return Concorde.Quantities.Zero;
      end if;
   end Current_Request;

   --------------------
   -- Current_Supply --
   --------------------

   function Current_Supply
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
      Minimum   : constant Quantity_Type :=
                    Minimum_Required (Colony, Commodity);
      Available : constant Quantity_Type :=
                    Concorde.Stock.Quantity
                      (Colony, Commodity);
   begin
      if Available > Minimum then
         return Available - Minimum;
      else
         return Zero;
      end if;
   end Current_Supply;

   ----------------
   -- Employment --
   ----------------

   function Employment
     (Colony : Colony_Class)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
   begin
      return Emp : Quantity_Type := Zero do
         for Sector of
           Accord.Colony_Sector.Select_By_Colony (Colony)
         loop
            for Sector_Module of
              Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
            loop
               Emp := Emp + Sector_Module.Module.Employment;
            end loop;
         end loop;
      end return;
   end Employment;

   ----------------------
   -- For_All_Colonies --
   ----------------------

   procedure For_All_Colonies
     (Process : not null access
        procedure (Colony : Colony_Class))
   is
   begin
      for Colony of Accord.Colony.Scan_By_Top_Record loop
         Process (Colony);
      end loop;
   end For_All_Colonies;

   ---------
   -- Gdp --
   ---------

   function Gdp
     (Colony : Colony_Class)
      return Concorde.Money.Money_Type
   is
      Base_Gdp : constant Non_Negative_Real :=
                   Real (Economy (Colony) * Stability (Colony))
                   * Concorde.Quantities.To_Real (Employment (Colony));
      Unrest_Penalty : constant Non_Negative_Real :=
                         Real'Min (Real (Unrest (Colony)),
                                   Base_Gdp / 2.0);

   begin
      return Concorde.Money.To_Money (Base_Gdp - Unrest_Penalty);
   end Gdp;

   -----------
   -- Level --
   -----------

   function Level
     (Colony : Colony_Class)
      return Positive
   is
      Max_Investment_For_Level : constant array (1 .. 20) of Positive :=
                                   (50, 100, 150, 200,
                                    300, 400, 500, 600,
                                    750, 900, 1050, 1200,
                                    1400, 1600, 1800, 2000,
                                    2250, 2500, 3750, 4000);
      Investment               : constant Natural :=
                                   Natural
                                     (Concorde.Money.To_Real
                                        (Total_Infrastructure (Colony)));
   begin
      for I in Max_Investment_For_Level'Range loop
         if Max_Investment_For_Level (I) >= Investment then
            return I;
         end if;
      end loop;

      return Max_Investment_For_Level'Last;
   end Level;

   ---------
   -- Log --
   ---------

   procedure Log
     (Colony  : Colony_Class;
      Message : String)
   is
   begin
      Concorde.Logging.Log
        (Colony.Faction.Adjective
         & " colony on " & Colony.World.Name,
         Message);
   end Log;

   ----------------------
   -- Minimum_Required --
   ----------------------

   function Minimum_Required
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      if Commodity.Tag = "food" then
         return Concorde.Quantities.Scale
           (Population (Colony), 10.0);
      else
         return Concorde.Quantities.Zero;
      end if;
   end Minimum_Required;

   ----------------
   -- Population --
   ----------------

   function Population
     (Colony : Colony_Class)
      return Concorde.Quantities.Quantity_Type
   is
      use Concorde.Quantities;
   begin
      return Pop : Quantity_Type := Zero do
         for Sector of
           Accord.Colony_Sector.Select_By_Colony (Colony)
         loop
            Pop := Pop + Sector.Population;
         end loop;
      end return;
   end Population;

   -------------
   -- Request --
   -------------

   procedure Request
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Offer     : Concorde.Money.Price_Type)
   is
      use Concorde.Quantities;
      Request : constant Accord.Colony_Request.Colony_Request_Class :=
                  Accord.Colony_Request.Get_By_Colony_Request
                    (Colony, Commodity, True);
   begin
      if Request.Has_Element then
         Log (Colony,
              "requesting additional "
              & Show (Quantity)
              & " "
              & Commodity.Tag
              & " for "
              & Concorde.Money.Show (Offer)
              & " each (total "
              & Concorde.Money.Show (Concorde.Money.Total (Offer, Quantity))
              & ")");

         Request.Update_Colony_Request
           .Set_Quantity (Request.Quantity + Quantity)
           .Set_Remaining (Request.Remaining + Quantity)
           .Set_Offer (Offer)
           .Done;
      else
         Log (Colony,
              "requesting "
              & Show (Quantity)
              & " "
              & Commodity.Tag
              & " for "
              & Concorde.Money.Show (Offer)
              & " each (total "
              & Concorde.Money.Show (Concorde.Money.Total (Offer, Quantity))
              & ")");

         Accord.Colony_Request.Create
           (Colony    => Colony,
            Commodity => Commodity,
            Quantity  => Quantity,
            Remaining => Quantity,
            Offer     => Offer,
            Active    => True);
      end if;
   end Request;

   -----------------------
   -- Ruling_Difficulty --
   -----------------------

   function Ruling_Difficulty (Colony : Colony_Class) return Natural is
   begin
      return 20 + Size (Colony);
   end Ruling_Difficulty;

   ----------
   -- Size --
   ----------

   function Size (Colony : Colony_Class) return Positive is
      Selection : constant Accord.Colony_Sector.Colony_Sector_Selection :=
                    Accord.Colony_Sector.Select_By_Colony (Colony);
   begin
      return Selection.Length;
   end Size;

   --------------------------
   -- Total_Infrastructure --
   --------------------------

   function Total_Infrastructure
     (Colony : Colony_Class)
      return Concorde.Money.Money_Type
   is
      use Concorde.Money;
   begin
      return Value : Money_Type := Zero do
         for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
            for Sector_Module of
              Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
            loop
               Value := Value +
                 Adjust
                   (Sector_Module.Module.Cost,
                    Concorde.Sectors.Module_Group_Cost
                      (Sector => Sector.World_Sector,
                       Group  => Sector_Module.Module.Module_Group));
            end loop;
         end loop;
      end return;
   end Total_Infrastructure;

   -----------------------
   -- Total_Maintenance --
   -----------------------

   function Total_Maintenance
     (Colony : Colony_Class)
      return Concorde.Money.Money_Type
   is
      use Concorde.Money;
   begin
      return Maintenance : Money_Type := Zero do
         for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
            Maintenance := Maintenance
              + To_Money (Real (Sector.Max_Slots) * 0.1);
            for Sector_Module of
              Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
            loop
               Maintenance := Maintenance +
                 Adjust
                   (Sector_Module.Module.Cost,
                    Concorde.Sectors.Module_Group_Cost
                      (Sector => Sector.World_Sector,
                       Group  => Sector_Module.Module.Module_Group)
                    * 0.01);
            end loop;
         end loop;
      end return;
   end Total_Maintenance;

begin
   Colony_Level_Attributes.Include ("capability");
   Colony_Level_Attributes.Include ("culture");
   Colony_Level_Attributes.Include ("defense");
   Colony_Level_Attributes.Include ("law");
   Colony_Level_Attributes.Include ("supply");
end Concorde.Colonies;
