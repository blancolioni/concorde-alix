with Concorde.Money;
with Concorde.Quantities;

with Accord.Colony;
with Accord.Commodity;
with Accord.Individual;
with Accord.Module;
with Accord.World_Sector;

package Concorde.Colonies is

   Slots_Per_Sector : constant := 12;

   subtype Colony_Class is Accord.Colony.Colony_Class;

   procedure Add_Module
     (To     : Colony_Class;
      Sector : Accord.World_Sector.World_Sector_Class;
      Tag    : String);

   procedure Add_Module
     (To     : Colony_Class;
      Sector : Accord.World_Sector.World_Sector_Class;
      Module : Accord.Module.Module_Class);

   type Colony_Attribute_Function is access
     function (Colony : Colony_Class) return Natural;

   function Economy (Colony : Colony_Class) return Natural;
   function Loyalty (Colony : Colony_Class) return Natural;
   function Stability (Colony : Colony_Class) return Natural;
   function Unrest (Colony : Colony_Class) return Natural;
   function Capability (Colony : Colony_Class) return Natural;
   function Culture (Colony : Colony_Class) return Natural;
   function Defense (Colony : Colony_Class) return Natural;
   function Law (Colony : Colony_Class) return Natural;
   function Supply (Colony : Colony_Class) return Natural;

   function Population
     (Colony : Colony_Class)
      return Concorde.Quantities.Quantity_Type;

   function Employment
     (Colony : Colony_Class)
      return Concorde.Quantities.Quantity_Type;

   function Gdp
     (Colony : Colony_Class)
     return Concorde.Money.Money_Type;

   function Gdp_Per_Capita
     (Colony : Colony_Class)
      return Concorde.Money.Price_Type;

   function Minimum_Required
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   function Current_Request
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   function Current_Supply
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   procedure Request
     (Colony    : Colony_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type;
      Offer     : Concorde.Money.Price_Type);

   function Size (Colony : Colony_Class) return Positive;
   function Ruling_Difficulty (Colony : Colony_Class) return Natural;
   function Total_Infrastructure
     (Colony : Colony_Class)
      return Concorde.Money.Money_Type;
   function Total_Maintenance
     (Colony : Colony_Class)
      return Concorde.Money.Money_Type;
   function Level
     (Colony : Colony_Class)
      return Positive;

   type Check_Result is private;

   function Roll (Check : Check_Result) return Positive;
   function Total (Check : Check_Result) return Natural;
   function Success (Check : Check_Result) return Boolean;
   function Major_Success (Check : Check_Result) return Boolean;
   function Failure (Check : Check_Result) return Boolean;
   function Major_Failure (Check : Check_Result) return Boolean;

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
      return Check_Result;

   procedure For_All_Colonies
     (Process : not null access
        procedure (Colony : Colony_Class));

   procedure Log
     (Colony  : Colony_Class;
      Message : String);

private

   type Check_Result is
      record
         Roll       : Positive;
         Total      : Natural;
         Difficulty : Natural;
      end record;

   function Roll (Check : Check_Result) return Positive
   is (Check.Roll);

   function Total (Check : Check_Result) return Natural
   is (Check.Total);

   function Success (Check : Check_Result) return Boolean
   is (Check.Total >= Check.Difficulty);

   function Major_Success (Check : Check_Result) return Boolean
   is (Check.Total >= Check.Difficulty + 5);

   function Failure (Check : Check_Result) return Boolean
   is (Check.Total < Check.Difficulty);

   function Major_Failure (Check : Check_Result) return Boolean
   is (Check.Total <= Check.Difficulty - 5);

   function Gdp_Per_Capita
     (Colony : Colony_Class)
      return Concorde.Money.Price_Type
   is (Concorde.Money.Price (Gdp (Colony), Population (Colony)));

end Concorde.Colonies;
