with Tropos;

with Accord.Constructed;
with Accord.Has_Stock;
with Accord.Supplied;

package Concorde.Configure.Commodities is

   procedure Configure_Commodities
     (Scenario_Name : String);

   procedure Configure_Components
     (Scenario_Name : String);

   procedure Configure_Initial_Prices;
   procedure Configure_Complexity;

   procedure Configure_Stock
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

   procedure Configure_Constructed
     (Constructed : Accord.Constructed.Constructed_Class;
      Config      : Tropos.Configuration;
      Factor      : Non_Negative_Real := 1.0);

   procedure Configure_Supplied
     (Supplied : Accord.Supplied.Supplied_Class;
      Config    : Tropos.Configuration;
      Factor    : Non_Negative_Real := 1.0);

end Concorde.Configure.Commodities;
