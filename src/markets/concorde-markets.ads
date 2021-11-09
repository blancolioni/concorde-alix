with Concorde.Money;

with Accord.Colony;
with Accord.Commodity;

package Concorde.Markets is

   type Trade_Classification_Element is
     (Agricultural, Asteroid_Belt, Barren_World, Desert_World,
      Fluid_Oceans, High_Population, Ice_Capped, Industrial,
      Low_Population, Non_Agricultural, Non_Industrial,
      Poor, Rich, Vacuum_World, Water_World);

   type Trade_Classification is
     array (Positive range <>) of Trade_Classification_Element;

   function Show (Classification : Trade_Classification) return String;

   function Classify_Colony
     (Colony : Accord.Colony.Colony_Class)
      return Trade_Classification;

   function Get_Trade_Goods_Commodity
     (Colony : Accord.Colony.Colony_Class)
      return Accord.Commodity.Commodity_Class;

   function Is_Trade_Goods_Commodity
     (Commodity : Accord.Commodity.Commodity_Class)
      return Boolean;

   function Cost_Of_Goods
     (Colony : Accord.Colony.Colony_Class)
      return Concorde.Money.Price_Type;

   function Base_Price_Of_Goods
     (Colony         : Accord.Colony.Colony_Class;
      Classification : Trade_Classification)
      return Concorde.Money.Price_Type;

   function Actual_Price_Of_Goods
     (Colony         : Accord.Colony.Colony_Class;
      Classification : Trade_Classification;
      Broker_Level   : Natural)
      return Concorde.Money.Price_Type;

   function To_Commodity
     (Classification : Trade_Classification)
      return Accord.Commodity.Commodity_Class;

   function To_Trade_Classification
     (Commodity : Accord.Commodity.Commodity_Class)
      return Trade_Classification;

private

   function Get_Trade_Goods_Commodity
     (Colony : Accord.Colony.Colony_Class)
      return Accord.Commodity.Commodity_Class
   is (To_Commodity (Classify_Colony (Colony)));

end Concorde.Markets;
