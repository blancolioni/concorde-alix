with Accord.Commodity;
with Accord.Has_Stock;

with Concorde.Quantities;

package Concorde.Stock is

   procedure Add
     (To        : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type);

end Concorde.Stock;
