with Accord.Commodity;
with Accord.Has_Stock;

with Concorde.Calendar;
with Concorde.Quantities;

package Concorde.Stock is

   function Quantity
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   function Quantity_At
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Clock     : Concorde.Calendar.Time;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type;

   procedure Add
     (To        : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type);

   procedure Remove
     (From      : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
     with Pre => Concorde.Quantities."<="
       (Quantity, Concorde.Stock.Quantity (From, Commodity));

   procedure Iterate
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Process   : not null access
        procedure (Commodity : Accord.Commodity.Commodity_Class;
                   Quantity  : Concorde.Quantities.Quantity_Type));

   procedure Save_History
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class);

end Concorde.Stock;
