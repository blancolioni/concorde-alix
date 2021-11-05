with Concorde.Money;

with Accord.Stock_Item;

package body Concorde.Stock is

   function Get_Stock_Item
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Accord.Stock_Item.Stock_Item_Class;

   ---------
   -- Add --
   ---------

   procedure Add
     (To        : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;

      Item : constant Accord.Stock_Item.Stock_Item_Class :=
               Get_Stock_Item (To, Commodity);
   begin
      Item.Update_Stock_Item
        .Set_Quantity (Item.Quantity + Quantity)
        .Done;
   end Add;

   --------------------
   -- Get_Stock_Item --
   --------------------

   function Get_Stock_Item
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Accord.Stock_Item.Stock_Item_Class
   is
      Stock_Item : constant Accord.Stock_Item.Stock_Item_Class :=
                     Accord.Stock_Item.Get_By_Stock_Item
                       (Has_Stock, Commodity);
   begin
      if Stock_Item.Has_Element then
         return Stock_Item;
      else
         return Accord.Stock_Item.Create
           (Has_Stock, Commodity, Concorde.Quantities.Zero,
            Concorde.Money.Zero);
      end if;
   end Get_Stock_Item;

end Concorde.Stock;
