with Accord.Historical_Stock;
with Accord.Stock_Item;

package body Concorde.Stock is

   function Get_Stock_Item
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Accord.Stock_Item.Stock_Item_Class;

   procedure Set_Quantity
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type);

   ---------
   -- Add --
   ---------

   procedure Add
     (To        : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
   begin
      if Quantity > Zero then
         Set_Quantity (To, Commodity,
                       Concorde.Stock.Quantity (To, Commodity)
                       + Quantity);
      end if;
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
           (Has_Stock, Commodity, Concorde.Quantities.Zero);
      end if;
   end Get_Stock_Item;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Process   : not null access
        procedure (Commodity : Accord.Commodity.Commodity_Class;
                   Quantity  : Concorde.Quantities.Quantity_Type))
   is
      use type Concorde.Quantities.Quantity_Type;
   begin
      for Stock_Item of
        Accord.Stock_Item.Select_By_Has_Stock (Has_Stock)
      loop
         if Stock_Item.Quantity > Concorde.Quantities.Zero then
            Process (Stock_Item.Commodity, Stock_Item.Quantity);
         end if;
      end loop;
   end Iterate;

   --------------
   -- Quantity --
   --------------

   function Quantity
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
      Stock_Item : constant Accord.Stock_Item.Stock_Item_Class :=
                     Accord.Stock_Item.Get_By_Stock_Item
                       (Has_Stock, Commodity);
   begin
      if Stock_Item.Has_Element then
         return Stock_Item.Quantity;
      else
         return Concorde.Quantities.Zero;
      end if;
   end Quantity;

   -----------------
   -- Quantity_At --
   -----------------

   function Quantity_At
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Clock     : Concorde.Calendar.Time;
      Commodity : Accord.Commodity.Commodity_Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      for Historical_Stock_Item of reverse
        Accord.Historical_Stock.Select_Historical_Stock_Bounded_By_Time_Stamp
          (Has_Stock         => Has_Stock,
           Commodity         => Commodity,
           Start_Time_Stamp  => Concorde.Calendar.Start,
           Finish_Time_Stamp => Clock)
      loop
         return Historical_Stock_Item.Quantity;
      end loop;
      return Concorde.Quantities.Zero;
   end Quantity_At;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (From      : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      use Concorde.Quantities;
   begin
      if Quantity > Zero then
         Set_Quantity (From, Commodity,
                       Concorde.Stock.Quantity (From, Commodity)
                       - Quantity);
      end if;
   end Remove;

   ------------------
   -- Save_History --
   ------------------

   procedure Save_History
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class)
   is
   begin
      for Stock_Item of
        Accord.Stock_Item.Select_By_Has_Stock (Has_Stock)
      loop
         Accord.Historical_Stock.Create
           (Time_Stamp => Concorde.Calendar.Clock,
            Has_Stock  => Has_Stock,
            Commodity  => Stock_Item.Commodity,
            Quantity   => Stock_Item.Quantity);
      end loop;
   end Save_History;

   ------------------
   -- Set_Quantity --
   ------------------

   procedure Set_Quantity
     (Has_Stock : Accord.Has_Stock.Has_Stock_Class;
      Commodity : Accord.Commodity.Commodity_Class;
      Quantity  : Concorde.Quantities.Quantity_Type)
   is
      Item : constant Accord.Stock_Item.Stock_Item_Class :=
               Get_Stock_Item (Has_Stock, Commodity);
   begin
      Item.Update_Stock_Item
        .Set_Quantity (Quantity)
        .Done;
   end Set_Quantity;

end Concorde.Stock;
