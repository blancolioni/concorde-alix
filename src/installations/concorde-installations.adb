with Ada.Containers.Doubly_Linked_Lists;
with WL.String_Maps;

with Concorde.Logging;
with Accord.Db;

package body Concorde.Installations is

   type Production_Queue_Record is
      record
         Commodity : Accord.Commodity.Commodity_Handle;
         Quantity  : Concorde.Quantities.Quantity_Type;
      end record;

   package Production_Queue_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Production_Queue_Record);

   package Production_Queue_Maps is
     new WL.String_Maps
       (Production_Queue_Lists.List, Production_Queue_Lists."=");

   Cached_Queues : Production_Queue_Maps.Map;

   --------------
   -- Describe --
   --------------

   function Describe
     (Installation : Accord.Installation.Installation_Class)
      return String
   is
   begin
      return Installation.Facility.Tag
        & Accord.Db.To_String (Installation.Reference_Installation);
   end Describe;

   -----------------
   -- Empty_Queue --
   -----------------

   function Empty_Queue
     (Installation : Installation_Class)
      return Boolean
   is
   begin
      return not Cached_Queues.Contains (Installation.Identifier)
        or else Cached_Queues (Installation.Identifier).Is_Empty;
   end Empty_Queue;

   ----------------------------
   -- First_Queued_Commodity --
   ----------------------------

   function First_Queued_Commodity
     (Installation : Installation_Class)
      return Accord.Commodity.Commodity_Class
   is
   begin
      return Cached_Queues (Installation.Identifier).First_Element.Commodity;
   end First_Queued_Commodity;

   ---------------------------
   -- First_Queued_Quantity --
   ---------------------------

   function First_Queued_Quantity
     (Installation : Installation_Class)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Cached_Queues (Installation.Identifier).First_Element.Quantity;
   end First_Queued_Quantity;

   -------------------
   -- Iterate_Queue --
   -------------------

   procedure Iterate_Queue
     (Installation : Installation_Class;
      Process      : not null access
        procedure (Commodity    : Accord.Commodity.Commodity_Class;
                   Quantity     : Concorde.Quantities.Quantity_Type))
   is
   begin
      if Cached_Queues.Contains (Installation.Identifier) then
         for Item of Cached_Queues (Installation.Identifier) loop
            Process (Item.Commodity, Item.Quantity);
         end loop;
      end if;
   end Iterate_Queue;

   ---------
   -- Log --
   ---------

   procedure Log
     (Installation : Accord.Installation.Installation_Class;
      Message      : String)
   is
   begin
      Concorde.Logging.Log (Describe (Installation), Message);
   end Log;

   -------------------------------
   -- Queue_Capacity_Production --
   -------------------------------

   procedure Queue_Capacity_Production
     (Installation : Installation_Class;
      Commodity    : Accord.Commodity.Commodity_Class)
   is
   begin
      Queue_Production (Installation, Commodity, Concorde.Quantities.Zero);
   end Queue_Capacity_Production;

   ----------------------
   -- Queue_Production --
   ----------------------

   procedure Queue_Production
     (Installation : Installation_Class;
      Commodity    : Accord.Commodity.Commodity_Class;
      Quantity     : Concorde.Quantities.Quantity_Type)
   is
   begin
      if not Cached_Queues.Contains (Installation.Identifier) then
         Cached_Queues.Insert
           (Installation.Identifier, Production_Queue_Lists.Empty_List);
      end if;
      Cached_Queues (Installation.Identifier).Append
        ((Commodity.To_Commodity_Handle, Quantity));
   end Queue_Production;

   --------------------
   -- Set_Production --
   --------------------

   procedure Set_Production
     (Installation : Installation_Class;
      Production   : Accord.Commodity.Commodity_Class)
   is
   begin
      if Installation.Previous.Has_Element
        and then Production.Tag = Installation.Previous.Tag
      then
         Installation.Update_Installation
           .Set_Inefficiency (Installation.Inefficiency * 0.9)
           .Done;
      else
         Installation.Update_Installation
           .Set_Inefficiency (0.5)
           .Set_Previous (Production)
           .Done;
      end if;
   end Set_Production;

   ------------------------
   -- Update_Queue_First --
   ------------------------

   procedure Update_Queue_First
     (Installation : Installation_Class;
      Quantity     : Concorde.Quantities.Quantity_Type)
   is
      use type Concorde.Quantities.Quantity_Type;
      Queue : Production_Queue_Lists.List renames
                Cached_Queues (Installation.Identifier);
   begin
      if Quantity = Concorde.Quantities.Zero then
         Queue.Delete_First;
      else
         declare
            Item  : Production_Queue_Record renames
                      Queue (Queue.First);
         begin
            Item.Quantity := Quantity;
         end;
      end if;
   end Update_Queue_First;

end Concorde.Installations;
