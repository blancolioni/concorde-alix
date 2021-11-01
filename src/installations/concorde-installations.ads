private with Concorde.Real_Images;

with Concorde.Quantities;

with Accord.Commodity;
with Accord.Installation;

package Concorde.Installations is

   subtype Installation_Handle is
     Accord.Installation.Installation_Handle;

   subtype Installation_Class is
     Accord.Installation.Installation_Class;

   function Describe
     (Installation : Installation_Class)
      return String;

   procedure Log
     (Installation     : Installation_Class;
      Message          : String);

   procedure Set_Production
     (Installation : Installation_Class;
      Production   : Accord.Commodity.Commodity_Class);

   function Empty_Queue
     (Installation : Installation_Class)
      return Boolean;

   function First_Queued_Commodity
     (Installation : Installation_Class)
      return Accord.Commodity.Commodity_Class
     with Pre => not Empty_Queue (Installation);

   function First_Queued_Quantity
     (Installation : Installation_Class)
      return Concorde.Quantities.Quantity_Type
     with Pre => not Empty_Queue (Installation);

   procedure Update_Queue_First
     (Installation : Installation_Class;
      Quantity     : Concorde.Quantities.Quantity_Type)
     with Pre => not Empty_Queue (Installation);

   procedure Queue_Production
     (Installation : Installation_Class;
      Commodity    : Accord.Commodity.Commodity_Class;
      Quantity     : Concorde.Quantities.Quantity_Type)
     with Post => not Empty_Queue (Installation);

   procedure Queue_Capacity_Production
     (Installation : Installation_Class;
      Commodity    : Accord.Commodity.Commodity_Class)
     with Post => not Empty_Queue (Installation);

   procedure Iterate_Queue
     (Installation : Installation_Class;
      Process      : not null access
        procedure (Commodity    : Accord.Commodity.Commodity_Class;
                   Quantity     : Concorde.Quantities.Quantity_Type));

private

   function Image (X : Real) return String
                      renames Concorde.Real_Images.Approximate_Image;

end Concorde.Installations;
