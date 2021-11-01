with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Commodities;
with Concorde.Markets;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Updates.Events;

with Accord.Commodity;
with Accord.Commodity;

package body Concorde.UI.Models.Market is

   type Market_Table_Column is
     (Commodity_Name,
      Demand, Supply, Last_Price);

   type Commodity_Record is
      record
         Commodity  : Accord.Commodity.Commodity_Handle;
         Demand     : Concorde.Quantities.Quantity_Type;
         Supply     : Concorde.Quantities.Quantity_Type;
         Last_Price : Concorde.Money.Price_Type;
      end record;

   Column_Type_Array : constant array (Market_Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (Commodity_Name => Nazar.Values.Text_Value_Type,
        Demand         => Nazar.Values.Text_Value_Type,
        Supply         => Nazar.Values.Text_Value_Type,
        Last_Price     => Nazar.Values.Text_Value_Type);

   package Commodity_Vectors is
     new Ada.Containers.Vectors
       (Positive, Commodity_Record);

   type Market_Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         Market : Accord.Market_Reference;
         State  : Commodity_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Market_Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Market_Model_Record)
      return Natural
   is (Market_Table_Column'Pos (Market_Table_Column'Last) + 1);

   overriding function Column_Name
     (Model : Market_Model_Record;
      Column_Index : Positive)
      return String
   is (case Market_Table_Column'Val (Column_Index - 1) is
          when Commodity_Name =>
             "commodity-name",
          when Demand         =>
             "demand",
          when Supply         =>
             "supply",
          when Last_Price     =>
             "last-price");

   overriding function Column_Heading
     (Model : Market_Model_Record;
      Column_Index : Positive)
      return String
   is (Market_Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model : Market_Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array (Market_Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model : Market_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Market_Model_Access is access all Market_Model_Record'Class;

   procedure Load
     (Model : in out Market_Model_Record'Class);

   type Market_Model_Update is
     new Concorde.Updates.Root_Update_Type with
      record
         Model : Market_Model_Access;
      end record;

   overriding procedure Activate
     (Update : Market_Model_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Market_Model_Update) is
   begin
      Update.Model.Load;
      Update.Model.Notify_Observers;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1), Update);
   end Activate;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Model : Market_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      Info : Commodity_Record renames
               Model.State (Row);
      Value : constant String :=
                (case Market_Table_Column'Val (Column - 1) is
                    when Commodity_Name =>
                      Info.Commodity.Tag,
                    when Demand         =>
                      Concorde.Quantities.Show (Info.Demand),
                    when Supply         =>
                      Concorde.Quantities.Show (Info.Supply),
                    when Last_Price     =>
                      Concorde.Money.Show (Info.Last_Price));
   begin
      return Nazar.Values.To_Value (Value);
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Market_Model_Record'Class)
   is
   begin
      Model.State.Clear;
      for Commodity of
        Accord.Commodity.Scan_By_Tag
      loop
         declare
            use Concorde.Money, Concorde.Quantities;
            Ref : constant Accord.Commodity.Commodity_Class :=
                    Commodity.Get_Commodity_Reference;
            Info : constant Commodity_Record :=
                     Commodity_Record'
                       (Commodity  => Accord.Commodity.Get (Ref),
                        Demand     =>
                          Concorde.Markets.Historical_Offer_Quantity
                            (Model.Market,
                             Concorde.Commodities.Get_Commodity (Ref),
                             Accord.Bid, Concorde.Calendar.Days (1)),
                        Supply     =>
                          Concorde.Markets.Historical_Offer_Quantity
                            (Model.Market,
                             Concorde.Commodities.Get_Commodity (Ref),
                             Accord.Ask, Concorde.Calendar.Days (1)),
                        Last_Price =>
                          Concorde.Markets.Historical_Mean_Price
                            (Model.Market,
                             Concorde.Commodities.Get_Commodity (Ref)));
         begin
            if Info.Demand > Zero
              or else Info.Supply > Zero
            then
               Model.State.Append (Info);
            end if;
         end;
      end loop;
   end Load;

   ------------------
   -- Market_Model --
   ------------------

   function Market_Model
     (Market : Accord.Market.Market_Handle)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Model : constant Market_Model_Access := new Market_Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
         Market => Market.Reference,
         State  => <>);
   begin
      Model.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Market_Model_Update'(Model => Model));
      return Nazar.Models.Table.Nazar_Table_Model (Model);
   end Market_Model;

end Concorde.UI.Models.Market;
