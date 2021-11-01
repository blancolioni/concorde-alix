with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Real_Images;

with Concorde.Updates.Events;

with Accord.Colony;

with Accord.World;

with Accord.Colony_Price;

package body Concorde.UI.Models.Commodities is

   type Table_Column is
     (World_Name, Demand, Supply, Share, Pressure, Price);

   type Row_Record is
      record
         World      : Accord.World.World_Handle;
         Colony     : Accord.Colony.Colony_Handle;
         Demand     : Accord.Network_Value.Network_Value_Handle;
         Supply     : Accord.Network_Value.Network_Value_Handle;
         Share      : Accord.Network_Value.Network_Value_Handle;
         Pressure   : Accord.Network_Value.Network_Value_Handle;
         Price      : Accord.Network_Value.Network_Value_Handle;
      end record;

   Column_Type_Array : constant array (Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (World_Name   => Nazar.Values.Text_Value_Type,
        Demand       => Nazar.Values.Text_Value_Type,
        Supply       => Nazar.Values.Text_Value_Type,
        Share        => Nazar.Values.Text_Value_Type,
        Pressure     => Nazar.Values.Text_Value_Type,
        Price        => Nazar.Values.Text_Value_Type);

   package Row_Vectors is
     new Ada.Containers.Vectors
       (Positive, Row_Record);

   type Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         Commodity : Accord.Commodity.Commodity_Handle;
         State     : Row_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Model_Record)
      return Natural
   is (Table_Column'Pos (Table_Column'Last) + 1);

   overriding function Column_Name
     (Model : Model_Record;
      Column_Index : Positive)
      return String
   is (Table_Column'Image
       (Table_Column'Val (Column_Index - 1)));

   overriding function Column_Heading
     (Model : Model_Record;
      Column_Index : Positive)
      return String
   is (Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model : Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array (Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model : Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Model_Access is
     access all Model_Record'Class;

   procedure Load
     (Model : in out Model_Record'Class);

   type Market_Model_Update is
     new Concorde.Updates.Root_Update_Type with
      record
         Model : Model_Access;
      end record;

   overriding procedure Execute
     (Update : Market_Model_Update);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Update : Market_Model_Update) is
   begin
      Update.Model.Load;
      Update.Model.Notify_Observers;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1), Update);
   end Execute;

   ----------------------------
   -- Commodity_Market_Model --
   ----------------------------

   function Commodity_Market_Model
     (Commodity : Accord.Commodity.Commodity_Class)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Model : constant Model_Access := new Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
         Commodity => Commodity.To_Commodity_Handle,
         State  => <>);
   begin
      Model.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Market_Model_Update'(Model => Model));
      return Nazar.Models.Table.Nazar_Table_Model (Model);
   end Commodity_Market_Model;

   -------------
   -- Element --
   -------------

   overriding function Element
     (Model : Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      Info  : Row_Record renames Model.State (Row);
      Col   : constant Table_Column :=
                Table_Column'Val (Column - 1);

      function Get_Value
        (V : Accord.Network_Value.Network_Value_Class)
         return Real
      is (V.Current_Value);

      function Quantity (X : Real) return String
      is (Concorde.Quantities.Show (Concorde.Quantities.To_Quantity (X)));

      function Supply return String
      is (Quantity (Get_Value (Info.Supply)));

      function Demand return String
      is (Quantity (Get_Value (Info.Demand)));

      function Share return String
      is (Image (Get_Value (Info.Share) * 100.0));

      function Pressure return String
      is (Image (Get_Value (Info.Pressure) * 100.0));

      function Price return String
      is (Concorde.Money.Show
          (Concorde.Money.Adjust_Price
           (Accord.Colony_Price.Get_By_Commodity_Price
            (Info.Colony, Model.Commodity)
            .Price,
            1.0 + Get_Value (Info.Price))));

      Value : constant String :=
                (case Col is
                    when World_Name => Info.World.Name,
                    when Supply     => Supply,
                    when Demand     => Demand,
                    when Share      => Share,
                    when Pressure   => Pressure,
                    when Price      => Price);
   begin
      return Nazar.Values.To_Value (Value);
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Model_Record'Class)
   is

      Tag : constant String :=
              Model.Commodity.Tag;

      procedure Add (Colony : Accord.Colony.Colony_Class);

      ---------
      -- Add --
      ---------

      procedure Add (Colony : Accord.Colony.Colony_Class) is

         use Accord.Network_Value;

         function Net (Suffix : String)
                       return Network_Value_Handle
         is (Get_By_Network_Value
             (Colony,
              Accord.Node.Get_By_Tag
                (Tag & "-" & Suffix)));

         Demand   : constant Network_Value_Handle :=
                      Net ("demand");
         Supply   : constant Network_Value_Handle :=
                      Net ("supply");
         Share    : constant Network_Value_Handle :=
                      Net ("share");
         Pressure : constant Network_Value_Handle :=
                      Net ("p-prod");
         Price    : constant Network_Value_Handle :=
                      Net ("price");

      begin
         Model.State.Append
           (Row_Record'
              (World    => Colony.World.To_World_Handle,
               Colony   => Colony.To_Colony_Handle,
               Demand    => Demand,
               Supply    => Supply,
               Share     => Share,
               Pressure  => Pressure,
               Price     => Price));
      end Add;

   begin
      Model.State.Clear;
      for Colony of
        Accord.Colony.Scan_By_Top_Record
      loop
         Add (Colony);
      end loop;
   end Load;

end Concorde.UI.Models.Commodities;
