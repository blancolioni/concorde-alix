with Ada.Containers.Vectors;

with Nazar.Models.Array_Table;
with Nazar.Values;

with Concorde.Calendar;
with Concorde.Money;
with Concorde.Quantities;

with Concorde.Agents;

with Concorde.Updates.Events;

with Accord.Colony;

package body Concorde.UI.Models.Colonies.Factions is

   type Faction_Colonies_Table_Column is
     (World, Population, Cash);

   type Faction_Colony_Record is
      record
         Colony          : Accord.Colony.Colony_Handle;
         Population_Node : Accord.Network_Value.Network_Value_Handle;
      end record;

   Column_Type_Array : constant array (Faction_Colonies_Table_Column)
     of Nazar.Values.Nazar_Value_Type :=
       (others => Nazar.Values.Text_Value_Type);

   package Faction_Colony_Vectors is
     new Ada.Containers.Vectors
       (Positive, Faction_Colony_Record);

   type Faction_Model_Record is
     new Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record with
      record
         Faction : Accord.Faction.Faction_Handle;
         State   : Faction_Colony_Vectors.Vector;
      end record;

   overriding function Row_Count
     (Model : Faction_Model_Record)
      return Natural
   is (Model.State.Last_Index);

   overriding function Column_Count
     (Model : Faction_Model_Record)
      return Natural
   is (Faction_Colonies_Table_Column'Pos
       (Faction_Colonies_Table_Column'Last) + 1);

   overriding function Column_Name
     (Model        : Faction_Model_Record;
      Column_Index : Positive)
      return String
   is (Faction_Colonies_Table_Column'Image
       (Faction_Colonies_Table_Column'Val (Column_Index - 1)));

   overriding function Column_Heading
     (Model        : Faction_Model_Record;
      Column_Index : Positive)
      return String
   is (Faction_Model_Record'Class (Model).Column_Name (Column_Index));

   overriding function Column_Type
     (Model        : Faction_Model_Record;
      Column_Index : Positive)
      return Nazar.Values.Nazar_Value_Type
   is (Column_Type_Array
       (Faction_Colonies_Table_Column'Val (Column_Index - 1)));

   overriding function Element
     (Model       : Faction_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value;

   type Faction_Model_Access is access all Faction_Model_Record'Class;

   procedure Load
     (Model : in out Faction_Model_Record'Class);

   type Faction_Model_Update is
     new Concorde.Updates.Root_Update_Type with
      record
         Model : Faction_Model_Access;
      end record;

   overriding procedure Activate
     (Update : Faction_Model_Update);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate (Update : Faction_Model_Update) is
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
     (Model       : Faction_Model_Record;
      Row, Column : Positive)
      return Nazar.Values.Nazar_Value
   is
      Info  : Faction_Colony_Record renames Model.State (Row);
      Value : constant String :=
                (case Faction_Colonies_Table_Column'Val (Column - 1) is
                    when World =>
                      Info.Colony.World.Name,
                    when Population =>
                      Concorde.Quantities.Show
                   (Concorde.Quantities.To_Quantity
                      (Info.Population_Node.Current_Value)),
                    when Cash =>
                      Concorde.Money.Show
                   (Concorde.Agents.Cash (Info.Colony)));
   begin
      return Nazar.Values.To_Value (Value);
   end Element;

   ----------
   -- Load --
   ----------

   procedure Load
     (Model : in out Faction_Model_Record'Class)
   is
   begin
      Model.State.Clear;
      for Colony of
        Accord.Colony.Select_By_Faction (Model.Faction)
      loop
         declare
            function Node_Reference
              (Name : String)
               return Accord.Network_Value.Network_Value_Handle
            is (Accord.Network_Value.Get_By_Network_Value
                 (Colony,
                  Accord.Node.Get_By_Tag (Name)));

            Info : constant Faction_Colony_Record :=
                     Faction_Colony_Record'
                       (Colony          => Colony.To_Colony_Handle,
                        Population_Node =>
                          Node_Reference ("everybody-population"));
         begin
            Model.State.Append (Info);
         end;
      end loop;
   end Load;

   -----------
   -- Model --
   -----------

   function Model
     (Faction : Accord.Faction.Faction_Handle)
      return Nazar.Models.Table.Nazar_Table_Model
   is
      Result : constant Faction_Model_Access := new Faction_Model_Record'
        (Nazar.Models.Array_Table.Nazar_Array_Table_Model_Record
           with Faction => Faction, State => <>);
   begin
      Result.Load;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1),
         Faction_Model_Update'(Model => Result));
      return Nazar.Models.Table.Nazar_Table_Model (Result);
   end Model;

end Concorde.UI.Models.Colonies.Factions;
