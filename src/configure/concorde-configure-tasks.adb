package body Concorde.Configure.Tasks is

   ------------------------
   -- Configuration_Work --
   ------------------------

   protected body Configuration_Work is

      -------------------
      -- Add_Work_Item --
      -------------------

      procedure Add_Work_Item
        (Identifier : String; Step_Count : Natural)
      is
      begin
         Work_Items.Append
           (Work_Item'(Identifier'Length, Identifier, Step_Count));
         Total_Steps := Total_Steps + Step_Count;
      end Add_Work_Item;

      -----------------------
      -- Current_Work_Item --
      -----------------------

      function Current_Work_Item return String is
      begin
         if Current_Item > Work_Items.Last_Index then
            return "";
         else
            return Work_Items.Element (Current_Item).Name;
         end if;
      end Current_Work_Item;

      ------------
      -- Finish --
      ------------

      procedure Finish is
      begin
         Finished_Flag := True;
      end Finish;

      --------------
      -- Finished --
      --------------

      function Finished return Boolean is
      begin
         return Finished_Flag;
      end Finished;

      ---------------
      -- Get_State --
      ---------------

      procedure Get_State
        (Current : out Natural; Total : out Natural)
      is
      begin
         Current := Current_Step;
         Total := Total_Steps;
      end Get_State;

      ----------
      -- Step --
      ----------

      procedure Step is
      begin
         Current_Step := Current_Step + 1;
         Current_Item_Step := Current_Item_Step + 1;
         if Current_Item_Step >=
           Work_Items.Element (Current_Item).Step_Count
         then
            Current_Item := Current_Item + 1;
            Current_Item_Step := 0;
         end if;
      end Step;

   end Configuration_Work;

end Concorde.Configure.Tasks;
