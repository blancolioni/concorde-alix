package body Concorde.UI.Models.Current_Date is

   ----------------------------
   -- Current_Date_Model_New --
   ----------------------------

   function Current_Date_Model_New return Current_Date_Model is
   begin
      return Model : constant Current_Date_Model :=
        new Current_Date_Model_Record
      do
         Model.Initialize
           (Concorde.Calendar.Image (Concorde.Calendar.Clock),
            Concorde.Calendar.Days (1));
      end return;
   end Current_Date_Model_New;

end Concorde.UI.Models.Current_Date;
