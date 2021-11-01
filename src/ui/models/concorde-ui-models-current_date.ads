private with Concorde.Calendar;

package Concorde.UI.Models.Current_Date is

   type Current_Date_Model_Record is
     new Dynamic_Text_Model with private;

   type Current_Date_Model is access all Current_Date_Model_Record'Class;

   function Current_Date_Model_New return Current_Date_Model;

private

   type Current_Date_Model_Record is
     new Dynamic_Text_Model with
      record
         null;
      end record;

   overriding function Current_Text
     (Model : Current_Date_Model_Record)
      return String
   is (Concorde.Calendar.Image (Concorde.Calendar.Clock));

end Concorde.UI.Models.Current_Date;
