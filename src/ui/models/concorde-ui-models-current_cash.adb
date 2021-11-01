with Nazar.Logging;

with Accord.Account;

with Concorde.Calendar;
with Concorde.Money;

package body Concorde.UI.Models.Current_Cash is

   ----------------------------
   -- Current_Cash_Model_New --
   ----------------------------

   function Current_Cash_Model_New
     (Faction : Accord.Faction.Faction_Handle)
      return Current_Cash_Model
   is
   begin
      return Model : constant Current_Cash_Model :=
        new Current_Cash_Model_Record
      do
         Model.Faction := Faction;
         Model.Initialize
           (Concorde.Money.Show (Faction.Account.Cash),
            Concorde.Calendar.Days (1));
      end return;
   end Current_Cash_Model_New;

   ------------------
   -- Current_Text --
   ------------------

   overriding function Current_Text
     (Model : Current_Cash_Model_Record)
      return String
   is
      Cash    : constant Concorde.Money.Money_Type :=
        Model.Faction.Account.Cash;
   begin
      Nazar.Logging.Log
        (Model, Concorde.Money.Show (Cash));
      return Concorde.Money.Show (Cash);
   end Current_Text;

end Concorde.UI.Models.Current_Cash;
