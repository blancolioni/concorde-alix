with Accord.Faction;

package Concorde.UI.Models.Current_Cash is

   type Current_Cash_Model_Record is
     new Dynamic_Text_Model with private;

   type Current_Cash_Model is access all Current_Cash_Model_Record'Class;

   function Current_Cash_Model_New
     (Faction : Accord.Faction.Faction_Handle)
      return Current_Cash_Model;

private

   type Current_Cash_Model_Record is
     new Dynamic_Text_Model with
      record
         Faction : Accord.Faction.Faction_Handle;
      end record;

   overriding function Current_Text
     (Model : Current_Cash_Model_Record)
      return String;

end Concorde.UI.Models.Current_Cash;
