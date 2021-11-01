with Nazar.Models.Table;

with Accord.Colony;
with Accord.Faction;

package Concorde.UI.Models.Colonies is

   function Faction_Colony_Table
     (Faction : Accord.Faction.Faction_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

   function Colony_Pop_Group_Model
     (Colony : Accord.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

   function Colony_Policy_Model
     (Colony : Accord.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

   function Colony_Market_Model
     (Colony : Accord.Colony.Colony_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Colonies;
