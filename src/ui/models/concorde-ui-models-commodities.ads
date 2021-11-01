with Nazar.Models.Table;

with Accord.Commodity;

package Concorde.UI.Models.Commodities is

   function Commodity_Market_Model
     (Commodity : Accord.Commodity.Commodity_Class)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Commodities;
