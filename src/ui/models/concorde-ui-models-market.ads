with Nazar.Models.Table;

with Accord.Market;

package Concorde.UI.Models.Market is

   function Market_Model
     (Market : Accord.Market.Market_Handle)
      return Nazar.Models.Table.Nazar_Table_Model;

end Concorde.UI.Models.Market;
