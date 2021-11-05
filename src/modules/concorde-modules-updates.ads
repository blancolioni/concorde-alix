with Accord.Has_Stock;
with Accord.World_Sector;

package Concorde.Modules.Updates is

   procedure Daily_Update
     (Module : Module_Class;
      Sector : Accord.World_Sector.World_Sector_Class;
      Stock  : Accord.Has_Stock.Has_Stock_Class);

end Concorde.Modules.Updates;
