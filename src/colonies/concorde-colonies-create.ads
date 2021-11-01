with Tropos;

with Accord.Faction;
with Accord.World;
with Accord.World_Sector;

package Concorde.Colonies.Create is

   procedure Initial_Colony
     (Faction : Accord.Faction.Faction_Class;
      World   : Accord.World.World_Class;
      Capital : Accord.World_Sector.World_Sector_Class;
      Init    : Tropos.Configuration);

end Concorde.Colonies.Create;
