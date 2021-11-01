with Nazar.Models.Draw;

with Accord.Faction;
with Accord.World;

package Concorde.UI.Models.World is

   type Display_Type is
     (Elevation_Display,
      Moisture_Display,
      Temperature_Display,
      Terrain_Display);

   function World_Model
     (Faction    : Accord.Faction.Faction_Handle;
      World      : Accord.World.World_Class;
      Display    : Display_Type;
      Show_Owner : Boolean;
      Show_Wind  : Boolean)
      return Nazar.Models.Draw.Nazar_Draw_Model;

end Concorde.UI.Models.World;
