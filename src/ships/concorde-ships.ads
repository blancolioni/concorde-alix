with Accord.Colony;
with Accord.Faction;
with Accord.Ship;
with Accord.Ship_Design;
with Accord.World;

package Concorde.Ships is

   subtype Ship_Class is Accord.Ship.Ship_Class;

   procedure Create_Ship
     (Owner   : Accord.Faction.Faction_Class;
      Home    : Accord.Colony.Colony_Class;
      World   : Accord.World.World_Class;
      Design  : Accord.Ship_Design.Ship_Design_Class;
      Manager : String;
      Name    : String);

   function Current_Mass
     (Ship : Ship_Class)
     return Non_Negative_Real;

   function Jump_Speed
     (Ship : Ship_Class)
      return Non_Negative_Real;
   --  speed of light = 1

end Concorde.Ships;
