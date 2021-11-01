with Accord.Module_Group;
with Accord.Terrain;

package Concorde.Terrain is

   function Ocean return Accord.Terrain.Terrain_Handle;

   function Module_Group_Cost
     (Terrain : Accord.Terrain.Terrain_Class;
      Group   : Accord.Module_Group.Module_Group_Class)
      return Non_Negative_Real;

end Concorde.Terrain;
