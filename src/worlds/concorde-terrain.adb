with Accord.Terrain_Module_Cost;

package body Concorde.Terrain is

   -----------------------
   -- Module_Group_Cost --
   -----------------------

   function Module_Group_Cost
     (Terrain : Accord.Terrain.Terrain_Class;
      Group   : Accord.Module_Group.Module_Group_Class)
      return Non_Negative_Real
   is
      Cost : constant Accord.Terrain_Module_Cost.Terrain_Module_Cost_Class :=
               Accord.Terrain_Module_Cost.Get_By_Terrain_Module_Cost
                 (Terrain, Group);
   begin
      if Cost.Has_Element then
         return Cost.Cost;
      else
         return 1.0;
      end if;
   end Module_Group_Cost;

   -----------
   -- Ocean --
   -----------

   function Ocean
     return Accord.Terrain.Terrain_Handle
   is
   begin
      return Accord.Terrain.Get_By_Tag ("ocean");
   end Ocean;

end Concorde.Terrain;
