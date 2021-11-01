with Accord.Module_Group;
with Accord.Resource;
with Accord.World_Sector;

package Concorde.Sectors is

   function Resource_Yield
     (Sector   : Accord.World_Sector.World_Sector_Class;
      Resource : Accord.Resource.Resource_Class)
      return Non_Negative_Real;

   function Module_Group_Cost
     (Sector   : Accord.World_Sector.World_Sector_Class;
      Group    : Accord.Module_Group.Module_Group_Class)
      return Non_Negative_Real;

end Concorde.Sectors;
