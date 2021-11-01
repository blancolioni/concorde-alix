with Accord.Commodity;
with Accord.Resource;
with Accord.World_Sector;

package Concorde.Commodities is

   subtype Commodity_Class is Accord.Commodity.Commodity_Class;
   subtype Commodity_Handle is Accord.Commodity.Commodity_Handle;

   subtype Resource_Class is Accord.Resource.Resource_Class;
   subtype Resource_Handle is Accord.Resource.Resource_Handle;

   function Exists (Tag : String) return Boolean;
   function Get (Tag : String) return Commodity_Class
     with Pre => Exists (Tag);

   function Happiness (Commodity : Commodity_Class) return Unit_Real;

   function Yield_Estimate
     (Resource : Resource_Class;
      Sector   : Accord.World_Sector.World_Sector_Class)
      return Non_Negative_Real;

end Concorde.Commodities;
