with Accord.Db;

package Concorde.Climates is

   function Airless return Accord.Climate_Reference;
   function Iceball return Accord.Climate_Reference;
   function Jovian return Accord.Climate_Reference;
   function Martian return Accord.Climate_Reference;
   function Temperate return Accord.Climate_Reference;
   function Venusian return Accord.Climate_Reference;
   function Water return Accord.Climate_Reference;

   function Name
     (Climate : Accord.Climate_Reference)
      return String;

   function Habitability
     (Climate : Accord.Climate_Reference)
      return Unit_Real;

   function Default_Terrain
     (Climate : Accord.Climate_Reference)
      return Accord.Terrain_Handle;

end Concorde.Climates;
