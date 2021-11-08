with Concorde.Worlds;

with Accord.Faction;
with Accord.Star;
with Accord.Star_System;

package Concorde.Star_Systems is

   type Interstellar_Position is
      record
         X, Y, Z : Real;
      end record;

   subtype Star_System_Class is Accord.Star_System.Star_System_Class;

   function Owner
     (Star_System : Star_System_Class)
      return Accord.Faction.Faction_Class;

   function Position
     (Star_System : Star_System_Class)
      return Interstellar_Position;

   function Distance
     (From, To : Star_System_Class)
      return Non_Negative_Real;

   function Primary
     (Star_System : Star_System_Class)
      return Accord.Star.Star_Class;

   function First return Star_System_Class;

   function Find_Exact
     (Name : String)
      return Star_System_Class;

   procedure Claim
     (Star_System : Star_System_Class);

   function Worlds
     (Star_System : Star_System_Class)
      return Concorde.Worlds.World_Selection;

   function Terrestrial_Worlds
     (Star_System : Star_System_Class)
      return Concorde.Worlds.World_Selection;

end Concorde.Star_Systems;
