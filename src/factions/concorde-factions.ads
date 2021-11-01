with Accord.Faction;
with Accord.User;

package Concorde.Factions is

   subtype Faction_Handle is Accord.Faction.Faction_Handle;
   subtype Faction_Class is Accord.Faction.Faction_Class;

   function Get_User_Faction
     (User : Accord.User.User_Class)
      return Faction_Class;

   function Economy
     (Faction : Faction_Class)
      return Non_Negative_Real;

   function Loyalty
     (Faction : Faction_Class)
      return Non_Negative_Real;

   function Stability
     (Faction : Faction_Class)
      return Non_Negative_Real;

   function Unrest
     (Faction : Faction_Class)
      return Non_Negative_Real;

   function Control_DC
     (Faction : Faction_Class)
      return Natural;

   procedure For_All_Factions
     (Process : not null access
        procedure (Faction : Faction_Class));

end Concorde.Factions;
