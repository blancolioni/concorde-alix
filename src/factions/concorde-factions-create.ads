with Tropos;

with Concorde.Color;

with Accord.Faction;
with Accord.User;

package Concorde.Factions.Create is

   function Create_Faction
     (User        : Accord.User.User_Class;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Concorde.Color.Concorde_Color;
      Setup       : Tropos.Configuration)
      return Accord.Faction.Faction_Class;

end Concorde.Factions.Create;
