with Ada.Containers.Doubly_Linked_Lists;

package Concorde.Factions.Lists is
  new Ada.Containers.Doubly_Linked_Lists
    (Accord.Faction.Faction_Handle, Accord."=");
