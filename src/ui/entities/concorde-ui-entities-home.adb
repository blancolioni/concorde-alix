with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Unbounded;

with Concorde.UI.Entities.Directories;
with Concorde.UI.Entities.Generic_Database_Entity;

with Accord.Faction;
with Accord.World;
with Accord.Owned_World;

with Accord.Db.World;

package body Concorde.UI.Entities.Home is

   function Get_Owned_World
     (Faction : Accord.Faction.Faction_Handle;
      Name    : String)
      return Accord.World.World_Handle;

   procedure Iterate_Owned_Worlds
     (Faction : Accord.Faction.Faction_Handle;
      Process : not null access
        procedure (World : Accord.World.World_Handle));

   function World_Contents
     (World : Accord.World.World_Handle)
      return String;

   package World_Directory is
     new Concorde.UI.Entities.Generic_Database_Entity
       (Container_Handle      => Accord.Faction.Faction_Handle,
        Record_Handle         => Accord.World.World_Handle,
        Get_By_Name           => Get_Owned_World,
        Iterate               => Iterate_Owned_Worlds,
        Contents              => World_Contents);

   ---------------------
   -- Get_Owned_World --
   ---------------------

   function Get_Owned_World
     (Faction : Accord.Faction.Faction_Handle;
      Name    : String)
      return Accord.World.World_Handle
   is
      World : constant Accord.World.World_Handle :=
                Accord.World.First_By_Name (Name);
   begin
      if World.Has_Element
        and then Accord.Owned_World.Is_Owned_World
          (Faction, World)
      then
         return World;
      else
         return Accord.World.Empty_Handle;
      end if;
   end Get_Owned_World;

   ---------------
   -- Home_Node --
   ---------------

   function Home_Node return Entity_Reference is
   begin
      return Node : constant Entity_Reference :=
        Directories.Create_Directory_Node
      do
         for Faction of Accord.Faction.Scan_By_Name loop
            declare
               Faction_Home : constant Entity_Reference :=
                 Directories.Create_Directory_Node;
            begin
               Faction_Home.Update.Bind_Child
                 ("worlds",
                  World_Directory.Get_Container_Node
                    (Accord.Faction.Faction_Handle (Faction)));
               Node.Update.Bind_Child
                 (Faction.Name, Faction_Home);
            end;
         end loop;
      end return;
   end Home_Node;

   --------------------------
   -- Iterate_Owned_Worlds --
   --------------------------

   procedure Iterate_Owned_Worlds
     (Faction : Accord.Faction.Faction_Handle;
      Process : not null access
        procedure (World : Accord.World.World_Handle))
   is
   begin
      for Owned of
        Accord.Owned_World.Select_By_Faction (Faction)
      loop
         Process (Accord.World.World_Handle (Owned.World));
      end loop;
   end Iterate_Owned_Worlds;

   --------------------
   -- World_Contents --
   --------------------

   function World_Contents
     (World : Accord.World.World_Handle)
      return String
   is
      package String_Vectors is
        new Ada.Containers.Indefinite_Vectors (Positive, String);

      Headings : String_Vectors.Vector;
      Values   : String_Vectors.Vector;

      procedure Add (Field_Name : String);

      function Collate return String;

      ---------
      -- Add --
      ---------

      procedure Add (Field_Name : String) is
      begin
         Headings.Append (Field_Name);
         Values.Append
           (Accord.Db.World.Get (World.Reference_World)
                .Get (Field_Name));
      end Add;

      -------------
      -- Collate --
      -------------

      function Collate return String is
         use Ada.Strings.Unbounded;
         Result : Unbounded_String;
      begin
         for I in 1 .. Headings.Last_Index loop
            Result := Result & Headings.Element (I) & ": "
              & Values.Element (I) & Character'Val (10);
         end loop;
         return To_String (Result);
      end Collate;

   begin
      Add ("name");
      Add ("category");
      Add ("climate");
      Add ("habitability");

      return Collate;
   end World_Contents;

end Concorde.UI.Entities.Home;
