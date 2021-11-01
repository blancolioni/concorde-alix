with Concorde.Configure.Worlds;

with Concorde.Elementary_Functions;

with Accord.Deposit;
with Accord.Generation;
with Accord.Sector_Neighbour;
with Accord.Sector_Vertex;

with Accord.Db;

package body Concorde.Worlds is

   package World_Sector_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Accord.World_Sector.World_Sector_Handle,
        Accord.World_Sector."=");

   procedure Check_Surface
     (World : Accord.World.World_Class);

   -----------------
   -- Best_Sector --
   -----------------

   function Best_Sector
     (World : World_Class;
      Score : not null access
        function (Sector : Accord.World_Sector.World_Sector_Class)
      return Real)
      return Accord.World_Sector.World_Sector_Class
   is
      Best_Handle : Accord.World_Sector.World_Sector_Handle :=
                      Accord.World_Sector.Empty_Handle;
      Best_Score     : Real := Real'First;
   begin
      Check_Surface (World);
      for Sector of
        Accord.World_Sector.Select_By_World
          (World)
      loop
         declare
            This_Score : constant Real :=
                           Score (Sector);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Best_Handle := Sector.To_World_Sector_Handle;
            end if;
         end;
      end loop;
      return Best_Handle;
   end Best_Sector;

   -------------------
   -- Check_Surface --
   -------------------

   procedure Check_Surface
     (World : Accord.World.World_Class)
   is
      Gen    : constant Accord.Generation.Generation_Handle :=
                 Accord.Generation.Get_By_Is_Generated (World);
   begin
      if not Gen.Has_Element or else not Gen.Ready then
         Concorde.Configure.Worlds.Generate_Surface (World);
         if Gen.Has_Element then
            Gen.Update_Generation
              .Set_Ready (True)
              .Done;
         else
            Accord.Generation.Create (World, True);
         end if;
      end if;
   end Check_Surface;

   -------------------
   -- Circular_Scan --
   -------------------

   procedure Circular_Scan
     (Start : Accord.World_Sector.World_Sector_Class;
      Process : not null access
        function (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean)
   is
      subtype World_Sector_Handle is
        Accord.World_Sector.World_Sector_Handle;
      package Sector_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (World_Sector_Handle,
           Accord.World_Sector."=");

      Visited : Sector_Lists.List;
      Queued  : Sector_Lists.List;
   begin
      for Neighbour of Get_Neighbours (Start) loop
         Queued.Append (Neighbour);
      end loop;

      while not Queued.Is_Empty loop
         declare
            Current : constant World_Sector_Handle :=
                        Queued.First_Element;
         begin
            Queued.Delete_First;
            if not Visited.Contains (Current) then
               Visited.Append (Current);
               if not Process (Current) then
                  exit;
               end if;

               for Neighbour of Get_Neighbours (Current) loop
                  if not Visited.Contains (Neighbour) then
                     Queued.Append (Neighbour);
                  end if;
               end loop;
            end if;
         end;
      end loop;

   end Circular_Scan;

   -----------
   -- Clear --
   -----------

   procedure Clear (Selection : in out World_Selection'Class) is
   begin
      Selection.List.Clear;
   end Clear;

   ------------
   -- Filter --
   ------------

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : World_Class)
      return Boolean)
   is
      New_List : World_Lists.List;
      Changed  : Boolean := False;
   begin
      for World of Selection.List loop
         if Test (World) then
            New_List.Append (World);
         else
            Changed := True;
         end if;
      end loop;
      if Changed then
         Selection.List := New_List;
      end if;
   end Filter;

   -----------------
   -- Find_Sector --
   -----------------

   function Find_Sector
     (World : World_Class;
      Test : not null access
        function (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean)
      return Accord.World_Sector.World_Sector_Class
   is
   begin
      for Sector of
        Accord.World_Sector.Select_By_World
          (World)
      loop
         if Test (Sector) then
            return Sector;
         end if;
      end loop;
      return Accord.World_Sector.Empty_Handle;
   end Find_Sector;

   -----------------
   -- Get_Bearing --
   -----------------

   function Get_Bearing
     (From, To : Accord.World_Sector.World_Sector_Class)
      return Concorde.Trigonometry.Angle
   is
      use Concorde.Spheres;
      B   : constant Surface_Point := Get_Centre (From);
      C   : constant Surface_Point := Get_Centre (To);
   begin
      return Get_Bearing (B, C);
   end Get_Bearing;

   ----------------
   -- Get_Centre --
   ----------------

   function Get_Centre
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Sector_Vertex
   is
   begin
      return Sector_Vertex'
        (Sector.X, Sector.Y, Sector.Z);
   end Get_Centre;

   ----------------
   -- Get_Centre --
   ----------------

   function Get_Centre
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Sector_Position
   is
      use Concorde.Elementary_Functions;
      Vertex : constant Sector_Vertex := (Sector.X, Sector.Y, Sector.Z);
   begin
      return Sector_Position'
        (Latitude  => Arcsin (Vertex.Z),
         Longitude => Arctan (Vertex.Y, Vertex.X));
   end Get_Centre;

   ------------------
   -- Get_Distance --
   ------------------

   function Get_Distance
     (From, To : Accord.World_Sector.World_Sector_Class)
      return Non_Negative_Real
   is
      use Concorde.Elementary_Functions;
      P1 : constant Sector_Vertex := Get_Centre (From);
      P2 : constant Sector_Vertex := Get_Centre (To);
      D  : constant Non_Negative_Real :=
             Sqrt
               ((P1.X - P2.X) ** 2
                + (P1.Y - P2.Y) ** 2
                + (P1.Z - P2.Z) ** 2);
      A  : constant Real := 2.0 * Arcsin (D / 2.0);
      R  : constant Non_Negative_Real := From.World.Radius;
   begin
      return A * R;
   end Get_Distance;

   --------------------
   -- Get_Neighbours --
   --------------------

   function Get_Neighbours
     (Sector : Accord.World_Sector.World_Sector_Class)
      return World_Sector_Array
   is
      Result : World_Sector_Array (1 .. 20);
      Count  : Natural := 0;
   begin
      for Neighbour of
        Accord.Sector_Neighbour.Select_By_Sector
          (Sector)
      loop
         Count := Count + 1;
         Result (Count) :=
           Accord.World_Sector.Get_From_Sector
             (Neighbour.Neighbour)
           .To_World_Sector_Handle;
      end loop;
      return Result (1 .. Count);
   end Get_Neighbours;

   ---------------
   -- Get_Owner --
   ---------------

   function Get_Owner
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Accord.Faction.Faction_Class
   is
   begin
      return Sector.Faction;
   end Get_Owner;

   -----------------
   -- Get_Terrain --
   -----------------

   function Get_Terrain
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Accord.Terrain.Terrain_Class
   is
   begin
      return Sector.Terrain;
   end Get_Terrain;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Sector_Vertex_Array
   is
      Count  : Natural := 0;
      Result : Sector_Vertex_Array (1 .. 10);
   begin
      for Vertex of
        Accord.Sector_Vertex.Select_By_Sector (Sector)
      loop
         Count := Count + 1;
         Result (Count) := (Vertex.X, Vertex.Y, Vertex.Z);
      end loop;
      return Result (1 .. Count);
   end Get_Vertices;

   ---------------
   -- Get_World --
   ---------------

   function Get_World
     (Sector : Accord.World_Sector.World_Sector_Class)
      return World_Class
   is
   begin
      return Sector.World;
   end Get_World;

   ----------------
   -- Get_Worlds --
   ----------------

   function Get_Worlds
     (Selection : World_Selection'Class)
      return World_Array
   is
      Index : Natural := 0;
   begin
      return Arr : World_Array (1 .. Natural (Selection.List.Length)) do
         for World of Selection.List loop
            Index := Index + 1;
            Arr (Index) := World;
         end loop;
      end return;
   end Get_Worlds;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : World_Class)
   is
   begin
      Selection.List.Append (World.To_World_Handle);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Selection : World_Selection'Class) return Boolean is
   begin
      return Selection.List.Is_Empty;
   end Is_Empty;

   --------------------
   -- Is_Terrestrial --
   --------------------

   function Is_Terrestrial
     (World : World_Class)
      return Boolean
   is
      use all type Accord.Db.World_Composition;
   begin
      return World.Composition in Ice .. Rock_Iron;
   end Is_Terrestrial;

   --------------------
   -- Scan_Resources --
   --------------------

   procedure Scan_Resources
     (Sector  : Accord.World_Sector.World_Sector_Class;
      Process : not null access
        procedure (Resource : Accord.Resource.Resource_Class;
                   Concentration : Unit_Real;
                   Difficulty    : Unit_Real;
                   Available     : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Deposit of
        Accord.Deposit.Select_By_World_Sector
          (Sector)
      loop
         Process (Deposit.Resource, Deposit.Concentration,
                  Deposit.Difficulty, Deposit.Available);
      end loop;
   end Scan_Resources;

   ------------------
   -- Scan_Surface --
   ------------------

   procedure Scan_Surface
     (World : World_Class;
      Process : not null access
        procedure (Sector : Accord.World_Sector.World_Sector_Class))
   is
      List : World_Sector_Lists.List;
   begin
      Check_Surface (World);
      for Sector of Accord.World_Sector.Select_By_World (World) loop
         List.Append (Sector.To_World_Sector_Handle);
      end loop;

      for Sector of List loop
         Process (Sector);
      end loop;

   end Scan_Surface;

   ---------------
   -- Set_Owner --
   ---------------

   procedure Set_Owner
     (Sector  : Accord.World_Sector.World_Sector_Class;
      Faction : Accord.Faction.Faction_Handle)
   is
   begin
      Sector.Update_World_Sector
        .Set_Faction (Faction)
        .Done;
   end Set_Owner;

end Concorde.Worlds;
