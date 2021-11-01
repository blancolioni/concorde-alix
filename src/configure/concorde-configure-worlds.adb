with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with WL.Random.Height_Maps;

with Concorde.Constants;
with Concorde.Elementary_Functions;
with Concorde.Identifiers;
with Concorde.Logging;
with Concorde.Solar_System;
with Concorde.Surfaces;
with Concorde.Terrain;
with Concorde.Trigonometry;

with Concorde.Configure.Resources;

with Accord.Climate_Terrain;
with Accord.Faction;
with Accord.Feature;
with Accord.Sector;
with Accord.Sector_Neighbour;
with Accord.Sector_Use;
with Accord.Sector_Vertex;
with Accord.Star_System;
with Accord.Terrain;
with Accord.World_Sector;

with Accord.Db;

package body Concorde.Configure.Worlds is

   package Heights renames WL.Random.Height_Maps;

   type Climate_Terrain_Record is
      record
         Terrain   : Accord.Terrain.Terrain_Handle;
         First     : Positive;
         Last      : Natural;
         Frequency : Unit_Real;
         Count     : Natural;
      end record;

   package Climate_Terrain_Vectors is
     new Ada.Containers.Vectors (Positive, Climate_Terrain_Record);

   procedure Save_Surface
     (Surface : Concorde.Surfaces.Surface_Type;
      World   : Accord.World.World_Class);

   procedure Get_Climate_Terrain
     (World  : Accord.World.World_Class;
      Vector : out Climate_Terrain_Vectors.Vector)
     with Unreferenced;

   function Get_Frequencies
     (World : Accord.World.World_Class)
      return Heights.Frequency_Map;

   type Terrain_Checker is access
     function (Sector  : Accord.World_Sector.World_Sector_Class)
               return Boolean;

   type Terrain_Checker_Record is
      record
         Terrain : Accord.Terrain.Terrain_Handle;
         Checker : Terrain_Checker;
      end record;

   package Terrain_Checker_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Terrain_Checker_Record);

   Terrain_Checkers : Terrain_Checker_Lists.List;

   procedure Load_Terrain_Checkers;

   function Check_Cold_Desert
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Desert
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Forest
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Hill
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Jungle
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Marsh
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Plain
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Savannah
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Steppe
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Tundra
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   function Check_Water
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean;

   -----------------------
   -- Check_Cold_Desert --
   -----------------------

   function Check_Cold_Desert
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
   begin
      return Sector.Moisture < 0.1
        and then Sector.Average_Temperature < 280.0
        and then Sector.Elevation >= 0;
   end Check_Cold_Desert;

   ------------------
   -- Check_Desert --
   ------------------

   function Check_Desert
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
   begin
      return Sector.Moisture < 0.1
        and then Sector.Average_Temperature >= 280.0
        and then Sector.Elevation >= 0;
   end Check_Desert;

   ------------------
   -- Check_Forest --
   ------------------

   function Check_Forest
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
      use type Accord.Db.Life_Complexity_Type;
   begin
      return Sector.World.Hydrosphere > 0.0
        and then Sector.World.Life > Accord.Db.Single_Celled
        and then Sector.Elevation >= 0
        and then Sector.Average_Temperature in 274.0 .. 290.0
        and then Sector.Moisture >= 0.2;
   end Check_Forest;

   ----------------
   -- Check_Hill --
   ----------------

   function Check_Hill
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
   begin
      return Sector.Elevation > 8
        and then Sector.Average_Temperature >= 280.0;
   end Check_Hill;

   ------------------
   -- Check_Jungle --
   ------------------

   function Check_Jungle
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
      use type Accord.Db.Life_Complexity_Type;
   begin
      return Sector.World.Hydrosphere > 0.0
        and then Sector.World.Life > Accord.Db.Single_Celled
        and then Sector.Elevation >= 0
        and then Sector.Average_Temperature > 290.0
        and then Sector.Moisture > 0.8;
   end Check_Jungle;

   -----------------
   -- Check_Marsh --
   -----------------

   function Check_Marsh
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
      use type Accord.Db.Life_Complexity_Type;
   begin
      return Sector.World.Hydrosphere > 0.0
        and then Sector.World.Life > Accord.Db.Single_Celled
        and then Sector.Elevation in 0 .. 2
        and then Sector.Average_Temperature >= 280.0
        and then Sector.Moisture > 0.5;
   end Check_Marsh;

   -----------------
   -- Check_Plain --
   -----------------

   function Check_Plain
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
   begin
      return Sector.Elevation >= 0;
   end Check_Plain;

   --------------------
   -- Check_Savannah --
   --------------------

   function Check_Savannah
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
      use type Accord.Db.Life_Complexity_Type;
   begin
      return Sector.World.Hydrosphere > 0.0
        and then Sector.World.Life > Accord.Db.Single_Celled
        and then Sector.Elevation in 1 .. 10
        and then Sector.Average_Temperature > 280.0
        and then Sector.Moisture > 0.1;
   end Check_Savannah;

   ------------------
   -- Check_Steppe --
   ------------------

   function Check_Steppe
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
      use type Accord.Db.Life_Complexity_Type;
   begin
      return Sector.World.Life > Accord.Db.Prebiotic
        and then Sector.Elevation >= 0
        and then Sector.Average_Temperature < 290.0
        and then Sector.Moisture in 0.1 .. 0.2;
   end Check_Steppe;

   ------------------
   -- Check_Tundra --
   ------------------

   function Check_Tundra
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
      use type Accord.Db.Life_Complexity_Type;
   begin
      return Sector.World.Hydrosphere > 0.0
        and then Sector.World.Life > Accord.Db.Prebiotic
        and then Sector.Elevation >= 0
        and then Sector.Average_Temperature < 280.0
        and then Sector.Moisture in 0.1 .. 0.2;
   end Check_Tundra;

   -----------------
   -- Check_Water --
   -----------------

   function Check_Water
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean
   is
   begin
      return Sector.World.Hydrosphere > 0.0
        and then Sector.Elevation < 0;
   end Check_Water;

   ----------------------
   -- Generate_Surface --
   ----------------------

   procedure Generate_Surface
     (World : Accord.World.World_Class)
   is
      use Accord.Db;
      Tile_Count : Natural;
      Radius     : constant Non_Negative_Real :=
                     World.Radius
                       / Concorde.Solar_System.Earth_Radius;
   begin

      case World.Composition is
         when Ice | Rock | Rock_Ice | Rock_Iron =>
            Tile_Count := Natural (Radius * 400.0);
         when Hydrogen | Gaseous =>
            Tile_Count := 0;
      end case;

      if Tile_Count > 0 then
         declare
            Surface   : Concorde.Surfaces.Root_Surface_Type;
         begin
            Concorde.Logging.Log
              ("generator", "surfaces", World.Name,
               "Creating surface with "
               & Tile_Count'Image & " tiles");
            Surface.Create_Voronoi_Partition (Tile_Count);
            Save_Surface (Surface, World);

            Concorde.Logging.Log
              ("generator", "surfaces", World.Name,
               "done");

         end;
      end if;

      declare
         System    : constant Accord.Star_System.Star_System_Class :=
                       World.Star_System;
      begin
         Concorde.Configure.Resources.Create_Deposits
           (World     => World,
            Generator =>
              Concorde.Configure.Resources.Create_Generator
                (System.X, System.Y, System.Z));
      end;

   end Generate_Surface;

   -------------------------
   -- Get_Climate_Terrain --
   -------------------------

   procedure Get_Climate_Terrain
     (World  : Accord.World.World_Class;
      Vector : out Climate_Terrain_Vectors.Vector)
   is
      Water_Last : constant Natural := World.Sea_Level;
      Next       : Positive := Water_Last + 1;
   begin

      if Water_Last > 0 then
         Vector.Append
           (Climate_Terrain_Record'
              (Terrain   => Concorde.Terrain.Ocean,
               First     => 1,
               Last      => Water_Last,
               Frequency => World.Hydrosphere,
               Count     => Water_Last));
      end if;

      for Climate_Terrain of
        Accord.Climate_Terrain.Select_By_Climate
          (World.Climate)
      loop

         declare
            Hydrosphere  : constant Unit_Real := World.Hydrosphere;
            Terrain_Freq : constant Unit_Real := Climate_Terrain.Frequency;
            Frequency    : constant Unit_Real :=
                             Terrain_Freq * (1.0 - Hydrosphere);
            Count     : constant Natural :=
              Natural (Frequency * Real (World.Elevation_Range));
         begin
            Vector.Append (Climate_Terrain_Record'
                             (Terrain   =>
                                Climate_Terrain.Terrain.To_Terrain_Handle,
                              First     => Next,
                              Last      => Next + Count - 1,
                              Frequency => Frequency,
                              Count     => Count));
            Next := Next + Count;
         end;
      end loop;

      while Next < World.Elevation_Range loop
         declare
            Total_Move : Natural := 0;
            Remaining  : constant Natural :=
              World.Elevation_Range - Next;
         begin
            for Item of Vector loop
               declare
                  This_Move : constant Positive :=
                    Natural'Min
                      (Natural'Max (Remaining / Vector.Last_Index, 1),
                       Remaining - Total_Move);
               begin
                  Item.First := Item.First + This_Move;
                  Item.Count := Item.Count + This_Move;
                  Item.Last := Item.First + Item.Count - 1;
                  Total_Move := Total_Move + This_Move;
                  exit when Total_Move = Remaining;
               end;
            end loop;
            Next := Next + Total_Move;
         end;
      end loop;

   end Get_Climate_Terrain;

   ---------------------
   -- Get_Frequencies --
   ---------------------

   function Get_Frequencies
     (World : Accord.World.World_Class)
      return Heights.Frequency_Map
   is
   begin
      return Freq : constant Heights.Frequency_Map
        (1 .. World.Elevation_Range) := (others => 1);
   end Get_Frequencies;

   ---------------------------
   -- Load_Terrain_Checkers --
   ---------------------------

   procedure Load_Terrain_Checkers is

      procedure Add (Tag : String;
                     Checker : Terrain_Checker);

      ---------
      -- Add --
      ---------

      procedure Add (Tag : String;
                     Checker : Terrain_Checker)
      is
      begin
         Terrain_Checkers.Append
           ((Accord.Terrain.Get_By_Tag (Tag), Checker));
         pragma Assert (Terrain_Checkers.Last_Element.Terrain.Has_Element);
      end Add;

   begin
      Add ("water", Check_Water'Access);
      Add ("cold-desert", Check_Cold_Desert'Access);
      Add ("desert", Check_Desert'Access);
      Add ("jungle", Check_Jungle'Access);
      Add ("marsh", Check_Marsh'Access);
      Add ("forest", Check_Forest'Access);
      Add ("steppe", Check_Steppe'Access);
      Add ("savannah", Check_Savannah'Access);
      Add ("tundra", Check_Tundra'Access);
      Add ("hill", Check_Hill'Access);
      Add ("plain", Check_Plain'Access);
   end Load_Terrain_Checkers;

   ------------------
   -- Save_Surface --
   ------------------

   procedure Save_Surface
     (Surface   : Concorde.Surfaces.Surface_Type;
      World     : Accord.World.World_Class)
   is
      Freqs : constant Heights.Frequency_Map :=
                Get_Frequencies (World);
      Hs    : Heights.Height_Array (1 .. Natural (Surface.Tile_Count));

      Tile_Refs : array (1 .. Surface.Tile_Count)
        of Accord.Sector.Sector_Handle;

      type Sector_Info is
         record
            Tile         : Concorde.Surfaces.Surface_Tile_Index;
            Centre       : Concorde.Surfaces.Vector_3;
            Elevation    : Integer;
            Ave_Temp     : Non_Negative_Real;
            Wind         : Real;
            Wind_To      : Concorde.Surfaces.Surface_Tile_Index;
            Moisture     : Unit_Real;
            Habitability : Unit_Real;
            Toxicity     : Unit_Real;
         end record;

      Sector_Array : array (1 .. Surface.Tile_Count) of Sector_Info;

      Axial_Tilt : constant Real :=
                     Concorde.Trigonometry.To_Degrees (World.Tilt);

      function Base_Temperature
        (Tile : Surfaces.Surface_Tile_Index)
         return Non_Negative_Real;

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array;

      function Prevailing_Wind
        (Latitude : Real)
         return Real;

      procedure Iterate_Moisture;

      ----------------------
      -- Base_Temperature --
      ----------------------

      function Base_Temperature
        (Tile : Surfaces.Surface_Tile_Index)
         return Non_Negative_Real
      is
         Y : constant Real := Surface.Tile_Centre (Tile) (3);
      begin
         return World.Average_Temperature
           + (0.5 - abs Y) * 10.0;
      end Base_Temperature;

      --------------------
      -- Get_Neighbours --
      --------------------

      function Get_Neighbours
        (Index : Positive)
         return Heights.Neighbour_Array
      is
         Tile : constant Surfaces.Surface_Tile_Index :=
                  Surfaces.Surface_Tile_Index (Index);
      begin
         return Ns : Heights.Neighbour_Array
           (1 .. Natural (Surface.Neighbour_Count (Tile)))
         do
            for I in Ns'Range loop
               Ns (I) :=
                 Positive
                   (Surface.Neighbour
                      (Tile,
                       Surfaces.Tile_Neighbour_Index (I)));
            end loop;
         end return;
      end Get_Neighbours;

      ----------------------
      -- Iterate_Moisture --
      ----------------------

      procedure Iterate_Moisture is
         New_Moisture : array (Sector_Array'Range) of Non_Negative_Real :=
                          (others => 0.0);

         procedure Update
           (Info   : Sector_Info;
            Tile   : Concorde.Surfaces.Surface_Tile_Index;
            Factor : Unit_Real);

         ------------
         -- Update --
         ------------

         procedure Update
           (Info   : Sector_Info;
            Tile   : Concorde.Surfaces.Surface_Tile_Index;
            Factor : Unit_Real)
         is
            M           : Non_Negative_Real renames New_Moisture (Tile);
            Delta_E     : constant Integer :=
                            Sector_Array (Tile).Elevation
                            - Info.Elevation;
            Attenuation : constant Unit_Real :=
                            Unit_Clamp
                              (1.0
                               - Real (Integer'Max (Delta_E, 0)) / 200.0)
                            * Factor;
            Cold_Factor : constant Unit_Real :=
                            (if Info.Ave_Temp < 275.0
                             then 0.1
                             else Unit_Clamp
                               ((Info.Ave_Temp - 275.0) / 10.0 + 0.1));
         begin
            M := M + Info.Moisture * Attenuation * Cold_Factor;
         end Update;

      begin
         for Info of Sector_Array loop

            Update (Info, Info.Wind_To, 0.9);

            for I in 1 .. Surface.Neighbour_Count (Info.Tile) loop
               declare
                  use type Concorde.Surfaces.Surface_Tile_Index;
                  N : constant Concorde.Surfaces.Surface_Tile_Index :=
                        Surface.Neighbour (Info.Tile, I);
               begin
                  if N /= Info.Wind_To
                    and then Surface.Is_Neighbour (Info.Tile, N)
                  then
                     Update (Info, N, 0.2);
                  end if;
               end;
            end loop;

         end loop;

         for I in Sector_Array'Range loop
            declare
               Info     : Sector_Info renames Sector_Array (I);
               Ave_Temp : constant Non_Negative_Real := Info.Ave_Temp;
            begin
               Info.Moisture :=
                 (if Info.Elevation < 0
                  then (if Ave_Temp < 263.0
                    then 0.0
                    elsif Ave_Temp < 273.0
                    then (Ave_Temp - 263.0) / 10.0
                    else 1.0)
                  else Unit_Clamp (New_Moisture (I)));
            end;
         end loop;
      end Iterate_Moisture;

      ---------------------
      -- Prevailing_Wind --
      ---------------------

      function Prevailing_Wind
        (Latitude : Real)
         return Real
      is
      begin
         if Latitude > 90.0 - Axial_Tilt then
            return -135.0;
         elsif Latitude > Axial_Tilt then
            return 45.0;
         elsif Latitude > 0.0 then
            return -135.0;
         elsif Latitude > -Axial_Tilt then
            return 135.0;
         elsif Latitude > -90.0 + Axial_Tilt then
            return -45.0;
         else
            return 135.0;
         end if;
      end Prevailing_Wind;

   begin

      if Terrain_Checkers.Is_Empty then
         Load_Terrain_Checkers;
      end if;

      WL.Random.Reset (World.Seed);

      Heights.Generate_Height_Map
        (Heights     => Hs,
         Frequencies => Freqs,
         Smoothing   => 3,
         Neighbours  => Get_Neighbours'Access);

      for I in Sector_Array'Range loop
         declare
            Centre : constant Concorde.Surfaces.Vector_3 :=
                       Surface.Tile_Centre (I);
            Latitude : constant Real :=
                         Concorde.Elementary_Functions.Arcsin
                           (Centre (3), 360.0);
            Elevation : constant Integer :=
                          Hs (Positive (I)) - World.Sea_Level;
            Wind      : constant Real := Prevailing_Wind (Latitude);
            Ave_Temp  : constant Non_Negative_Real :=
                          Base_Temperature (I)
                          - (if Elevation <= 0 then 0.0
                             else 6.5 * Real (Elevation) / 100.0);
            Moisture  : constant Unit_Real :=
                          (if Elevation < 0
                           then (if Ave_Temp < 263.0
                             then 0.0
                             elsif Ave_Temp < 273.0
                             then (Ave_Temp - 263.0) / 10.0
                             else 1.0)
                           else 0.0);
         begin
            Sector_Array (I) := Sector_Info'
              (Tile         => I,
               Centre       => Centre,
               Elevation    => Elevation,
               Ave_Temp     => Ave_Temp,
               Wind         => Wind,
               Wind_To      => Concorde.Surfaces.Neighbour
                 (Surface => Surface,
                  Tile    => I,
                  Bearing => Wind),
               Moisture     => Moisture,
               Habitability => World.Habitability,
               Toxicity     => 0.0);
         end;
      end loop;

      for I in 1 .. Sector_Array'Length / 20 loop
         Iterate_Moisture;
      end loop;

      for I in Tile_Refs'Range loop
         declare
            Centre : constant Concorde.Surfaces.Vector_3 :=
                       Sector_Array (I).Centre;
            Sector               : constant Accord.World_Sector
              .World_Sector_Handle :=
                Accord.World_Sector.Create
                  (Identifier          => Concorde.Identifiers.Next_Identifier,
                   Surface             => World,
                   X                   => Centre (1),
                   Y                   => Centre (2),
                   Z                   => Centre (3),
                   Faction             =>
                     Accord.Faction.Empty_Handle,
                   World               => World,
                   Terrain             => Accord.Terrain.Empty_Handle,
                   Feature             =>
                     Accord.Feature.Empty_Handle,
                   Height              => Hs (Positive (I)),
                   Elevation           => Sector_Array (I).Elevation,
                   Sector_Use          =>
                     Accord.Sector_Use.Empty_Handle,
                   Average_Temperature => Sector_Array (I).Ave_Temp,
                   Habitability        => Sector_Array (I).Habitability,
                   Prevailing_Wind     => Sector_Array (I).Wind,
                   Moisture            => Sector_Array (I).Moisture);
         begin
            Tile_Refs (I) := Sector.To_Sector_Handle;
            for V of Surface.Tile_Boundary (I) loop
               Accord.Sector_Vertex.Create
                 (Sector => Tile_Refs (I),
                  X      => V (1),
                  Y      => V (2),
                  Z      => V (3));
            end loop;
         end;
      end loop;

      for Tile_Index in 1 .. Surface.Tile_Count loop
         for I in 1 .. Surface.Neighbour_Count (Tile_Index) loop
            Accord.Sector_Neighbour.Create
              (Sector    => Tile_Refs (Tile_Index),
               Neighbour => Tile_Refs (Surface.Neighbour (Tile_Index, I)));
         end loop;
      end loop;

      for Sector of
        Accord.World_Sector.Select_By_World (World)
      loop
         for Element of Terrain_Checkers loop
            if Element.Checker (Sector) then
               declare
                  use Accord.World_Sector;
                  Start_Update : constant World_Sector_Update_Handle'Class :=
                                   Sector.Update_World_Sector;
                  End_Update   : constant World_Sector_Update_Handle'Class :=
                                   Start_Update.Set_Terrain (Element.Terrain);
               begin
                  End_Update.Done;
                  exit;
               end;
            end if;
         end loop;
      end loop;

      declare

         package Sector_Lists is
           new Ada.Containers.Doubly_Linked_Lists
             (Accord.World_Sector.World_Sector_Handle,
              Accord.World_Sector."=");

         function Effective_Temp
           (Sector : Accord.World_Sector.World_Sector_Handle)
            return Non_Negative_Real
         is (if Sector.Terrain.Is_Water
             then Sector.Average_Temperature + 10.0
             else Sector.Average_Temperature);

         function Colder
           (Left, Right : Accord.World_Sector.World_Sector_Handle)
            return Boolean
         is (Effective_Temp (Left) < Effective_Temp (Right));

         package Sector_Sorting is
           new Sector_Lists.Generic_Sorting (Colder);

         Cold_Sectors : Sector_Lists.List;

      begin
         for World_Sector of
           Accord.World_Sector
             .Select_World_Temperature_Bounded_By_Average_Temperature
               (World, 0.0, Concorde.Constants.Freezing_Point_Of_Water)
         loop
            Cold_Sectors.Append (World_Sector.To_World_Sector_Handle);
         end loop;

         Sector_Sorting.Sort (Cold_Sectors);

         declare
            Max_Ice_Sectors  : constant Natural :=
                                 (if World.Average_Temperature < -20.0
                                  then Natural (Surface.Tile_Count)
                                  else Natural (Real (Surface.Tile_Count)
                                    * World.Hydrosphere));
            Ice_Sector_Count : Natural := 0;
            Ice_Feature      : constant Accord.Feature.Feature_Handle
              := Accord.Feature.Get_By_Tag ("ice");
         begin
            for Ref of Cold_Sectors loop
               exit when Ice_Sector_Count >= Max_Ice_Sectors;

               Accord.World_Sector.Update_World_Sector (Ref)
                 .Set_Feature (Ice_Feature)
                 .Done;
               Ice_Sector_Count := Ice_Sector_Count + 1;
            end loop;
         end;
      end;

   end Save_Surface;

end Concorde.Configure.Worlds;
