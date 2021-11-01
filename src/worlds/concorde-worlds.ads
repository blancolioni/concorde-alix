private with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Quantities;
with Concorde.Spheres;
with Concorde.Trigonometry;

with Accord.Faction;
with Accord.Resource;
with Accord.Terrain;
with Accord.World;
with Accord.World_Sector;

package Concorde.Worlds is

   subtype World_Class is Accord.World.World_Class;
   subtype World_Handle is Accord.World.World_Handle;

   type World_Selection is tagged private;

   function Is_Empty (Selection : World_Selection'Class) return Boolean;

   procedure Clear (Selection : in out World_Selection'Class);
   procedure Insert
     (Selection : in out World_Selection'Class;
      World     : World_Class);

   procedure Filter
     (Selection : in out World_Selection'Class;
      Test      : not null access
        function (World : World_Class)
      return Boolean);

   type World_Array is
     array (Positive range <>) of World_Handle;

   function Get_Worlds (Selection : World_Selection'Class) return World_Array;

   function Is_Terrestrial
     (World : World_Class)
      return Boolean;

   type World_Sector_Array is
     array (Positive range <>)
     of Accord.World_Sector.World_Sector_Handle;

   function Get_Neighbours
     (Sector : Accord.World_Sector.World_Sector_Class)
      return World_Sector_Array;

   function Get_Bearing
     (From, To : Accord.World_Sector.World_Sector_Class)
      return Concorde.Trigonometry.Angle;

   function Get_Distance
     (From, To : Accord.World_Sector.World_Sector_Class)
      return Non_Negative_Real;

   procedure Circular_Scan
     (Start : Accord.World_Sector.World_Sector_Class;
      Process : not null access
        function (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean);

   --  function should return False if scanning is to stop

   procedure Scan_Surface
     (World : World_Class;
      Process : not null access
        procedure (Sector : Accord.World_Sector.World_Sector_Class));

   function Best_Sector
     (World : World_Class;
      Score : not null access
        function (Sector : Accord.World_Sector.World_Sector_Class)
      return Real)
      return Accord.World_Sector.World_Sector_Class;

   function Find_Sector
     (World : World_Class;
      Test : not null access
        function (Sector : Accord.World_Sector.World_Sector_Class)
      return Boolean)
      return Accord.World_Sector.World_Sector_Class;

   function Get_World
     (Sector : Accord.World_Sector.World_Sector_Class)
      return World_Class;

   function Get_Owner
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Accord.Faction.Faction_Class;

   procedure Set_Owner
     (Sector  : Accord.World_Sector.World_Sector_Class;
      Faction : Accord.Faction.Faction_Handle);

   procedure Scan_Resources
     (Sector  : Accord.World_Sector.World_Sector_Class;
      Process : not null access
        procedure (Resource : Accord.Resource.Resource_Class;
                   Concentration : Unit_Real;
                   Difficulty    : Unit_Real;
                   Available     : Concorde.Quantities.Quantity_Type));

   subtype Sector_Vertex is Concorde.Spheres.Surface_Point;

   type Sector_Position is
      record
         Latitude  : Real;
         Longitude : Real;
      end record;

   function Get_Centre
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Sector_Vertex;

   function Get_Centre
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Sector_Position;

   function Get_Terrain
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Accord.Terrain.Terrain_Class;

   type Sector_Vertex_Array is array (Positive range <>) of Sector_Vertex;

   function Get_Vertices
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Sector_Vertex_Array;

private

   package World_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (World_Handle, Accord.World."=");

   type World_Selection is tagged
      record
         List : World_Lists.List;
      end record;

end Concorde.Worlds;
