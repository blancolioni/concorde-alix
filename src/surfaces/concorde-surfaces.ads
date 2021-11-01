private with Ada.Containers.Vectors;

with Ada.Numerics.Generic_Real_Arrays;

with Concorde.Trigonometry;

package Concorde.Surfaces is

   Max_Surface_Tiles : constant := 60_000;

   Max_Tile_Neighbours : constant := 20;

   type Surface_Tile_Count is range 0 .. Max_Surface_Tiles;
   subtype Surface_Tile_Index is
     Surface_Tile_Count range 1 .. Surface_Tile_Count'Last;

   type Tile_Neighbour_Count is range 0 .. Max_Tile_Neighbours;

   subtype Tile_Neighbour_Index is
     Tile_Neighbour_Count range 1 .. Tile_Neighbour_Count'Last;

   type Array_Of_Tile_Neighbours is
     array (Tile_Neighbour_Index range <>) of Surface_Tile_Index;

   type Root_Surface_Type is tagged private;

   function Tile_Count
     (Surface : Root_Surface_Type'Class)
      return Surface_Tile_Count;

   function Neighbour_Count
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Neighbour_Count
     with Pre => Tile <= Surface.Tile_Count;

   function Neighbour
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index;
      Index   : Tile_Neighbour_Index)
      return Surface_Tile_Index
     with Pre => Tile <= Surface.Tile_Count
     and then Index <= Surface.Neighbour_Count (Tile);

   function Neighbour
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index;
      Bearing : Real)
      return Surface_Tile_Index;

   function Is_Neighbour
     (Surface   : Root_Surface_Type'Class;
      Tile      : Surface_Tile_Index;
      Neighbour : Surface_Tile_Index)
      return Boolean;

   function Get_Tile
     (Relative_Latitude  : Signed_Unit_Real;
      Relative_Longitude : Unit_Real)
      return Surface_Tile_Index;

   function Get_Bearing
     (Surface  : Root_Surface_Type'Class;
      From, To : Surface_Tile_Index)
      return Concorde.Trigonometry.Angle;

   function Latitude
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Real;

   function Longitude
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Real;

   package Real_Arrays is
     new Ada.Numerics.Generic_Real_Arrays (Real);

   subtype Vector_3 is Real_Arrays.Real_Vector (1 .. 3);

   type Tile_Vertex_Array is
     array (Tile_Neighbour_Index range <>) of Vector_3;

   function Tile_Centre
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Vector_3;

   function Tile_Boundary
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Vertex_Array;

   procedure Create_Voronoi_Partition
     (Surface : in out Root_Surface_Type'Class;
      Count   : Natural);

   subtype Surface_Type is Root_Surface_Type'Class;

private

   package Neighbour_Vectors is
     new Ada.Containers.Vectors (Tile_Neighbour_Index, Surface_Tile_Index);

   type Tile_Vertex_Index is new Surface_Tile_Index;

   package Tile_Vertex_Vectors is
     new Ada.Containers.Vectors (Tile_Neighbour_Index, Vector_3,
                                 Real_Arrays."=");

   type Tile_Record is
      record
         Position    : Vector_3;
         Vertices    : Tile_Vertex_Vectors.Vector;
         Neighbours  : Neighbour_Vectors.Vector;
      end record;

   package Tile_Vectors is
     new Ada.Containers.Vectors (Surface_Tile_Index, Tile_Record);

   package Vertex_Vectors is
     new Ada.Containers.Vectors (Positive, Vector_3, Real_Arrays."=");

   type Root_Surface_Type is tagged
      record
         Vertices      : Vertex_Vectors.Vector;
         Tiles         : Tile_Vectors.Vector;
      end record;

   function Tile_Count
     (Surface : Root_Surface_Type'Class)
      return Surface_Tile_Count
   is (Surface.Tiles.Last_Index);

   function Neighbour_Count
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index)
      return Tile_Neighbour_Count
   is (Surface.Tiles (Tile).Neighbours.Last_Index);

   function Neighbour
     (Surface : Root_Surface_Type'Class;
      Tile    : Surface_Tile_Index;
      Index   : Tile_Neighbour_Index)
      return Surface_Tile_Index
   is (Surface.Tiles (Tile).Neighbours (Index));

end Concorde.Surfaces;
