with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with Ada.Numerics;

with WL.String_Maps;

with Nazar.Colors;

with Concorde.Elementary_Functions;
--  with Concorde.Logging;
with Concorde.Real_Images;
with Concorde.Trigonometry;

with Concorde.Worlds;

with Accord.Feature;
with Accord.Has_Color;

with Accord.Colony;
with Accord.World_Sector;

package body Concorde.UI.Models.World is

   Sqrt_2 : constant := 1.4142135623730950488016887242;

   package Vertex_Holders is
     new Ada.Containers.Indefinite_Holders
       (Concorde.Worlds.Sector_Vertex_Array, Concorde.Worlds."=");

   type Map_Point is
      record
         X, Y : Nazar.Nazar_Float;
      end record;

   type Sector_Model is
      record
         Owner      : Accord.Faction.Faction_Handle;
         Sector     : Accord.World_Sector.World_Sector_Handle;
         Centre     : Concorde.Worlds.Sector_Vertex;
         Map_Centre : Map_Point;
         Boundary   : Vertex_Holders.Holder;
         Color      : Nazar.Colors.Nazar_Color;
         Wind       : Real;
         Wind_To    : Natural;
      end record;

   package Sector_Model_Vectors is
      new Ada.Containers.Vectors (Positive, Sector_Model);

   package Sector_Reference_Maps is
     new WL.String_Maps (Positive);

   type World_Model_Type is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Sectors    : Sector_Model_Vectors.Vector;
         Ref_Map    : Sector_Reference_Maps.Map;
         Display    : Display_Type;
         Show_Owner : Boolean;
         Show_Wind  : Boolean;
      end record;

   function Project
     (Latitude   : Real;
      Longitude  : Real;
      Positive_X : Boolean)
      return Map_Point;

   procedure Draw_World
     (Model  : in out World_Model_Type'Class;
      Centre : Concorde.Worlds.Sector_Vertex);

   function To_Nazar_Color
     (Has_Color : Accord.Has_Color.Has_Color_Class)
      return Nazar.Colors.Nazar_Color
   is (Red   => Nazar.Nazar_Unit_Float (Has_Color.Red),
       Green => Nazar.Nazar_Unit_Float (Has_Color.Green),
       Blue  => Nazar.Nazar_Unit_Float (Has_Color.Blue),
       Alpha => 1.0);

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image
     with Unreferenced;

   ----------------
   -- Draw_World --
   ----------------

   procedure Draw_World
     (Model  : in out World_Model_Type'Class;
      Centre : Concorde.Worlds.Sector_Vertex)
   is
      use Ada.Numerics;
      use Concorde.Elementary_Functions;
      Long_0 : constant Real :=
                 Arctan (Centre.Y, Centre.X);
   begin

      --  Concorde.Logging.Log
      --    ("model", "world", "draw",
      --     "long-0 = "
      --     & Image (180.0 * Long_0 / Pi));

      for Fill in reverse Boolean loop

         Model.Set_Fill (Fill);

         if not Fill then
            Model.Set_Color ((0.75, 0.75, 0.75, 1.0));
         end if;

         for Sector of Model.Sectors loop
            declare
               Color       : constant Nazar.Colors.Nazar_Color :=
                               Sector.Color;
               Boundary    : constant Concorde.Worlds.Sector_Vertex_Array :=
                               Sector.Boundary.Element;
               Centre_Lat  : constant Real := Arcsin (Sector.Centre.Z);
               Centre_Long : Real :=
                               Arctan (Sector.Centre.Y, Sector.Centre.X)
                               - Long_0;
               First       : Boolean := True;
               First_Point : Map_Point;
            begin
               if Centre_Long < -Pi then
                  Centre_Long := Centre_Long + 2.0 * Pi;
               elsif Centre_Long > Pi then
                  Centre_Long := Centre_Long - 2.0 * Pi;
               end if;

               if Fill then
                  Model.Set_Color (Color);
                  Sector.Map_Centre :=
                    Project (Centre_Lat, Centre_Long, Centre_Long > 0.0);
               end if;

               for Pt of Boundary loop
                  declare
                     Long    : constant Real :=
                                 Arctan (Pt.Y, Pt.X) - Long_0;
                     Lat     : constant Real := Arcsin (Pt.Z);
                     Map_Pt  : constant Map_Point :=
                                 Project (Lat, Long, Centre_Long > 0.0);
                  begin
                     if First then
                        Model.Move_To (Map_Pt.X, Map_Pt.Y);
                        First := False;
                        First_Point := Map_Pt;
                     else
                        Model.Line_To (Map_Pt.X, Map_Pt.Y);
                     end if;
                  end;
               end loop;
               Model.Line_To (First_Point.X, First_Point.Y);
               Model.Render;
            end;
         end loop;
      end loop;

      if Model.Show_Wind then
         Model.Set_Color (0.0, 0.0, 0.0, 1.0);
         for Sector of Model.Sectors loop
            declare
               use Nazar;
               From : constant Map_Point :=
                        (Sector.Map_Centre.X, Sector.Map_Centre.Y);
               To_S : constant Sector_Model :=
                        Model.Sectors.Element (Sector.Wind_To);
               To   : constant Map_Point :=
                        (From.X + (To_S.Map_Centre.X - From.X) / 2.0,
                         From.Y + (To_S.Map_Centre.Y - From.Y) / 2.0);
               DX   : constant Nazar_Float := To.X - From.X;
               DY   : constant Nazar_Float := To.Y - From.Y;
               D    : constant Non_Negative_Real :=
                        Sqrt (Real (DX) ** 2 + Real (DY) ** 2);
               N    : constant Map_Point :=
                        (DX / Nazar_Float (D), DY / Nazar_Float (D));
               AX   : constant Nazar_Float := 0.025 * (-N.Y - N.X);
               AY   : constant Nazar_Float := 0.025 * (N.X - N.Y);
            begin
               Model.Move_To (From.X, From.Y);
               Model.Line_To (To.X, To.Y);
               Model.Line_To (To.X + AX, To.Y + AY);
               Model.Move_To (To.X, To.Y);
               Model.Line_To (To.X - AY, To.Y + AX);

               Model.Render;
            end;
         end loop;
      end if;

   end Draw_World;

   -------------
   -- Project --
   -------------

   function Project
     (Latitude   : Real;
      Longitude  : Real;
      Positive_X : Boolean)
      return Map_Point
   is
      use Nazar;
      use Concorde.Elementary_Functions;

      Pi : constant := Ada.Numerics.Pi;
      Theta : Real := Latitude;
      Sin_Lat : constant Real := Sin (Latitude);
      Pi_Sin_Lat : constant Real := Pi * Sin_Lat;
      Abs_Lat    : constant Nazar_Float :=
                     Nazar_Float (abs Latitude);
      K : constant := 2.0 * Sqrt_2 / Pi;

   begin
      if Theta > -2.0 * Pi and then Theta < 2.0 * Pi then
         loop
            declare
               Prev : constant Real := Theta;
            begin
               Theta :=
                 Theta -
                   (2.0 * Theta + Sin (2.0 * Theta) - Pi_Sin_Lat)
                     / (2.0 + 2.0 * Cos (2.0 * Theta));

               exit when abs (Prev - Theta) < 1.0e-9;
            end;
         end loop;
      end if;

      declare
         Cos_Theta : constant Nazar_Float :=
                       Nazar_Float (Cos (Theta));
         X         : Nazar_Float :=
                       Nazar_Float
                         (K * Longitude) * Cos_Theta;
         Y         : constant Nazar_Float :=
                       Nazar_Float (Sqrt_2 * Sin (Theta));

      begin
         if Positive_X and then X < -Pi / 2.0 + Abs_Lat then
            X := X + 4.0 * Sqrt_2 * Cos_Theta;
         elsif not Positive_X and then X > Pi / 2.0 - Abs_Lat then
            X := X - 4.0 * Sqrt_2 * Cos_Theta;
         end if;

         return (X, Y);
      end;

   end Project;

   -----------------
   -- World_Model --
   -----------------

   function World_Model
     (Faction    : Accord.Faction.Faction_Handle;
      World      : Accord.World.World_Class;
      Display    : Display_Type;
      Show_Owner : Boolean;
      Show_Wind  : Boolean)
      return Nazar.Models.Draw.Nazar_Draw_Model
   is
      pragma Unreferenced (Faction);
      Result : World_Model_Type;

      function Get_Color
        (Sector : Accord.World_Sector.World_Sector_Class)
         return Nazar.Colors.Nazar_Color;

      ---------------
      -- Get_Color --
      ---------------

      function Get_Color
        (Sector : Accord.World_Sector.World_Sector_Class)
         return Nazar.Colors.Nazar_Color
      is
      begin
         if Show_Owner and then Sector.Faction.Has_Element then
            return To_Nazar_Color (Sector.Faction);
         end if;

         case Display is
            when Terrain_Display =>
               if Sector.Feature.Has_Element then
                  return To_Nazar_Color (Sector.Feature);
               else
                  return To_Nazar_Color (Sector.Terrain);
               end if;
            when Elevation_Display =>
               if Sector.Elevation < 0 then
                  return (0.1, 0.1, 0.5, 1.0);
               else
                  declare
                     use Nazar;
                     E : constant Nazar_Unit_Float :=
                           Nazar_Unit_Float
                             (Unit_Clamp
                                (Real (Sector.Elevation) / 60.0 + 0.2));
                  begin
                     return (E, E, E, 1.0);
                  end;
               end if;
            when Moisture_Display =>
               if Sector.Elevation < 0 then
                  return (0.1, 0.1, 0.5, 1.0);
               else
                  declare
                     use Nazar;
                     E : constant Nazar_Unit_Float :=
                           Nazar_Unit_Float (Sector.Moisture);
                  begin
                     return (E, E, E, 1.0);
                  end;
               end if;
            when Temperature_Display =>
               if Sector.Average_Temperature < 270.0 then
                  return (1.0, 1.0, 1.0, 1.0);
               elsif Sector.Average_Temperature > 320.0 then
                  return (1.0, 0.0, 0.0, 1.0);
               else
                  declare
                     use Nazar;
                     T : constant Nazar_Unit_Float :=
                           Nazar_Unit_Float
                             ((Sector.Average_Temperature - 270.0)
                              / 50.0);
                  begin
                     return (1.0, 1.0 - T, 1.0 - T, 1.0);
                  end;
               end if;
         end case;
      end Get_Color;

   begin

      Result.Show_Wind := Show_Wind;
      Result.Show_Owner := Show_Owner;
      Result.Display := Display;

      for Sector of
        Accord.World_Sector.Select_By_World (World)
      loop
         declare
            Color   : constant Nazar.Colors.Nazar_Color :=
                        Get_Color (Sector);
         begin
            Result.Sectors.Append
              (Sector_Model'
                 (Sector     => Sector.To_World_Sector_Handle,
                  Owner      => Sector.Faction.To_Faction_Handle,
                  Centre     => Concorde.Worlds.Get_Centre (Sector),
                  Map_Centre => (0.0, 0.0),
                  Boundary   =>
                    Vertex_Holders.To_Holder
                      (Concorde.Worlds.Get_Vertices (Sector)),
                  Color      => Color,
                  Wind       => Sector.Prevailing_Wind,
                  Wind_To    => 0));
            Result.Ref_Map.Insert
              (Sector.Identifier, Result.Sectors.Last_Index);
         end;
      end loop;

      if Show_Wind then
         for Sector of Result.Sectors loop
            declare
               use Concorde.Trigonometry;
               Wind    : constant Angle := From_Degrees (Sector.Wind);
               To      : Accord.World_Sector.World_Sector_Handle :=
                           Accord.World_Sector.Empty_Handle;
               Closest : Angle := From_Radians (0.0);
               First   : Boolean := True;
            begin

               for Neighbour of
                 Concorde.Worlds.Get_Neighbours (Sector.Sector)
               loop
                  declare
                     Bearing : constant Angle :=
                                 Concorde.Worlds.Get_Bearing
                                   (Sector.Sector, Neighbour);
                  begin
                     if First or else Wind - Bearing < Closest then
                        Closest := Bearing;
                        To := Neighbour;
                        First := False;
                     end if;
                  end;
               end loop;
               Sector.Wind_To :=
                 Result.Ref_Map.Element (To.Identifier);
            end;
         end loop;
      end if;

      declare
         Colony : constant Accord.Colony.Colony_Class :=
                    Accord.Colony.First_By_World (World);
      begin
         Result.Draw_World
           (Centre =>
              Concorde.Worlds.Get_Centre (Colony.Capital));
      end;

      declare
         use Nazar;
      begin
         Result.Set_Bounding_Box
           (Box => (-3.0, -2.0, 6.0, 4.0));
      end;
      return new World_Model_Type'(Result);
   end World_Model;

end Concorde.UI.Models.World;
