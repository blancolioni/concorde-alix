with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Long_Float_Text_IO;
with Ada.Text_IO;

with Ada.Numerics.Long_Elementary_Functions;

with WL.String_Sets;

with Concorde.Calendar;
with Concorde.Elementary_Functions;
with Concorde.Identifiers;
with Concorde.Random;
with Concorde.Real_Images;
with Concorde.Trigonometry;

with Concorde.Solar_System;
with Concorde.Stars;
with Concorde.Stars.Tables;

with Concorde.Configure.Resources;

with Accord.Massive_Object;
with Accord.Scenario;
with Accord.Star_System;
with Accord.Star_System_Distance;
with Accord.Star;

with Accord.Db;

package body Concorde.Configure.Galaxies is

   function Random_Star_Mass return Non_Negative_Real;

   procedure Calculate_System_Distances with Unreferenced;

   function Right_Ascension_To_Radians
     (Hours, Minutes, Seconds : Real)
      return Real
     with Unreferenced;

   function Declination_To_Radians
     (Degrees, Minutes, Seconds : Real)
      return Real
     with Unreferenced;

   procedure String_To_Real_3
     (Text    : String;
      X, Y, Z : out Real)
     with Unreferenced;

   --------------------------------
   -- Calculate_System_Distances --
   --------------------------------

   procedure Calculate_System_Distances is
      use type Accord.Star_System.Star_System_Class;
   begin
      Ada.Text_IO.Put_Line ("Calculating distances between star systems ...");

      for From of Accord.Star_System.Scan_By_Top_Record loop
         for To of Accord.Star_System.Scan_By_Top_Record loop
            if From /= To then
               declare
                  use Concorde.Elementary_Functions;
               begin
                  Accord.Star_System_Distance.Create
                    (Star_System => From,
                     To          => To,
                     Distance    =>
                       Sqrt ((To.X - From.X) ** 2 +
                         (To.Y - From.Y) ** 2 +
                         (To.Z - From.Z) ** 2));
               end;
            end if;
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("done");
   end Calculate_System_Distances;

   ----------------------------
   -- Declination_To_Radians --
   ----------------------------

   function Declination_To_Radians
     (Degrees, Minutes, Seconds : Real)
      return Real
   is
      Result : constant Real :=
                 (abs Degrees + Minutes / 60.0 + Seconds / 3600.0)
                 * Ada.Numerics.Pi / 180.0;
   begin
      if Degrees < 0.0 then
         return -Result;
      else
         return Result;
      end if;
   end Declination_To_Radians;

   ---------------------
   -- Generate_Galaxy --
   ---------------------

   procedure Generate_Galaxy
     (Number_Of_Systems  : Positive;
      Radius_X           : Non_Negative_Real;
      Radius_Y           : Non_Negative_Real;
      Radius_Z           : Non_Negative_Real;
      Create_Coordinates : Biased_Coordinate_Generator;
      Names              : WL.Random.Names.Name_Generator;
      Progress           : in out Progress_Interface'Class)
   is
      use Concorde.Elementary_Functions;
      use Concorde.Real_Images;

      Stored_Nearest_Count : constant := 24;

      Volume     : constant Non_Negative_Real :=
                     4.0 * Ada.Numerics.Pi / 3.0
                       * Radius_X * Radius_Y * Radius_Z;
      Volume_Per_Star : constant Non_Negative_Real :=
                          Volume
                            / Non_Negative_Real (Number_Of_Systems);
      Radius_Per_Star : constant Non_Negative_Real :=
                          (3.0 * Volume_Per_Star
                           / (4.0 * Ada.Numerics.Pi)) ** (1.0 / 3.0);
      Minimum_Distance : constant Non_Negative_Real :=
                           Radius_Per_Star / 4.0;

      type Star_Distance_Record is
         record
            To       : Positive;
            Distance : Non_Negative_Real;
         end record;

      package Star_Distance_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Star_Distance_Record);

      function Closer (Left, Right : Star_Distance_Record) return Boolean
      is (Left.Distance < Right.Distance);

      package Sorting is
        new Star_Distance_Lists.Generic_Sorting (Closer);

      type Generated_Star_Record is
         record
            X, Y, Z   : Real;
            Reference : Accord.Star_System.Star_System_Handle;
            Nearest   : Star_Distance_Lists.List;
         end record;

      package Generated_Star_Vectors is
        new Ada.Containers.Vectors (Positive, Generated_Star_Record);

      Vector : Generated_Star_Vectors.Vector;

      function Distance
        (From, To : Generated_Star_Record)
         return Non_Negative_Real
      is ((From.X - To.X) ** 2
          + (From.Y - To.Y) ** 2
          + (From.Z - To.Z) ** 2);

      Name_Set : WL.String_Sets.Set;

      function Create_System_Name return String;

      ------------------------
      -- Create_System_Name --
      ------------------------

      function Create_System_Name return String is
      begin
         loop
            declare
               Name : constant String :=
                 WL.Random.Names.Random_Name (Names);
            begin
               if not Name_Set.Contains (Name) then
                  Name_Set.Include (Name);
                  return Name;
               end if;
            end;
         end loop;
      end Create_System_Name;

   begin

      if Volume = 0.0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "At least one of galaxy-radius, galaxy-radius-[xyz]"
            & " must have a value");
         raise Program_Error;
      end if;

      Ada.Text_IO.Put_Line
        ("Galaxy size: ("
         & Approximate_Image (Radius_X)
         & ","
         & Approximate_Image (Radius_Y)
         & ","
         & Approximate_Image (Radius_Z)
         & ")");
      Ada.Text_IO.Put_Line
        ("Volume (ly**3): "
         & Approximate_Image (Volume)
         & " per star: "
         & Approximate_Image (Volume_Per_Star));
      Ada.Text_IO.Put_Line
        ("Average star distance: "
         & Approximate_Image (Radius_Per_Star));
      Ada.Text_IO.Put_Line
        ("Minimum star distance: "
         & Approximate_Image (Minimum_Distance));

      Progress.Add_Work_Item ("generate-coordinates", Number_Of_Systems);
      Progress.Add_Work_Item ("find-distances", Number_Of_Systems);
      Progress.Add_Work_Item ("update-database", Number_Of_Systems);
      Progress.Add_Work_Item ("save-distances", Number_Of_Systems);

      for I in 1 .. Number_Of_Systems loop

         declare
            Rec : Generated_Star_Record;
            OK  : Boolean := False;
            Min : constant Non_Negative_Real := Minimum_Distance ** 2;
         begin
            while not OK loop
               if I = 1 then
                  Rec.X := 0.0;
                  Rec.Y := 0.0;
                  Rec.Z := 0.0;
               else
                  Create_Coordinates (Rec.X, Rec.Y, Rec.Z);
                  Rec.X := Rec.X * Radius_X;
                  Rec.Y := Rec.Y * Radius_Y;
                  Rec.Z := Rec.Z * Radius_Z;
               end if;

               OK := True;
               for J in 1 .. I - 1 loop
                  if Distance (Rec, Vector.Element (J)) < Min then
                     OK := False;
                     exit;
                  end if;
               end loop;
            end loop;

            Vector.Append (Rec);
            Progress.Step;
         end;
      end loop;

      for I in 1 .. Number_Of_Systems loop
         declare
            From : Generated_Star_Record := Vector.Element (I);
         begin
            for J in 1 .. Number_Of_Systems loop
               if I /= J then
                  declare
                     D : constant Non_Negative_Real :=
                           Distance (Vector.Element (I), Vector.Element (J));
                     Length : constant Natural :=
                                Natural (From.Nearest.Length);
                  begin
                     if Length < Stored_Nearest_Count
                       or else D < From.Nearest.Last_Element.Distance
                     then
                        if Length >= Stored_Nearest_Count then
                           From.Nearest.Delete_Last;
                        end if;

                        declare
                           use Star_Distance_Lists;
                           Position : Cursor := From.Nearest.First;
                        begin
                           while Has_Element (Position)
                             and then D >= Element (Position).Distance
                           loop
                              Next (Position);
                           end loop;
                           From.Nearest.Insert (Position, (J, D));
                        end;
                     end if;
                  end;
               end if;
            end loop;

            if False then
               Sorting.Sort (From.Nearest);
            end if;

            Vector.Replace_Element (I, From);
            Progress.Step;
         end;
      end loop;

      for I in 1 .. Number_Of_Systems loop

         declare
            subtype Star_System_Handle is
              Accord.Star_System.Star_System_Handle;
            Gen          : constant Generated_Star_Record :=
                             Vector.Element (I);
            System_Name  : constant String := Create_System_Name;
            Solar_Masses : constant Non_Negative_Real :=
                             Random_Star_Mass;
            Mass         : constant Non_Negative_Real :=
                             Solar_Masses * Concorde.Solar_System.Solar_Mass;
            Star_System  : constant Star_System_Handle :=
                             Accord.Star_System.Create
                               (Name    => System_Name,
                                X       => Gen.X,
                                Y       => Gen.Y,
                                Z       => Gen.Z,
                                Claimed => False);
            Class        : Accord.Db.Spectral_Class;
            Subclass     : Natural;
            Radius       : Non_Negative_Real;
            Luminosity   : Non_Negative_Real;
            R, G, B      : Unit_Real;
         begin

            Vector (I).Reference := Star_System;

            if I = 1 then
               Accord.Scenario.Get_By_Active (True).Update_Scenario
                 .Set_Central_System (Star_System)
                 .Done;
            end if;

            Concorde.Stars.Tables.Get_Main_Sequence_Info
              (Solar_Masses => Solar_Masses,
               Class        => Class,
               Subclass     => Subclass,
               Radius       => Radius,
               Luminosity   => Luminosity,
               R            => R,
               G            => G,
               B            => B);

            declare
               Volume : constant Non_Negative_Real :=
                          4.0 * Ada.Numerics.Pi * (Radius ** 3) / 3.0;
               Temperature : constant Non_Negative_Real :=
                 (Luminosity ** 0.25)
                 * Concorde.Solar_System.Solar_Surface_Temperature;
               Age    : constant Non_Negative_Real :=
                          1.0e10
                            * Solar_Masses
                            / Luminosity;
               Star : constant Accord.Star.Star_Handle :=
                        Accord.Star.Create
                          (Star_System           => Star_System,
                           Primary               =>
                             Accord.Star.Empty_Handle,
                           Mass                  => Mass,
                           Identifier            =>
                             Concorde.Identifiers.Next_Identifier,
                           Radius                => Radius,
                           Density               => Mass / Volume,
                           Primary_Massive       =>
                             Accord.Massive_Object.Empty_Handle,
                           Epoch                 => Concorde.Calendar.Clock,
                           Period                => 0.0,
                           Semimajor_Axis        => 0.0,
                           Eccentricity          => 0.0,
                           Rotation_Period       => 0.0,
                           Tilt                  =>
                             Concorde.Trigonometry.From_Degrees (0.0),
                           Surface_Gravity       => 0.0,
                           Red                   => R,
                           Green                 => G,
                           Blue                  => B,
                           Name                  => System_Name,
                           Class                 => Class,
                           Subclass              => Subclass,
                           Luminosity            => Luminosity,
                           Temperature           => Temperature,
                           Age                   => Age,
                           Ecosphere             =>
                             Concorde.Elementary_Functions.Sqrt
                               (Luminosity));
--                            (Star_System           => Star_System,
--                             Primary               =>
--                        Accord.Null_Star_System_Object_Reference,
--                             Mass                  => Mass,
--                             Radius                => Radius,
--                             Density               => Mass / Volume,
--                             Zero_Longitude        =>
--                               Concorde.Random.Unit_Random * 360.0,
--                             Semimajor_Axis        => 0.0,
--                             Period                => 0.0,
--                             Eccentricity          => 0.0,
--                             Rotation_Period       => 0.0,
--                             Tilt                  => 0.0,
--                             Surface_Acceleration  => 0.0,
--                             Surface_Gravity       => 0.0,
--                             Escape_Velocity       => 0.0,
--                             Surface_Temperature   => 0.0,
--                             Min_Molecular_Weight  => 0.0,
--                             Rms_Velocity          => 0.0,
--                             Resonant_Period       => False,
--                             Red                   => R,
--                             Green                 => G,
--                             Blue                  => B,
--                             Name                  => System_Name,
--                             Class                 => Class,
--                             Subclass              => Subclass,
--                             Luminosity            => Luminosity,
--                             Age                   => Age,
--                             Ecosphere             =>
--                               Concorde.Elementary_Functions.Sqrt
--                                 (Luminosity));
            begin
               if False then
                  Ada.Text_IO.Put (Star.Name);
                  Ada.Text_IO.Set_Col (30);
                  Ada.Text_IO.Put (Concorde.Stars.Spectral_Type (Star));
                  Ada.Text_IO.Set_Col (35);
                  Ada.Long_Float_Text_IO.Put
                    (Concorde.Stars.Solar_Masses (Star),
                     1, 2, 0);
                  Ada.Text_IO.New_Line;
               end if;
               Progress.Step;
            end;
         end;
      end loop;

      for I in 1 .. Number_Of_Systems loop
         declare
            Count : Natural := 0;
         begin
            for Nearest of Vector.Element (I).Nearest loop
               Count := Count + 1;
               exit when Count > Stored_Nearest_Count;

               Accord.Star_System_Distance.Create
                 (Star_System => Vector.Element (I).Reference,
                  To          => Vector.Element (Nearest.To).Reference,
                  Distance    =>
                    Sqrt (Distance
                      (Vector.Element (I),
                           Vector.Element (Nearest.To))));
            end loop;
            Progress.Step;
         end;
      end loop;

      Progress.Finish;

      Concorde.Configure.Resources.Create_Resource_Spheres
        (System_Count => Number_Of_Systems,
         R_X          => Radius_X,
         R_Y          => Radius_Y,
         R_Z          => Radius_Z);

   end Generate_Galaxy;

   --------------------------------------------
   -- Random_Centre_Bias_Sphere_Distribution --
   --------------------------------------------

   procedure Random_Centre_Bias_Sphere_Distribution
     (X, Y, Z : out Signed_Unit_Real)
   is
      use Ada.Numerics.Long_Elementary_Functions;
      D    : constant Unit_Real := Concorde.Random.Unit_Random;
      DX   : constant Signed_Unit_Real :=
               Concorde.Random.Unit_Random * 2.0 - 1.0;
      DY   : constant Signed_Unit_Real :=
               Concorde.Random.Unit_Random * 2.0 - 1.0;
      DZ   : constant Signed_Unit_Real :=
               Concorde.Random.Unit_Random * 2.0 - 1.0;
      Norm : constant Unit_Real :=
               Sqrt (DX ** 2 + DY ** 2 + DZ ** 2);
   begin
      X := DX * D / Norm;
      Y := DY * D / Norm;
      Z := DZ * D / Norm;
   end Random_Centre_Bias_Sphere_Distribution;

   ------------------------------
   -- Random_Cube_Distribution --
   ------------------------------

   procedure Random_Cube_Distribution
     (X, Y, Z : out Signed_Unit_Real)
   is
   begin
      X := Concorde.Random.Unit_Random * 2.0 - 1.0;
      Y := Concorde.Random.Unit_Random * 2.0 - 1.0;
      Z := Concorde.Random.Unit_Random * 2.0 - 1.0;
   end Random_Cube_Distribution;

   --------------------------------
   -- Random_Sphere_Distribution --
   --------------------------------

   procedure Random_Sphere_Distribution
     (X, Y, Z : out Signed_Unit_Real)
   is
   begin
      loop
         Random_Cube_Distribution (X, Y, Z);
         exit when X * X + Y * Y + Z * Z < 1.0;
      end loop;
   end Random_Sphere_Distribution;

   ----------------------
   -- Random_Star_Mass --
   ----------------------

   function Random_Star_Mass return Non_Negative_Real is
      Seed : constant Real := Concorde.Random.Unit_Random;
      Solar_Mass_Count : Real;
   begin
      if Seed <= 0.99 then
         Solar_Mass_Count :=
           0.1 + 6.0 * Seed - 15.0 * Seed ** 2
             + 11.0 * Seed ** 3;
      else
         declare
            X : constant Real := (Seed - 0.99) * 1.0E4;
            A : constant Real := 0.110833;
            B : constant Real := -14.0358;
            C : constant Real := 445.25;
         begin
            Solar_Mass_Count := A * X ** 2 + B * X + C;
         end;
      end if;
      return Solar_Mass_Count;
   end Random_Star_Mass;

   --------------------------------
   -- Right_Ascension_To_Radians --
   --------------------------------

   function Right_Ascension_To_Radians
     (Hours, Minutes, Seconds : Real)
      return Real
   is
   begin
      return (Hours * 15.0 + Minutes / 4.0 + Seconds / 4.0 / 60.0)
        * Ada.Numerics.Pi / 180.0;
   end Right_Ascension_To_Radians;

   ----------------------
   -- String_To_Real_3 --
   ----------------------

   procedure String_To_Real_3
     (Text    : String;
      X, Y, Z : out Real)
   is
      Start : Positive := Text'First;
      Index : Positive := Text'First;
      Result : array (1 .. 3) of Real := (others => 0.0);
      R_Idx  : Positive := 1;
   begin

      while Index <= Text'Last loop
         while Index <= Text'Last
           and then Text (Index) = ' '
         loop
            Index := Index + 1;
         end loop;

         Start := Index;

         while Index <= Text'Last
           and then Text (Index) /= ' '
         loop
            Index := Index + 1;
         end loop;

         if Index > Start then
            Result (R_Idx) := Real'Value (Text (Start .. Index - 1));
            R_Idx := R_Idx + 1;
         end if;
      end loop;

      X := Result (1);
      Y := Result (2);
      Z := Result (3);

   end String_To_Real_3;

end Concorde.Configure.Galaxies;
