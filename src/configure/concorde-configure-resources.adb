with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;

with WL.Random.Weighted_Random_Choices;
with WL.Random;

with Concorde.Elementary_Functions;
with Concorde.Random;
--  with Concorde.Real_Images;
with Concorde.Solar_System;

with Accord.Deposit;
with Accord.Fuzzy_Set;
with Accord.Atmospheric_Gas;
with Accord.Resource_Constraint;
with Accord.Resource_Sphere;
with Accord.Star_System;
with Accord.World_Sector;

with Accord.Db;

with Concorde.Logging;
with Concorde.Real_Images;

package body Concorde.Configure.Resources is

   package Resource_Choices is
     new WL.Random.Weighted_Random_Choices
       (Element_Type => Accord.Resource.Resource_Handle,
        "="          => Accord.Resource."=");

   function Create_Score
     (World     : Accord.World.World_Class;
      Resource  : Accord.Resource.Resource_Class;
      Generator : Random_Deposit_Generator)
      return Natural;

   function Sample
     (A, B, C, D : Real;
      Position   : Real)
      return Unit_Real
   is (if Position <= A
       then 0.0
       elsif Position <= B
       then (Position - A) / (B - A)
       elsif Position <= C
       then 1.0
       elsif Position <= D
       then (D - Position) / (D - C)
       else 0.0);

--     function Image (X : Real) return String
--                     renames Concorde.Real_Images.Approximate_Image;

   -------------------------------------
   -- Configure_Atmosphere_Components --
   -------------------------------------

   procedure Configure_Atmosphere_Components
     (Config : Tropos.Configuration)
   is
   begin
      for Cfg of Config loop
         declare
            use Concorde.Solar_System;
            function Get (Name : String)
                          return Non_Negative_Real
            is (Get_Real (Cfg, Name));

            Formula      : constant String :=
                             Cfg.Get ("formula", Cfg.Config_Name);
            Weight       : constant Non_Negative_Real := Get ("weight");
            MP           : constant Non_Negative_Real := Get ("mp");
            BP           : constant Non_Negative_Real := Get ("bp");
            Density      : constant Non_Negative_Real := Get ("density");
            Abund_E      : constant Non_Negative_Real := Get ("abunde");
            Abund_S      : constant Non_Negative_Real := Get ("abunds");
            React        : constant Non_Negative_Real := Get ("react");
            Max_IPP_HG   : constant Non_Negative_Real := Get ("max_ipp_hg");
            Max_IPP_PPM  : constant Non_Negative_Real := Get ("max_ipp_ppm");

         begin
            Accord.Atmospheric_Gas.Create
              (Tag              =>
                 Ada.Characters.Handling.To_Lower (Cfg.Config_Name),
               Formula          => Formula,
               Molecular_Weight => Weight,
               Melting_Point    => MP,
               Boiling_Point    => BP,
               Density          => Density,
               Abundance_E      => Abund_E,
               Abundance_S      => Abund_S,
               Reactivity       => React,
               Max_Ipp          =>
                 (if Max_IPP_HG /= 0.0
                  then Max_IPP_HG * Earth_Surface_Pressure / 760.0
                  else Max_IPP_PPM * Earth_Surface_Pressure / 1.0E6));
         end;
      end loop;
   end Configure_Atmosphere_Components;

   ---------------------
   -- Create_Deposits --
   ---------------------

   procedure Create_Deposits
     (World     : Accord.World.World_Class;
      Generator : Random_Deposit_Generator)
   is
      use Concorde.Elementary_Functions;
      Initial_Concentration : constant Unit_Real :=
        (0.4 + Concorde.Random.Unit_Random / 2.0)
        ** (World.Radius
            / Concorde.Solar_System.Earth_Radius);
      Concentration         : Unit_Real := Initial_Concentration;
      Deposit_Count         : Positive;
      package Sector_Vectors is
        new Ada.Containers.Vectors
          (Positive, Accord.World_Sector.World_Sector_Handle,
           Accord.World_Sector."=");

      Sector_Refs           : Sector_Vectors.Vector;

      Resource_Choice : Resource_Choices.Weighted_Choice_Set;

   begin

      for Resource of
        Accord.Resource.Scan_By_Tag
      loop
         declare
            Score : constant Natural :=
                      Create_Score
                        (World    => World,
                         Resource => Resource,
                         Generator => Generator);
         begin
            Concorde.Logging.Log
              (World.Name,
               Resource.Tag
               & ": score" & Score'Image);

            Resource_Choice.Insert
              (Resource.To_Resource_Handle, Score);
         end;
      end loop;

      for World_Sector of
        Accord.World_Sector.Select_By_World
          (World)
      loop
         Sector_Refs.Append (World_Sector.To_World_Sector_Handle);
      end loop;

      Deposit_Count :=
        Natural'Max
          (Sector_Refs.Last_Index / 5, 1);

      while Concentration > Initial_Concentration / 20.0 loop
         declare
            Resource : constant Accord.Resource.Resource_Handle :=
                         Resource_Choice.Choose;
            Sector_Index : constant Natural :=
                             (if Sector_Refs.Is_Empty
                              then 0
                              else WL.Random.Random_Number
                                (1, Sector_Refs.Last_Index));
            This_Conc    : constant Unit_Real :=
                             Unit_Clamp
                               ((Concorde.Random.Normal_Random (0.1)
                                + 1.0)
                                * Concentration);
         begin
            Concorde.Logging.Log
              (World.Name,
               "choice: "
               & Resource.Tag
               & "; concentration "
               & Concorde.Real_Images.Approximate_Image (This_Conc));

            Accord.Deposit.Create
              (World         => World,
               World_Sector  =>
                 (if Sector_Refs.Is_Empty
                  then Accord.World_Sector.Empty_Handle
                  else Sector_Refs.Element (Sector_Index)),
               Resource      => Resource,
               Concentration => This_Conc,
               Difficulty    => 1.0 - This_Conc,
               Available     =>
                 Concorde.Quantities.To_Quantity
                   (Concorde.Random.About
                        (2.0e5, 1.0e5)));
            if not Sector_Refs.Is_Empty then
               Sector_Refs (Sector_Index) :=
                 Sector_Refs.Element (Sector_Refs.Last_Index);
               Sector_Refs.Delete_Last;
            end if;
         end;

         Concentration := Concentration
           * (Concorde.Random.Unit_Random + 15.0) / 16.0;
         Deposit_Count := Natural'Max (Deposit_Count - 1, 1);
      end loop;
   end Create_Deposits;

   ----------------------
   -- Create_Generator --
   ----------------------

   function Create_Generator
     (X, Y, Z : Real)
      return Random_Deposit_Generator
   is
      Gen : Random_Deposit_Generator;
   begin
      for Sphere of Accord.Resource_Sphere.Scan_By_Top_Record loop
         declare
            use Concorde.Elementary_Functions;
            Distance : constant Non_Negative_Real :=
              Sqrt ((X - Sphere.Centre_X) ** 2
                    + (Y - Sphere.Centre_Y) ** 2
                    + (Z - Sphere.Centre_Z) ** 2);
            Strength : constant Non_Negative_Real :=
              (if Distance <= Sphere.Radius
               then Sphere.Strength
               else Sphere.Strength
               / ((Distance - Sphere.Radius + 1.0) ** Sphere.Attenuation));
            Found    : Boolean := False;

         begin
            for Item of Gen.Resources loop
               if Item.Resource.Tag = Sphere.Resource.Tag then
                  Item.Strength := Item.Strength + Strength;
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Gen.Resources.Append
                 (Resource_Record'
                    (Sphere.Resource.To_Resource_Handle, Strength));
            end if;

            Gen.Total_Strength := Gen.Total_Strength + Strength;
         end;

      end loop;
      return Gen;
   end Create_Generator;

   -----------------------------
   -- Create_Resource_Spheres --
   -----------------------------

   procedure Create_Resource_Spheres
     (System_Count  : Positive;
      R_X, R_Y, R_Z : Non_Negative_Real)
   is
   begin
      for Constraint of
        Accord.Resource_Constraint.Select_By_Sphere_Constraint (True)
      loop
         declare
            RX     : constant Real := Constraint.Sphere_Rx * R_X;
            RY     : constant Real := Constraint.Sphere_Ry * R_Y;
            RZ     : constant Real := Constraint.Sphere_Rz * R_Z;
            Count  : constant Natural :=
                       Natural (Constraint.Sphere_Frequency
                                * Real (System_Count));
         begin

            for I in 1 .. Count loop
               declare
                  X     : constant Real :=
                            RX * (Concorde.Random.Unit_Random * 2.0 - 1.0);
                  Y     : constant Real :=
                            RY * (Concorde.Random.Unit_Random * 2.0 - 1.0);
                  Z     : constant Real :=
                            RZ * (Concorde.Random.Unit_Random * 2.0 - 1.0);
                  S      : constant Real :=
                             Real'Max
                               (Concorde.Random.Normal_Random
                                  (Constraint.Standard_Deviation)
                                + Constraint.Mean,
                                0.0);
                  A      : constant Real :=
                             Constraint.Attenuation_Min
                               + Concorde.Random.Unit_Random
                             * (Constraint.Attenuation_Max
                                - Constraint.Attenuation_Min);
                  R      : constant Real :=
                             Concorde.Random.Unit_Random * RX + RX / 2.0;
               begin

                  if R > 0.0 and then S > 0.0 then
                     Accord.Resource_Sphere.Create
                       (Resource    => Constraint.Resource,
                        Centre_X    => X,
                        Centre_Y    => Y,
                        Centre_Z    => Z,
                        Strength    => S,
                        Radius      => R,
                        Attenuation => A);
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Create_Resource_Spheres;

   ------------------
   -- Create_Score --
   ------------------

   function Create_Score
     (World     : Accord.World.World_Class;
      Resource  : Accord.Resource.Resource_Class;
      Generator : Random_Deposit_Generator)
      return Natural
   is
      pragma Unreferenced (Generator);

      type Constraint_Record is
         record
            Mean    : Real;
            Std_Dev : Non_Negative_Real;
            Weight  : Non_Negative_Real;
         end record;

      function Score_Spheres return Constraint_Record;

      package Constraint_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Constraint_Record);

      Sub_Constraints : Constraint_Lists.List;
      Total_Weight    : Non_Negative_Real := 0.0;

      function Check
        (Constraint : Accord.Resource_Constraint
         .Resource_Constraint_Class)
         return Boolean;

      -----------
      -- Check --
      -----------

      function Check
        (Constraint : Accord.Resource_Constraint
         .Resource_Constraint_Class)
         return Boolean
      is
         use type Accord.Db.Life_Complexity_Type;
         use type Accord.Db.Stellar_Orbit_Zone;
         use type Accord.Db.World_Composition;
      begin
         if Constraint.Zone_Constraint
           and then Constraint.Zone /= World.Orbit_Zone
         then
            return False;
         end if;

         if Constraint.Life_Constraint
           and then Constraint.Min_Lifeforms > World.Life
         then
            return False;
         end if;

         if Constraint.Age_Constraint
           and then Constraint.Min_Age > World.Age
         then
            return False;
         end if;

         if Constraint.Hydrosphere_Constraint
           and then Constraint.Min_Hydrosphere > World.Hydrosphere
         then
            return False;
         end if;

         if Constraint.Composition_Constraint
           and then Constraint.Composition /= World.Composition
         then
            return False;
         end if;

         if Constraint.Sphere_Constraint then
            return False;
         end if;

         return True;
      end Check;

      -------------------
      -- Score_Spheres --
      -------------------

      function Score_Spheres return Constraint_Record is
         System : constant Accord.Star_System.Star_System_Class :=
                    World.Star_System;
         Mean   : Non_Negative_Real := 0.0;
      begin
         for Resource_Sphere of
           Accord.Resource_Sphere.Select_By_Resource (Resource)
         loop
            declare
               use Concorde.Elementary_Functions;
               D : constant Non_Negative_Real :=
                     Sqrt ((System.X - Resource_Sphere.Centre_X) ** 2
                           + (System.Y - Resource_Sphere.Centre_Y) ** 2
                           + (System.Z - Resource_Sphere.Centre_Z) ** 2);
            begin
--                 Ada.Text_IO.Put_Line
--                   (Accord.Resource.Get (Resource).Tag
--                    & ": sphere at ("
--                    & Image (Resource_Sphere.Centre_X)
--                    & "," & Image (Resource_Sphere.Centre_Y)
--                    & "," & Image (Resource_Sphere.Centre_Z)
--                    & ") distance "
--                    & Image (D)
--                    & " strength "
--                    & Image (Resource_Sphere.Strength)
--                    & " attenuation "
--                    & Image (Resource_Sphere.Attenuation)
--                    & " contribution "
--                      & Image (Resource_Sphere.Strength
--                      / (D ** Resource_Sphere.Attenuation)));
               if D < Resource_Sphere.Radius then
                  Mean := Mean
                    + Resource_Sphere.Strength
                    / (D ** Resource_Sphere.Attenuation);
               end if;
            end;
         end loop;
         return (Mean / 100.0, Mean / 1_000.0, 1.0);
      end Score_Spheres;

   begin

      for Constraint of
        Accord.Resource_Constraint.Select_By_Resource (Resource)
      loop
         if Constraint.Sphere_Constraint then
            declare
               Sub : constant Constraint_Record := Score_Spheres;
            begin
               Sub_Constraints.Append (Sub);
               Total_Weight := Total_Weight + Sub.Weight;
            end;
         elsif Check (Constraint) then
            if Constraint.Unlimited then
               return 0;
            elsif Constraint.Mass_Constraint then
               declare
                  M_E : constant Non_Negative_Real :=
                          World.Mass
                            / Concorde.Solar_System.Earth_Mass;
                  Fuzzy_Set : constant Accord.Fuzzy_Set.Fuzzy_Set_Class :=
                                Constraint.Mass;
                  Weight : constant Unit_Real :=
                             Sample
                               (Fuzzy_Set.Max_Zero, Fuzzy_Set.Min_One,
                                Fuzzy_Set.Max_One, Fuzzy_Set.Min_Zero,
                                M_E);
               begin
                  Sub_Constraints.Append
                    ((Constraint.Mean, Constraint.Standard_Deviation, Weight));
                  Total_Weight := Total_Weight + Weight;
               end;
            else
               Sub_Constraints.Append
                 ((Constraint.Mean, Constraint.Standard_Deviation, 1.0));
               Total_Weight := Total_Weight + 1.0;
            end if;
         end if;
      end loop;

      declare
         Score : Non_Negative_Real := 0.0;
      begin
         for Item of Sub_Constraints loop
            declare
               Item_Score : constant Real :=
                              (Concorde.Random.Normal_Random
                                 (Item.Std_Dev)
                               + Item.Mean)
                              * Item.Weight / Total_Weight;
            begin
               Score := Score + Real'Max (Item_Score, 0.0);
            end;
         end loop;

--           if Score > 0.0 then
--              Ada.Text_IO.Put_Line
--          (World.Name & ": " & Accord.Resource.Get (Resource).Tag
--                 & ": " & Concorde.Real_Images.Approximate_Image (Score));
--           end if;

         return Natural (Score * 1000.0);
      end;

   end Create_Score;

end Concorde.Configure.Resources;
