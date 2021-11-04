with Ada.Numerics;

with Concorde.Elementary_Functions;
with Concorde.Logging;
with Concorde.Real_Images;
with Concorde.Terrain;

with Accord.Deposit;
with Accord.Resource_Constraint;

with Accord.Db;

package body Concorde.Sectors is

   Log_Sectors : constant Boolean := False;

   Root_2_Pi : constant Real :=
                 Concorde.Elementary_Functions.Sqrt
                   (1.0 * Ada.Numerics.Pi)
                   with Unreferenced;

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   function Calculate_Yield
     (Value, Min, Max : Real)
      return Unit_Real;

   ---------------------
   -- Calculate_Yield --
   ---------------------

   function Calculate_Yield
     (Value, Min, Max : Real)
      return Unit_Real
   is
      use Concorde.Elementary_Functions;
      Mean  : constant Real := Min + (Max - Min) / 2.0;
      Sigma : constant Real := Sqrt (Mean - Min);

      function Normal (X : Real) return Real
      is (Exp (-1.0 * (X ** 2 / 2.0)));

   begin
      return Normal ((Value - Mean) / Sigma);
   end Calculate_Yield;

   -----------------------
   -- Module_Group_Cost --
   -----------------------

   function Module_Group_Cost
     (Sector   : Accord.World_Sector.World_Sector_Class;
      Group    : Accord.Module_Group.Module_Group_Class)
      return Non_Negative_Real
   is
      function Deposit_Score return Non_Negative_Real;

      -------------------
      -- Deposit_Score --
      -------------------

      function Deposit_Score return Non_Negative_Real is
         Deposit : constant Accord.Deposit.Deposit_Class :=
                     Accord.Deposit.Get_By_World_Sector (Sector);
      begin
         if Deposit.Has_Element then
            return Deposit.Concentration;
         else
            return 0.0;
         end if;
      end Deposit_Score;

   begin
      return Factor : constant Non_Negative_Real :=
        1.0
          * (if Group.Habitable_Factor = 0.0 then 1.0
             else (1.25 - Sector.Habitability) / Group.Habitable_Factor)
        * (if Group.Deposit_Factor = 0.0 then 1.0
           else (1.5 - Deposit_Score) / Group.Deposit_Factor)
        * Concorde.Terrain.Module_Group_Cost (Sector.Terrain, Group)
      do
         if Log_Sectors and then Factor /= 1.0 then
            Concorde.Logging.Log
              ("module-group-cost",
               "sector" & Accord.Db.To_String (Sector.Reference_World_Sector)
               & " terrain "
               & Sector.Terrain.Tag
               & " group "
               & Group.Tag
               & " cost "
               & Concorde.Real_Images.Approximate_Image
                 (Factor * 100.0)
               & "%");
         end if;
      end return;
   end Module_Group_Cost;

   --------------------
   -- Resource_Yield --
   --------------------

   function Resource_Yield
     (Sector   : Accord.World_Sector.World_Sector_Class;
      Resource : Accord.Resource.Resource_Class)
      return Non_Negative_Real
   is
      use type Accord.Db.Life_Complexity_Type;
      subtype Resource_Constraint_Class is
        Accord.Resource_Constraint.Resource_Constraint_Class;
      Constraint : constant Resource_Constraint_Class :=
                     Accord.Resource_Constraint.First_By_Resource
                       (Resource);
      Yield : Unit_Real := 1.0;
   begin
      if Constraint.Life_Constraint then
         if Sector.World.Life < Constraint.Min_Lifeforms then
            Yield := 0.0;
         end if;
      end if;

      if Constraint.Terrain_Constraint then
         if Sector.Terrain.Tag /= Constraint.Terrain.Tag then
            Yield := 0.0;
         end if;
      end if;

      if Constraint.Temperature_Constraint then
         declare
            This : constant Real :=
              Calculate_Yield
                (Sector.Average_Temperature,
                 Constraint.Min_Temperature,
                 Constraint.Max_Temperature);
         begin
            Concorde.Logging.Log
              (Category => Resource.Tag,
               Message  =>
                 "temperature: min "
               & Image (Constraint.Min_Temperature - 273.0)
               & "; max "
               & Image (Constraint.Max_Temperature - 273.0)
               & "; average "
               & Image (Sector.Average_Temperature - 273.0)
               & "; base yield "
               & Image (This * 100.0) & "%"
               & "; final yield "
               & Image (This * Resource.Yield * 100.0) & "%");

            Yield := Real'Min (Yield, This * Resource.Yield);
         end;
      end if;

      if Constraint.Moisture_Constraint then
         declare
            This : constant Real :=
              Calculate_Yield
                (Sector.Moisture,
                 Constraint.Min_Moisture,
                 Constraint.Max_Moisture);
         begin
            Concorde.Logging.Log
              (Category => Resource.Tag,
               Message  =>
                 "moisture: min "
               & Image (Constraint.Min_Moisture * 100.0) & "%"
               & "; max "
               & Image (Constraint.Max_Moisture * 100.0) & "%"
               & "; average "
               & Image (Sector.Moisture * 100.0) & "%"
               & "; yield "
               & Image (This * 100.0) & "%");

            Yield := Real'Min (Yield, This);
         end;
      end if;

      return Yield;
   end Resource_Yield;

end Concorde.Sectors;
