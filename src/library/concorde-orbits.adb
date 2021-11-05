with Ada.Numerics;

with Concorde.Constants;
with Concorde.Elementary_Functions;

package body Concorde.Orbits is

   function Sqrt (X : Non_Negative_Real) return Non_Negative_Real
                  renames Concorde.Elementary_Functions.Sqrt;

   ---------------------------------
   -- Calculate_Current_Longitude --
   ---------------------------------

   function Calculate_Current_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Epoch      : Concorde.Calendar.Time)
      return Real
   is
      use Concorde.Calendar;
   begin
      return Calculate_Longitude
        (Large_Mass => Large_Mass,
         Orbit      => Orbit,
         Elapsed    => Clock - Epoch);
   end Calculate_Current_Longitude;

   -------------------------
   -- Calculate_Longitude --
   -------------------------

   function Calculate_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Concorde_Duration)
      return Real
   is
      Period      : constant Non_Negative_Real :=
        Concorde.Orbits.Period (Large_Mass, Orbit);
      Orbit_Count : constant Real := Real (Elapsed) / Period;
      Partial     : constant Unit_Real :=
        Orbit_Count - Real'Floor (Orbit_Count);
      Longitude   : Real := Partial * 360.0;
   begin
      Longitude := Partial * 360.0;
      if Longitude >= 360.0 then
         Longitude := Longitude - 360.0;
      end if;
      if Longitude >= 180.0 then
         Longitude := -(360.0 - Longitude);
      end if;
      return Longitude;
   end Calculate_Longitude;

   ------------------------
   -- Calculate_Position --
   ------------------------

   procedure Calculate_Position
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Duration;
      Latitude   : out Real;
      Longitude  : out Real)
   is
      use Concorde.Elementary_Functions;
      Period      : constant Non_Negative_Real :=
        Concorde.Orbits.Period (Large_Mass, Orbit);
      Orbit_Count : constant Non_Negative_Real := Real (Elapsed) / Period;
      Partial     : constant Unit_Real :=
        Orbit_Count - Real'Truncation (Orbit_Count);
   begin
      Longitude := Partial * 360.0;
      if Longitude >= 360.0 then
         Longitude := Longitude - 360.0;
      end if;
      if Longitude >= 180.0 then
         Longitude := -(360.0 - Longitude);
      end if;
      Latitude := Arcsin (Partial, 360.0);
   end Calculate_Position;

   ------------
   -- Period --
   ------------

   function Period
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real)
      return Non_Negative_Real
   is
      Pi : constant := Ada.Numerics.Pi;
      G  : constant := Concorde.Constants.Gravitational_Constant;
      M  : constant Non_Negative_Real := Large_Mass;
   begin
      return 2.0 * Pi * Sqrt (Orbit ** 3 / G / M);
   end Period;

end Concorde.Orbits;
