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
      return Concorde.Trigonometry.Angle
   is
      use Concorde.Calendar;
   begin
      return Calculate_Longitude
        (Large_Mass => Large_Mass,
         Orbit      => Orbit,
         Elapsed    => Clock - Epoch);
   end Calculate_Current_Longitude;

   --------------------------------
   -- Calculate_Current_Position --
   --------------------------------

   procedure Calculate_Current_Position
     (Object     : Accord.Orbiting_Object.Orbiting_Object_Class;
      Latitude   : out Concorde.Trigonometry.Angle;
      Longitude  : out Concorde.Trigonometry.Angle)
   is
      use type Concorde.Calendar.Time;
   begin
      Calculate_Position
        (Object    => Object,
         Elapsed   => Concorde.Calendar.Clock - Concorde.Calendar.Start,
         Latitude  => Latitude,
         Longitude => Longitude);
   end Calculate_Current_Position;

   -------------------------
   -- Calculate_Longitude --
   -------------------------

   function Calculate_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Concorde_Duration)
      return Concorde.Trigonometry.Angle
   is
      Period      : constant Concorde_Duration :=
        Concorde.Orbits.Period (Large_Mass, Orbit);
      Orbit_Count : constant Real := Real (Elapsed) / Real (Period);
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
      return Concorde.Trigonometry.From_Degrees (Longitude);
   end Calculate_Longitude;

   ------------------------
   -- Calculate_Position --
   ------------------------

   procedure Calculate_Position
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Concorde_Duration;
      Latitude   : out Concorde.Trigonometry.Angle;
      Longitude  : out Concorde.Trigonometry.Angle)
   is
      Period      : constant Concorde_Duration :=
        Concorde.Orbits.Period (Large_Mass, Orbit);
      Orbit_Count : constant Non_Negative_Real :=
                      Real (Elapsed) / Real (Period);
      Partial     : constant Unit_Real :=
        Orbit_Count - Real'Truncation (Orbit_Count);
   begin
      Longitude := Concorde.Trigonometry.From_Degrees (Partial * 360.0);
      Latitude  := Concorde.Trigonometry.From_Degrees (0.0);
      --  Latitude := Concorde.Trigonometry.Arcsin (Partial);
   end Calculate_Position;

   ------------------------
   -- Calculate_Position --
   ------------------------

   procedure Calculate_Position
     (Object     : Accord.Orbiting_Object.Orbiting_Object_Class;
      Elapsed    : Concorde_Duration;
      Latitude   : out Concorde.Trigonometry.Angle;
      Longitude  : out Concorde.Trigonometry.Angle)
   is
   begin
      Calculate_Position
        (Large_Mass => Object.Primary_Massive.Mass,
         Orbit      => Object.Semimajor_Axis,
         Elapsed    => Elapsed,
         Latitude   => Latitude,
         Longitude  => Longitude);
   end Calculate_Position;

   -----------------------
   -- Current_Longitude --
   -----------------------

   function Current_Longitude
     (Object : Accord.Orbiting_Object.Orbiting_Object_Class)
      return Concorde.Trigonometry.Angle
   is
      use type Concorde.Calendar.Time;
   begin
      return Calculate_Longitude
        (Large_Mass => Object.Primary_Massive.Mass,
         Orbit      => Object.Semimajor_Axis,
         Elapsed    => Concorde.Calendar.Clock - Concorde.Calendar.Start);
   end Current_Longitude;

   ---------------------------
   -- Get_Longitude_At_Time --
   ---------------------------

   function Get_Longitude_At_Time
     (Object  : Accord.Orbiting_Object.Orbiting_Object_Class;
      At_Time : Concorde.Calendar.Time)
      return Concorde.Trigonometry.Angle
   is
      use type Concorde.Calendar.Time;
   begin
      return Calculate_Longitude
        (Large_Mass => Object.Primary_Massive.Mass,
         Orbit      => Object.Semimajor_Axis,
         Elapsed    => At_Time - Concorde.Calendar.Start);
   end Get_Longitude_At_Time;

   ------------
   -- Period --
   ------------

   function Period
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real)
      return Concorde_Duration
   is
      Pi : constant := Ada.Numerics.Pi;
      G  : constant := Concorde.Constants.Gravitational_Constant;
      M  : constant Non_Negative_Real := Large_Mass;
   begin
      return Concorde_Duration (2.0 * Pi * Sqrt (Orbit ** 3 / G / M));
   end Period;

end Concorde.Orbits;
