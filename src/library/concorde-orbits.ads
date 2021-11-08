with Concorde.Calendar;
with Concorde.Trigonometry;

with Accord.Orbiting_Object;

package Concorde.Orbits is

   function Period
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real)
      return Concorde_Duration;

   procedure Calculate_Position
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Concorde_Duration;
      Latitude   : out Concorde.Trigonometry.Angle;
      Longitude  : out Concorde.Trigonometry.Angle);

   function Calculate_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Elapsed    : Concorde_Duration)
      return Concorde.Trigonometry.Angle;

   function Calculate_Current_Longitude
     (Large_Mass : Non_Negative_Real;
      Orbit      : Non_Negative_Real;
      Epoch      : Concorde.Calendar.Time)
      return Concorde.Trigonometry.Angle;

   function Current_Longitude
     (Object : Accord.Orbiting_Object.Orbiting_Object_Class)
      return Concorde.Trigonometry.Angle;

   function Get_Longitude_At_Time
     (Object  : Accord.Orbiting_Object.Orbiting_Object_Class;
      At_Time : Concorde.Calendar.Time)
      return Concorde.Trigonometry.Angle;

   procedure Calculate_Position
     (Object     : Accord.Orbiting_Object.Orbiting_Object_Class;
      Elapsed    : Concorde_Duration;
      Latitude   : out Concorde.Trigonometry.Angle;
      Longitude  : out Concorde.Trigonometry.Angle);

   procedure Calculate_Current_Position
     (Object     : Accord.Orbiting_Object.Orbiting_Object_Class;
      Latitude   : out Concorde.Trigonometry.Angle;
      Longitude  : out Concorde.Trigonometry.Angle);

end Concorde.Orbits;
