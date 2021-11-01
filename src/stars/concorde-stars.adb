with Concorde.Solar_System;

with Accord.Db;

package body Concorde.Stars is

   ------------------
   -- Solar_Masses --
   ------------------

   function Solar_Masses
     (Star : Accord.Star.Star_Class)
      return Non_Negative_Real
   is
   begin
      return Star.Mass / Concorde.Solar_System.Solar_Mass;
   end Solar_Masses;

   -------------------
   -- Spectral_Type --
   -------------------

   function Spectral_Type
     (Star : Accord.Star.Star_Class)
      return String
   is
   begin
      return Accord.Db.Spectral_Class'Image (Star.Class)
        & Character'Val (Star.Subclass + 48);
   end Spectral_Type;

end Concorde.Stars;
