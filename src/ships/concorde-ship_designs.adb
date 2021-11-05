package body Concorde.Ship_Designs is

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass (Design : Ship_Design_Class) return Non_Negative_Real is
   begin
      return Design.Tonnage;
   end Dry_Mass;

end Concorde.Ship_Designs;
