with Accord.Ship_Design_Module;

package body Concorde.Ship_Designs is

   --------------
   -- Dry_Mass --
   --------------

   function Dry_Mass (Design : Ship_Design_Class) return Non_Negative_Real is
   begin
      return Mass : Non_Negative_Real := Design.Dry_Mass do
         for Module of
           Accord.Ship_Design_Module.Select_By_Ship_Design (Design)
         loop
            Mass := Mass + Module.Mass;
         end loop;
      end return;
   end Dry_Mass;

end Concorde.Ship_Designs;
