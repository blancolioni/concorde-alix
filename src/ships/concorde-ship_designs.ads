with Accord.Ship_Design;

package Concorde.Ship_Designs is

   subtype Ship_Design_Class is Accord.Ship_Design.Ship_Design_Class;

   function Dry_Mass (Design : Ship_Design_Class) return Non_Negative_Real;

end Concorde.Ship_Designs;
