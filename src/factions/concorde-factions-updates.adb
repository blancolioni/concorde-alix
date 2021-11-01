with Concorde.Random;
with Concorde.Real_Images;

package body Concorde.Factions.Updates is

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   function Check
     (Check_Name : String;
      Faction    : Faction_Class;
      Office     : Accord.Office.Office_Class;
      Plus       : Non_Negative_Real;
      Minus      : Non_Negative_Real)
      return Boolean;

   ---------------------
   -- Stability_Check --
   ---------------------

   procedure Stability_Check (Faction : Faction_Class) is
      Current_Stab : constant Non_Negative_Real := Stability (Faction);
      Ruler        : constant Accord.Office.Office_Class :=
                       Accord.Office.Get_By_Tag ("ruler");
      Success      : constant Boolean :=

   begin
      for Colony of Accord.Colony.Select_By_Faction
   end Stability_Check;

end Concorde.Factions.Updates;
