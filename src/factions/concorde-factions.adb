with Concorde.Attributes;
with Concorde.Individuals;

with Accord.Colony;
with Accord.Colony_Sector;
with Accord.Colony_Sector_Module;
with Accord.Individual;
with Accord.Office;

package body Concorde.Factions is

   function Calculate_Attribute
     (Faction : Faction_Class;
      Tag     : String)
      return Non_Negative_Real;

   -------------------------
   -- Calculate_Attribute --
   -------------------------

   function Calculate_Attribute
     (Faction : Faction_Class;
      Tag     : String)
      return Non_Negative_Real
   is
      Result : Integer := Concorde.Attributes.Get (Faction, Tag);
   begin
      for Colony of Accord.Colony.Select_By_Faction (Faction) loop
         for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
            for Sector_Module of
              Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
            loop
               Result := Result
                 + Concorde.Attributes.Get (Sector_Module.Module, Tag);
            end loop;
         end loop;
      end loop;

      for Office of Accord.Office.Scan_By_Rank loop
         declare
            Individual : constant Accord.Individual.Individual_Handle :=
                           Accord.Individual.First_By_Faction_Office
                             (Faction, Office);
         begin
            if Individual.Has_Element then
               Result := Result
                 + Concorde.Individuals.Ruling_Bonus (Individual, Office)
                 * Concorde.Attributes.Get (Office, Tag);
            end if;
         end;
      end loop;

      declare
         Denominator : Natural := 0;
      begin
         for Colony of Accord.Colony.Select_By_Faction (Faction) loop
            Denominator := Denominator + 1;
            for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
               Denominator := Denominator + 1;
            end loop;
         end loop;

         return Real (Integer'Max (Result, 0))
           / Real (Integer'Max (Denominator, 1));
      end;
   end Calculate_Attribute;

   ----------------
   -- Control_DC --
   ----------------

   function Control_DC
     (Faction : Faction_Class)
      return Natural
   is
   begin
      return Result : Natural := 0 do
         for Colony of Accord.Colony.Select_By_Faction (Faction) loop
            Result := Result + 1;
            for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
               Result := Result + 1;
            end loop;
         end loop;
      end return;
   end Control_DC;

   -------------
   -- Economy --
   -------------

   function Economy
     (Faction : Faction_Class)
      return Non_Negative_Real
   is
   begin
      return Calculate_Attribute (Faction, "economy");
   end Economy;

   ----------------------
   -- For_All_Factions --
   ----------------------

   procedure For_All_Factions
     (Process : not null access
        procedure (Faction : Faction_Class))
   is
   begin
      for Faction of Accord.Faction.Scan_By_Top_Record loop
         Process (Faction);
      end loop;
   end For_All_Factions;

   ----------------------
   -- Get_User_Faction --
   ----------------------

   function Get_User_Faction
     (User : Accord.User.User_Class)
      return Faction_Class
   is
   begin
      return Accord.Faction.First_By_User (User);
   end Get_User_Faction;

   -------------
   -- Loyalty --
   -------------

   function Loyalty
     (Faction : Faction_Class)
      return Non_Negative_Real
   is
   begin
      return Calculate_Attribute (Faction, "loyalty");
   end Loyalty;

   ---------------
   -- Stability --
   ---------------

   function Stability
     (Faction : Faction_Class)
      return Non_Negative_Real
   is
   begin
      return Calculate_Attribute (Faction, "stability");
   end Stability;

   ------------
   -- Unrest --
   ------------

   function Unrest
     (Faction : Faction_Class)
      return Non_Negative_Real
   is
   begin
      return Calculate_Attribute (Faction, "unrest");
   end Unrest;

end Concorde.Factions;
