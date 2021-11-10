with WL.Random;

with Concorde.Agents;
with Concorde.Attributes;
with Concorde.Money;
with Concorde.Real_Images;

with Accord.Colony_Edict;
with Accord.Edict_Group;
with Accord.Individual;
with Accord.Office;

package body Concorde.Colonies.Updates is

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   -------------------
   -- Collect_Taxes --
   -------------------

   procedure Collect_Taxes (Colony : Colony_Class) is
      Taxation_Group : constant Accord.Edict_Group.Edict_Group_Class :=
                         Accord.Edict_Group.Get_By_Tag ("taxation");
      Colony_Edict   : constant Accord.Colony_Edict.Colony_Edict_Class :=
                         Accord.Colony_Edict.Get_By_Colony_Edict
                           (Colony, Taxation_Group);
      Ruler          : constant Accord.Individual.Individual_Class :=
                         Accord.Individual.First_By_Faction_Office
                           (Faction => Colony.Faction,
                            Office  => Accord.Office.Get_By_Tag ("treasurer"));
   begin
      if not Ruler.Has_Element then
         Log (Colony, "no treasurer, no taxes");
         return;
      end if;

      declare
         use Concorde.Money;
         Result : constant Check_Result :=
                    Check
                      (Colony        => Colony,
                       Checker       => Ruler,
                       Attribute     => Economy'Access,
                       Attribute_Tag => "economy",
                       Check_Tag     => "collect-taxes",
                       Penalty       => Unrest (Colony),
                       Penalty_Tag   => "unrest");
         Old_Unrest : constant Natural :=
                        Concorde.Attributes.Get (Colony, "unrest");
         New_Unrest : Natural := Old_Unrest;
         Collection : constant Integer := Total (Result);
         Taxes      : constant Money_Type :=
                        (if Collection > 0
                         then To_Money
                           (Real (Collection)
                            * Colony_Edict.Edict.Revenue
                            * To_Real (Gdp (Colony)) * 0.01)
                         else Zero);
      begin
         if Major_Failure (Result) then
            New_Unrest := New_Unrest + 4;
         elsif Failure (Result) then
            New_Unrest := New_Unrest + 2;
         end if;

         Log
           (Colony,
            "taxation edict " & Colony_Edict.Edict.Tag
            & "; revenue " & Image (Colony_Edict.Edict.Revenue * 100.0)
            & "%; total taxes " & Show (Taxes));
         Concorde.Agents.Add_Cash (Colony.Faction, Taxes, "taxes");

         if New_Unrest > Old_Unrest then
            Concorde.Attributes.Increase
              (Colony, "unrest", New_Unrest - Old_Unrest);
         end if;
      end;

   end Collect_Taxes;

   ---------------------
   -- Pay_Maintenance --
   ---------------------

   procedure Pay_Maintenance (Colony : Colony_Class) is
      use Concorde.Money;
      Amount : constant Money_Type :=
                 Total_Maintenance (Colony);
   begin
      Log (Colony,
           "maintenance "
           & Show (Amount));

      if Amount > Concorde.Agents.Cash (Colony.Faction) then
         Concorde.Attributes.Increase (Colony, "unrest", 2);
      end if;

      Concorde.Agents.Remove_Cash (Colony.Faction, Amount,
                                   "colony-maintenance");
   end Pay_Maintenance;

   ---------------------
   -- Stability_Check --
   ---------------------

   procedure Stability_Check (Colony : Colony_Class) is
      Ruler          : constant Accord.Individual.Individual_Class :=
                         Accord.Individual.First_By_Faction_Office
                           (Faction => Colony.Faction,
                            Office  => Accord.Office.Get_By_Tag ("ruler"));
      Result         : constant Check_Result :=
                         Check
                           (Colony        => Colony,
                            Checker       => Ruler,
                            Attribute     => Stability'Access,
                            Attribute_Tag => "stability",
                            Check_Tag     => "stability-check",
                            Penalty       => Unrest (Colony),
                            Penalty_Tag   => "unrest");

   begin
      if Success (Result) then
         if Unrest (Colony) = 0 then
            Concorde.Agents.Add_Cash
              (Agent => Colony.Faction,
               Cash  => Concorde.Money.To_Money (1.0),
               Tag   => "stability-check");
            Log
              (Colony,
               "success: cash now "
               & Concorde.Money.Show (Concorde.Agents.Cash (Colony.Faction)));
         else
            Concorde.Attributes.Decrease (Colony, "unrest", 1);
            Log
              (Colony,
               "success: unrest now" & Unrest (Colony)'Image);
         end if;
      elsif not Major_Failure (Result) then
         Concorde.Attributes.Increase (Colony, "unrest", 1);
         Log
           (Colony,
            "failure: unrest now" & Unrest (Colony)'Image);
      else
         declare
            Increase : constant Positive :=
                         WL.Random.Random_Number (1, 4) + 1;
         begin
            Concorde.Attributes.Increase (Colony, "unrest", Increase);
            Log
              (Colony,
               "major failure: unrest now" & Unrest (Colony)'Image);
         end;
      end if;
   end Stability_Check;

end Concorde.Colonies.Updates;
