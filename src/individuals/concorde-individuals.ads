with Concorde.Abilities;

with Accord.Ability;
with Accord.Individual;
with Accord.Office;
with Accord.Skill;

package Concorde.Individuals is

   function Full_Name
     (Individual : Accord.Individual.Individual_Class)
      return String;

   function Full_Name_And_Title
     (Individual : Accord.Individual.Individual_Class)
      return String;

   function Ability_Summary
     (Individual : Accord.Individual.Individual_Class)
      return String;

   function Ability_Score
     (Individual : Accord.Individual.Individual_Class;
      Ability    : Accord.Ability.Ability_Class)
      return Natural;

   function Ability_Modifier
     (Individual : Accord.Individual.Individual_Class;
      Ability    : Accord.Ability.Ability_Class)
      return Integer
   is (Concorde.Abilities.Check_Modifier
       (Ability_Score (Individual, Ability)));

   function Check
     (Individual : Accord.Individual.Individual_Class;
      Ability    : Accord.Ability.Ability_Class;
      Difficulty : Positive)
      return Concorde.Abilities.Check_Result
   is (Concorde.Abilities.Check_Ability
         (Score      => Ability_Score (Individual, Ability),
          Difficulty => Difficulty));

   procedure Advance_Skill
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class)
     with Post => Has_Skill (Individual, Skill);

   procedure Advance_Skill
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class;
      Level      : Natural)
     with Post => Has_Skill (Individual, Skill);

   function Has_Skill
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class)
      return Boolean;

   function Current_Level
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class)
      return Natural
     with Pre => Has_Skill (Individual, Skill);

   function Ruling_Bonus
     (Individual : Accord.Individual.Individual_Class;
      Office     : Accord.Office.Office_Class)
      return Integer;

   procedure Log
     (Individual : Accord.Individual.Individual_Class;
      Message    : String);

end Concorde.Individuals;
