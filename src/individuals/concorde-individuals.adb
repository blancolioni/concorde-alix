with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with WL.Localisation;

with Concorde.Logging;

with Accord.Ability_Score;
with Accord.Office_Ability;
with Accord.Office_Skill;
with Accord.Skill_Level;

package body Concorde.Individuals is

   -------------------
   -- Ability_Score --
   -------------------

   function Ability_Score
     (Individual : Accord.Individual.Individual_Class;
      Ability    : Accord.Ability.Ability_Class)
      return Natural
   is
   begin
      return Accord.Ability_Score.Get_By_Ability_Score
        (Individual, Ability)
        .Score;
   end Ability_Score;

   ---------------------
   -- Ability_Summary --
   ---------------------

   function Ability_Summary
     (Individual : Accord.Individual.Individual_Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Ability of Accord.Ability.Scan_By_Top_Record loop
         declare
            use Ada.Strings, Ada.Strings.Fixed;
            use Accord.Ability_Score;
            Score : constant Ability_Score_Handle :=
                      Get_By_Ability_Score
                        (Individual, Ability);
         begin
            Result := Result
              & (if Result = Null_Unbounded_String then "[" else ",")
              & Ability.Tag & ":" & Trim (Score.Score'Image, Left);
         end;
      end loop;
      return To_String (Result & "]");
   end Ability_Summary;

   -------------------
   -- Advance_Skill --
   -------------------

   procedure Advance_Skill
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class)
   is
   begin
      if not Has_Skill (Individual, Skill) then
         Log (Individual,
              "gains "
              & Skill.Tag
              & " at level 0");

         Accord.Skill_Level.Create
           (Individual => Individual,
            Skill      => Skill,
            Level      => 0);
      else
         declare
            use Accord.Skill_Level;
            Skill_Level : constant Skill_Level_Handle :=
                            Get_By_Skill_Level
                              (Individual, Skill);
         begin
            Skill_Level.Update_Skill_Level
              .Set_Level (Current_Level (Individual, Skill) + 1)
              .Done;
         end;

         Log (Individual,
              "raises "
              & Skill.Tag
              & " to level"
              & Natural'Image (Current_Level (Individual, Skill)));
      end if;
   end Advance_Skill;

   -------------------
   -- Advance_Skill --
   -------------------

   procedure Advance_Skill
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class;
      Level      : Natural)
   is
   begin
      if not Has_Skill (Individual, Skill) then
         Log (Individual,
              "gains "
              & Skill.Tag
              & " at level" & Level'Image);

         Accord.Skill_Level.Create
           (Individual => Individual,
            Skill      => Skill,
            Level      => Level);
      elsif Current_Level (Individual, Skill) < Level then
         declare
            use Accord.Skill_Level;
            Skill_Level : constant Skill_Level_Handle :=
                            Get_By_Skill_Level
                              (Individual, Skill);
         begin
            Skill_Level.Update_Skill_Level
              .Set_Level (Level)
              .Done;
         end;

         Log (Individual,
              "raises "
              & Skill.Tag
              & " to level"
              & Level'Image);
      end if;
   end Advance_Skill;

   -------------------
   -- Current_Level --
   -------------------

   function Current_Level
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class)
      return Natural
   is
   begin
      return Accord.Skill_Level.Get_By_Skill_Level
        (Individual, Skill)
        .Level;
   end Current_Level;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (Individual : Accord.Individual.Individual_Class)
      return String
   is
   begin
      return Individual.First_Name & " " & Individual.Last_Name;
   end Full_Name;

   -------------------------
   -- Full_Name_And_Title --
   -------------------------

   function Full_Name_And_Title
     (Individual : Accord.Individual.Individual_Class)
      return String
   is
      Name : constant String := Full_Name (Individual);
      Title : constant String :=
                (if Individual.Office.Has_Element
                 then WL.Localisation.Local_Text (Individual.Office.Tag)
                 elsif Individual.Title /= ""
                 then WL.Localisation.Local_Text (Individual.Title)
                 else "");
   begin
      return (if Title = "" then Name else Title & " " & Name);
   end Full_Name_And_Title;

   ---------------
   -- Has_Skill --
   ---------------

   function Has_Skill
     (Individual : Accord.Individual.Individual_Class;
      Skill      : Accord.Skill.Skill_Class)
      return Boolean
   is
   begin
      return Accord.Skill_Level.Get_By_Skill_Level
        (Individual, Skill)
        .Has_Element;
   end Has_Skill;

   ---------
   -- Log --
   ---------

   procedure Log
     (Individual : Accord.Individual.Individual_Class;
      Message    : String)
   is
      Title : constant String :=
                Individual.Title;
   begin
      Concorde.Logging.Log
        (Actor    =>
           (if Title = "" then "" else Title & " ")
             & Individual.First_Name & " " & Individual.Last_Name,
         Location => Individual.World_Sector.World.Name,
         Category => "career",
         Message  => Message);
   end Log;

   ------------------
   -- Ruling_Bonus --
   ------------------

   function Ruling_Bonus
     (Individual : Accord.Individual.Individual_Class;
      Office     : Accord.Office.Office_Class)
      return Integer
   is
      Bonus : Integer := 0;
   begin
      for Office_Ability of
        Accord.Office_Ability.Select_By_Office (Office)
      loop
         Bonus := Bonus
           + Ability_Modifier  (Individual, Office_Ability.Ability);
      end loop;
      for Office_Skill of
        Accord.Office_Skill.Select_By_Office (Office)
      loop
         if Has_Skill (Individual, Office_Skill.Skill) then
            Bonus := Bonus
              + Concorde.Individuals.Current_Level
              (Individual, Office_Skill.Skill);
         else
            Bonus := Bonus - 1;
         end if;
      end loop;
      return Bonus;
   end Ruling_Bonus;

end Concorde.Individuals;
