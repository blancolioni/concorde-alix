with Ada.Text_IO;

with Tropos.Reader;

with Accord.Ability;
with Accord.Advancement_Table;
with Accord.Assignment;
with Accord.Assignment_Rank;
with Accord.Career;
with Accord.Career_Benefit;
with Accord.Career_Mishap;
with Accord.Event;
with Accord.Skill;

with Accord.Db;

package body Concorde.Configure.Careers is

   procedure Configure_Career
     (Career_Config : Tropos.Configuration);

   procedure Configure_Assignment
     (Career            : Accord.Career.Career_Class;
      Assignment_Config : Tropos.Configuration);

   procedure Get_Ability_Score
     (Config  : Tropos.Configuration;
      Ability : out Accord.Ability.Ability_Handle;
      Score   : out Natural);

   procedure Configure_Advancement
     (Config          : Tropos.Configuration;
      Advance_Cash    : out Concorde.Money.Money_Type;
      Advance_Ability : out Accord.Ability.Ability_Handle;
      Advance_Skill   : out Accord.Skill.Skill_Handle;
      Advance_Level   : out Integer);

   procedure Configure_Benefits
     (Career : Accord.Career.Career_Class;
      Config : Tropos.Configuration);

   procedure Configure_Advancement_Table
     (Career       : Accord.Career.Career_Class;
      Assignment   : Accord.Assignment.Assignment_Class;
      Training     : Accord.Db.Skill_Training_Type;
      Table_Config : Tropos.Configuration);

   procedure Configure_Rank_Table
     (Assignment   : Accord.Assignment.Assignment_Class;
      Table_Config : Tropos.Configuration);

   function Get_Skill_Training
     (Tag : String)
      return Accord.Db.Skill_Training_Type
   is (if Tag = "personal-development"
       then Accord.Db.Personal_Development
       elsif Tag = "service-skills"
       then Accord.Db.Service_Skills
       elsif Tag = "advanced-education"
       then Accord.Db.Advanced_Skills
       elsif Tag = "advancement-table"
       then Accord.Db.Assignment_Training
       elsif Tag = "commission"
       then Accord.Db.Commission_Skills
       else raise Constraint_Error with
         "undefined skill training tag: " & Tag);

   ---------------------------
   -- Configure_Advancement --
   ---------------------------

   procedure Configure_Advancement
     (Config          : Tropos.Configuration;
      Advance_Cash    : out Concorde.Money.Money_Type;
      Advance_Ability : out Accord.Ability.Ability_Handle;
      Advance_Skill   : out Accord.Skill.Skill_Handle;
      Advance_Level   : out Integer)
   is
      use Accord.Ability;
      use Accord.Skill;
   begin
      Advance_Cash := Concorde.Money.Zero;
      Advance_Ability := Empty_Handle;
      Advance_Skill := Empty_Handle;
      Advance_Level := 0;
      for Item_Config of Config loop
         declare
            Tag : constant String := Item_Config.Config_Name;
            Cash : constant Boolean := Tag = "cash";
            Ability : constant Ability_Handle :=
                        Accord.Ability.Get_By_Tag (Tag);
            Skill   : constant Skill_Handle :=
                        Accord.Skill.Get_By_Tag (Tag);
         begin
            if Cash then
               Advance_Cash :=
                 Concorde.Money.To_Money
                   (Real (Long_Float'(Item_Config.Value)));
            elsif Ability.Has_Element then
               Advance_Ability := Ability;
            elsif Skill.Has_Element then
               Advance_Skill := Skill;
               Advance_Level :=
                 (if Item_Config.Child_Count = 0
                  then -1
                  else Item_Config.Value);
            end if;
         end;
      end loop;

   end Configure_Advancement;

   ---------------------------------
   -- Configure_Advancement_Table --
   ---------------------------------

   procedure Configure_Advancement_Table
     (Career       : Accord.Career.Career_Class;
      Assignment   : Accord.Assignment.Assignment_Class;
      Training     : Accord.Db.Skill_Training_Type;
      Table_Config : Tropos.Configuration)
   is
      DR : Positive := 1;
   begin
      for Config of Table_Config loop
         declare
            Advance_Cash    : Concorde.Money.Money_Type;
            Advance_Ability : Accord.Ability.Ability_Handle;
            Advance_Skill   : Accord.Skill.Skill_Handle;
            Advance_Level   : Integer;
         begin
            Configure_Advancement
              (Config, Advance_Cash, Advance_Ability,
               Advance_Skill, Advance_Level);

            Accord.Advancement_Table.Create
              (Ability     => Advance_Ability,
               Cash        => Advance_Cash,
               Skill       => Advance_Skill,
               Skill_Level => Advance_Level,
               Table       => Training,
               Career      => Career,
               Assignment  => Assignment,
               Dr          => DR);
            DR := DR + 1;
         end;
      end loop;
   end Configure_Advancement_Table;

   --------------------------
   -- Configure_Assignment --
   --------------------------

   procedure Configure_Assignment
     (Career            : Accord.Career.Career_Class;
      Assignment_Config : Tropos.Configuration)
   is
      Survival_Ability : Accord.Ability.Ability_Handle;
      Survival_Check   : Natural;
      Advance_Ability  : Accord.Ability.Ability_Handle;
      Advance_Check    : Natural;

   begin

      Get_Ability_Score
        (Assignment_Config.Child ("survival"),
         Survival_Ability, Survival_Check);

      Get_Ability_Score
        (Assignment_Config.Child ("advance"),
         Advance_Ability, Advance_Check);

      declare
         Assignment : constant Accord.Assignment.Assignment_Handle :=
                        Accord.Assignment.Create
                          (Tag              => Assignment_Config.Config_Name,
                           Career           => Career,
                           Survival_Ability => Survival_Ability,
                           Survival_Check   => Survival_Check,
                           Advance_Ability  => Advance_Ability,
                           Advance_Check    => Advance_Check);
      begin
         Configure_Advancement_Table
           (Career       => Career,
            Assignment   => Assignment,
            Training     => Accord.Db.Assignment_Training,
            Table_Config => Assignment_Config.Child ("advancement-table"));
         Configure_Rank_Table
           (Assignment   => Assignment,
            Table_Config => Assignment_Config.Child ("ranks"));
      end;

   end Configure_Assignment;

   ------------------------
   -- Configure_Benefits --
   ------------------------

   procedure Configure_Benefits
     (Career : Accord.Career.Career_Class;
      Config : Tropos.Configuration)
   is
      Benefit_Index : Positive := 1;
   begin
      for Item_Config of Config loop
         declare
            Advance_Cash    : Concorde.Money.Money_Type;
            Advance_Ability : Accord.Ability.Ability_Handle;
            Advance_Skill   : Accord.Skill.Skill_Handle;
            Advance_Level   : Integer;
         begin
            Configure_Advancement
              (Item_Config, Advance_Cash, Advance_Ability,
               Advance_Skill, Advance_Level);

            Accord.Career_Benefit.Create
              (Ability     => Advance_Ability,
               Cash        => Advance_Cash,
               Skill       => Advance_Skill,
               Skill_Level => Advance_Level,
               Career      => Career,
               Index       => Benefit_Index);
            Benefit_Index := Benefit_Index + 1;
         end;
      end loop;
   end Configure_Benefits;

   ----------------------
   -- Configure_Career --
   ----------------------

   procedure Configure_Career
     (Career_Config : Tropos.Configuration)
   is
      Qualification    : Accord.Ability.Ability_Handle;
      Score            : Natural;
      Advanced_Ability : Accord.Ability.Ability_Handle;
      Advanced_Check   : Natural;
   begin
      Get_Ability_Score
        (Career_Config.Child ("qualification"),
         Qualification, Score);
      Get_Ability_Score
        (Career_Config.Child ("advanced-education"),
         Advanced_Ability, Advanced_Check);

      declare
         Career : constant Accord.Career.Career_Class :=
                    Accord.Career.Create
                      (Tag              => Career_Config.Config_Name,
                       Qualification    => Qualification,
                       Check            => Score,
                       Advanced_Ability => Advanced_Ability,
                       Advanced_Check   => Advanced_Check,
                       Has_Commission   =>
                         Career_Config.Get ("has-commission"),
                       Basic_Training   =>
                         Get_Skill_Training
                           (Career_Config.Get
                                ("basic-training",
                                 "service-skills")));

      begin
         Configure_Benefits (Career, Career_Config.Child ("benefits"));

         for Advancement_Config of
           Career_Config.Child ("advancement-tables")
         loop
            declare
               Training : constant Accord.Db.Skill_Training_Type :=
                            Get_Skill_Training
                              (Advancement_Config.Config_Name);
            begin
               Configure_Advancement_Table
                 (Career, Accord.Assignment.Empty_Handle, Training,
                  Advancement_Config);
            end;
         end loop;

         for Assignment_Config of Career_Config.Child ("assignment") loop
            Configure_Assignment (Career, Assignment_Config);
         end loop;

         for Mishap_Config of Career_Config.Child ("mishaps") loop
            declare
               use Accord.Event;
               Event : constant Event_Handle :=
                         Accord.Event.Get_By_Tag
                           (Mishap_Config.Value);
            begin
               if not Event.Has_Element then
                  Ada.Text_IO.Put_Line
                    ("undefined event: " & Mishap_Config.Value);
               else
                  Accord.Career_Mishap.Create
                    (Career => Career,
                     Event  => Event);
               end if;
            end;
         end loop;
      end;

   end Configure_Career;

   -----------------------
   -- Configure_Careers --
   -----------------------

   procedure Configure_Careers (Scenario_Name : String) is
      Path : constant String :=
               Scenario_Directory
                 (Scenario_Name  => Scenario_Name,
                  Directory_Name => "careers");
   begin
      Tropos.Reader.Read_Config
        (Path      => Path,
         Extension => "career",
         Configure => Configure_Career'Access);
   end Configure_Careers;

   --------------------------
   -- Configure_Rank_Table --
   --------------------------

   procedure Configure_Rank_Table
     (Assignment   : Accord.Assignment.Assignment_Class;
      Table_Config : Tropos.Configuration)
   is
      Rank : Natural := 0;
   begin
      for Rank_Config of Table_Config loop
         declare
            Advance_Cash    : Concorde.Money.Money_Type;
            Advance_Ability : Accord.Ability.Ability_Handle;
            Advance_Skill   : Accord.Skill.Skill_Handle;
            Advance_Level   : Integer;
         begin
            Configure_Advancement
              (Rank_Config, Advance_Cash, Advance_Ability,
               Advance_Skill, Advance_Level);

            Accord.Assignment_Rank.Create
              (Ability     => Advance_Ability,
               Cash        => Advance_Cash,
               Skill       => Advance_Skill,
               Skill_Level => Advance_Level,
               Tag         => Rank_Config.Config_Name,
               Assignment  => Assignment,
               Rank_Index  => Rank);
            Rank := Rank + 1;
         end;
      end loop;
   end Configure_Rank_Table;

   -----------------------
   -- Get_Ability_Score --
   -----------------------

   procedure Get_Ability_Score
     (Config  : Tropos.Configuration;
      Ability : out Accord.Ability.Ability_Handle;
      Score   : out Natural)
   is
   begin
      if Config.Child_Count = 0 then
         Ability := Accord.Ability.Empty_Handle;
         Score := 0;
      elsif Config.Contains ("ability") then
         Ability :=
           Accord.Ability.Get_By_Tag
             (Config.Get ("ability"));
         Score := Config.Get ("score", 99);
      else
         declare
            Child : constant Tropos.Configuration :=
                      Config.Child (1);
         begin
            Ability :=
              Accord.Ability.Get_By_Tag
                (Child.Config_Name);
            Score := Child.Value;
         end;
      end if;
   end Get_Ability_Score;

end Concorde.Configure.Careers;
