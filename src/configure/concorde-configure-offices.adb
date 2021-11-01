with Tropos.Reader;

with Concorde.Configure.Attributes;

with Accord.Office;
with Accord.Office_Ability;
with Accord.Office_Skill;
with Accord.Office_Vacancy;

with Accord.Ability;
with Accord.Skill;

package body Concorde.Configure.Offices is

   procedure Configure_Office (Office_Config : Tropos.Configuration);

   ----------------------
   -- Configure_Office --
   ----------------------

   procedure Configure_Office (Office_Config : Tropos.Configuration) is
      Attribute_Config : constant Tropos.Configuration :=
                           Office_Config.Child ("attributes");
      Vacancy_Config   : constant Tropos.Configuration :=
                           Office_Config.Child ("vacancy");

      Office           : constant Accord.Office.Office_Handle :=
                           Accord.Office.Create
                             (Tag        => Office_Config.Config_Name,
                              Rank       => Office_Config.Get ("rank"));

   begin

      Concorde.Configure.Attributes.Configure_Has_Attributes
        (Has_Attributes    => Office,
         Attributes_Config => Attribute_Config);

      for Skill_Config of Office_Config.Child ("skills") loop
         Accord.Office_Skill.Create
           (Office => Office,
            Skill  => Accord.Skill.Get_By_Tag (Skill_Config.Config_Name));
      end loop;

      for Ability_Config of Office_Config.Child ("abilities") loop
         Accord.Office_Ability.Create
           (Office => Office,
            Ability => Accord.Ability.Get_By_Tag (Ability_Config.Config_Name));
      end loop;

      declare
         Vacancy : constant Accord.Office_Vacancy.Office_Vacancy_Handle :=
                     Accord.Office_Vacancy.Create
                       (Office     => Office);
      begin
         Concorde.Configure.Attributes.Configure_Has_Attributes
           (Has_Attributes    => Vacancy,
            Attributes_Config => Vacancy_Config);
      end;

   end Configure_Office;

   -----------------------
   -- Configure_Offices --
   -----------------------

   procedure Configure_Offices (Scenario_Name : String) is
      Path : constant String :=
               Scenario_Directory
                 (Scenario_Name  => Scenario_Name,
                  Directory_Name => "offices");
   begin
      Tropos.Reader.Read_Config
        (Path      => Path,
         Extension => "office",
         Configure => Configure_Office'Access);
   end Configure_Offices;

end Concorde.Configure.Offices;
