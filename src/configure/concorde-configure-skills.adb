with Tropos.Reader;

with Accord.Skill;

package body Concorde.Configure.Skills is

   ----------------------
   -- Configure_Skills --
   ----------------------

   procedure Configure_Skills (Scenario_Name : String) is
   begin
      for Skill_Config of
        Tropos.Reader.Read_Config
          (Scenario_File (Scenario_Name, "individuals", "skills.txt"))
      loop
         Accord.Skill.Create (Skill_Config.Config_Name);
      end loop;

   end Configure_Skills;

end Concorde.Configure.Skills;
