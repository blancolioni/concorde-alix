with Tropos.Reader;

with Concorde.Configure.Attributes;

with Accord.Edict;
with Accord.Edict_Group;

package body Concorde.Configure.Edicts is

   procedure Configure_Edict
     (Edict_Config : Tropos.Configuration);

   ---------------------
   -- Configure_Edict --
   ---------------------

   procedure Configure_Edict
     (Edict_Config : Tropos.Configuration)
   is
      Group : constant Accord.Edict_Group.Edict_Group_Handle :=
                Accord.Edict_Group.Create
                  (Tag => Edict_Config.Config_Name);
      Index  : Positive := 1;
   begin
      for Choice_Config of Edict_Config.Child ("choices") loop

         declare
            function Get (Name : String) return Integer
            is (Choice_Config.Get (Name, 0));

            function Get (Name : String) return Real
            is (Real (Float'(Choice_Config.Get (Name, 0.0))));

            function Get (Name : String) return String
            is (Choice_Config.Get (Name, ""));

            Edict   : constant Accord.Edict.Edict_Handle :=
                        Accord.Edict.Create
                          (Tag                => Choice_Config.Config_Name,
                           Edict_Group        => Group,
                           Group_Index        => Index,
                           Economy_Check      =>
                             Choice_Config.Child ("check").Get ("economy", 0),
                           Extra_Consumption  => Get ("consumption"),
                           Extra_Claims       => Get ("sector-claims"),
                           Revenue            => Get ("revenue"));
         begin

            Concorde.Configure.Attributes.Configure_Has_Attributes
              (Has_Attributes    => Edict,
               Attributes_Config => Choice_Config);
         end;

         Index := Index + 1;

      end loop;

   exception
      when others =>
         raise Constraint_Error with
           "error while configuring edict " & Edict_Config.Config_Name;
   end Configure_Edict;

   ----------------------
   -- Configure_Edicts --
   ----------------------

   procedure Configure_Edicts (Scenario_Name : String) is
      Path : constant String :=
               Scenario_Directory
                 (Scenario_Name  => Scenario_Name,
                  Directory_Name => "edicts");
   begin
      Tropos.Reader.Read_Config
        (Path      => Path,
         Extension => "edict",
         Configure => Configure_Edict'Access);
   end Configure_Edicts;

end Concorde.Configure.Edicts;
