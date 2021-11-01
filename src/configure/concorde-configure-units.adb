with Tropos.Reader;

with Concorde.Configure.Commodities;

with Accord.Movement;
with Accord.Technology;
with Accord.Unit;

package body Concorde.Configure.Units is

   procedure Configure_Unit
     (Unit_Config : Tropos.Configuration);

   function Get_Movement
     (Tag : String)
      return Accord.Movement.Movement_Handle;

   --------------------
   -- Configure_Unit --
   --------------------

   procedure Configure_Unit
     (Unit_Config : Tropos.Configuration)
   is
      function Get (Name : String) return Real
      is (Real (Float'(Unit_Config.Get (Name, 0.0))));

      Unit : constant Accord.Unit.Unit_Handle :=
        Accord.Unit.Create
          (Tag        => Unit_Config.Config_Name,
           Enabled_By => Accord.Technology.Empty_Handle,
           Movement   =>
             Get_Movement (Unit_Config.Get ("movement-type")),
           Base_Speed => Get ("movement-speed"),
           Armour     => Get ("armour"),
           Attack     => Get ("attack"),
           Discipline => Get ("discipline"),
           Recon      => Get ("recon"),
           Camoflage  => Get ("camoflage"));

   begin
      Concorde.Configure.Commodities.Configure_Constructed
        (Unit, Unit_Config);
      Concorde.Configure.Commodities.Configure_Supplied
        (Unit, Unit_Config);
   end Configure_Unit;

   ---------------------
   -- Configure_Units --
   ---------------------

   procedure Configure_Units (Scenario_Name : String) is
   begin
      for Unit_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory (Scenario_Name, "units"),
           "unit")
      loop
         Configure_Unit (Unit_Config);
      end loop;
   end Configure_Units;

   ------------------
   -- Get_Movement --
   ------------------

   function Get_Movement
     (Tag : String)
      return Accord.Movement.Movement_Handle
   is
      Movement : constant Accord.Movement.Movement_Handle :=
                   Accord.Movement.Get_By_Tag (Tag);
   begin
      if not Movement.Has_Element then
         return Accord.Movement.Create (Tag);
      else
         return Movement;
      end if;
   end Get_Movement;

end Concorde.Configure.Units;
