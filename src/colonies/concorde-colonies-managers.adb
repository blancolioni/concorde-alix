with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Colonies.Updates;
with Concorde.Modules.Updates;

with Accord.Colony_Sector;
with Accord.Colony_Sector_Module;

package body Concorde.Colonies.Managers is

   type Colony_Daily_Manager is new Root_Colony_Manager with
      record
         null;
      end record;

   overriding function Name
     (Manager : Colony_Daily_Manager)
      return String;

   overriding procedure Execute
     (Manager : in out Colony_Daily_Manager);

   type Colony_Monthly_Manager is new Root_Colony_Manager with
      record
         null;
      end record;

   overriding function Name
     (Manager : Colony_Monthly_Manager)
      return String;

   overriding procedure Execute
     (Manager : in out Colony_Monthly_Manager);

   -------------------------
   -- Create_Daily_Update --
   -------------------------

   function Create_Daily_Update
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
   is
      Manager : constant Colony_Daily_Manager :=
                  Colony_Daily_Manager'
                    (Concorde.Managers.Root_Concorde_Manager with
                     Colony => Accord.Colony.Get_From_Managed (Managed));
   begin
      return Manager;
   end Create_Daily_Update;

   ---------------------------
   -- Create_Monthly_Update --
   ---------------------------

   function Create_Monthly_Update
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
   is
      Manager : constant Colony_Monthly_Manager :=
                  Colony_Monthly_Manager'
                    (Concorde.Managers.Root_Concorde_Manager with
                     Colony => Accord.Colony.Get_From_Managed (Managed));
   begin
      return Manager;
   end Create_Monthly_Update;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Manager : in out Colony_Daily_Manager) is
   begin
      for Sector of Accord.Colony_Sector.Select_By_Colony (Manager.Colony) loop
         for Sector_Module of
           Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
         loop
            Concorde.Modules.Updates.Daily_Update
              (Module => Sector_Module.Module,
               Sector => Sector.World_Sector,
               Stock  => Manager.Colony);
         end loop;
      end loop;
      Manager.Update_With_Delay (Concorde.Calendar.Days (1));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Manager : in out Colony_Monthly_Manager) is
   begin
      Updates.Stability_Check (Manager.Colony);
      Updates.Pay_Maintenance (Manager.Colony);
      Updates.Collect_Taxes (Manager.Colony);
      Log (Manager.Colony,
           "cash: "
           & Concorde.Money.Show
             (Concorde.Agents.Cash (Manager.Colony.Faction)));

      Manager.Update_With_Delay (Concorde.Calendar.Days (30));
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name (Manager : Colony_Daily_Manager) return String is
   begin
      return "daily: "
        & Manager.Colony.Faction.Name
        & " colony on "
        & Manager.Colony.World.Name;
   end Name;

   ----------
   -- Name --
   ----------

   overriding function Name (Manager : Colony_Monthly_Manager) return String is
   begin
      return "monthly: "
        & Manager.Colony.Faction.Name
        & " colony on "
        & Manager.Colony.World.Name;
   end Name;

end Concorde.Colonies.Managers;
