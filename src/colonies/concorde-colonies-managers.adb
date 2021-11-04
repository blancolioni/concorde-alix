with Concorde.Calendar;
with Concorde.Colonies.Updates;

package body Concorde.Colonies.Managers is

   -----------------------------------
   -- Create_Default_Colony_Manager --
   -----------------------------------

   function Create_Default_Colony_Manager
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
   is
      Manager : constant Colony_Manager := Colony_Manager'
        (Concorde.Managers.Root_Concorde_Manager with
         Colony => Accord.Colony.Get_From_Managed (Managed));
   begin
      return Manager;
   end Create_Default_Colony_Manager;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Manager : in out Colony_Manager) is
   begin
      Updates.Stability_Check (Manager.Colony);
      Updates.Pay_Maintenance (Manager.Colony);
      Updates.Collect_Taxes (Manager.Colony);
      Manager.Update_With_Delay (Concorde.Calendar.Days (30));
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name (Manager : Colony_Manager) return String is
   begin
      return Manager.Colony.Faction.Name
        & " colony on "
        & Manager.Colony.World.Name;
   end Name;

end Concorde.Colonies.Managers;
