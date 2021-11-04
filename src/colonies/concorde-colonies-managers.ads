with Accord.Managed;

with Accord.Db;

with Concorde.Managers;

package Concorde.Colonies.Managers is

   type Colony_Manager is
     new Concorde.Managers.Root_Concorde_Manager
   with private;

   overriding function Name
     (Manager : Colony_Manager)
      return String;

   overriding procedure Execute
     (Manager : in out Colony_Manager);

   function Create_Default_Colony_Manager
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
     with Pre => Accord.Db."=" (Managed.Top_Record, Accord.Db.R_Colony);

private

   type Colony_Manager is
     new Concorde.Managers.Root_Concorde_Manager with
      record
         Colony : Accord.Colony.Colony_Handle;
      end record;

end Concorde.Colonies.Managers;
