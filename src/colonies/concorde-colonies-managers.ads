with Accord.Managed;

with Accord.Db;

with Concorde.Managers;

package Concorde.Colonies.Managers is

   type Root_Colony_Manager is
     abstract new Concorde.Managers.Root_Concorde_Manager
   with private;

   function Create_Monthly_Update
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
     with Pre => Accord.Db."=" (Managed.Top_Record, Accord.Db.R_Colony);

   function Create_Daily_Update
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
     with Pre => Accord.Db."=" (Managed.Top_Record, Accord.Db.R_Colony);

private

   type Root_Colony_Manager is
     abstract new Concorde.Managers.Root_Concorde_Manager with
      record
         Colony : Accord.Colony.Colony_Handle;
      end record;

end Concorde.Colonies.Managers;
