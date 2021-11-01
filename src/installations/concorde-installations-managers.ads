with Accord.Managed;
with Concorde.Managers;

package Concorde.Installations.Managers is

   function Create_Default_Manager
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

   function Create_Outpost_Manager
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

   function Create_Service_Manager
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

   function Create_Farm_Manager
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

   function Create_Factory_Manager
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Manager_Type'Class;

end Concorde.Installations.Managers;
