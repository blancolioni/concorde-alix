private with Accord.Managed;

with Concorde.Calendar;

package Concorde.Managers is

   type Root_Concorde_Manager is abstract tagged private;

   function Name
     (Manager : Root_Concorde_Manager)
      return String
      is abstract;

   procedure Execute
     (Manager : in out Root_Concorde_Manager)
   is abstract;

   procedure Update_With_Delay
     (Manager      : in out Root_Concorde_Manager'Class;
      Update_Delay : Concorde_Duration);

   procedure Update_At
     (Manager      : in out Root_Concorde_Manager'Class;
      Update_Time  : Concorde.Calendar.Time);

   procedure Start_Managers;

private

   type Root_Concorde_Manager is abstract tagged
      record
         Has_Update     : Boolean;
         Next_Update_At : Concorde.Calendar.Time;
      end record;

   type Manager_Launch_Function is access
     function (Managed : Accord.Managed.Managed_Class)
               return Root_Concorde_Manager'Class;

   procedure Add_Manager
     (Name   : String;
      Launch : not null Manager_Launch_Function);

end Concorde.Managers;
