with Accord.Managed;

with Accord.Db;

with Concorde.Managers;

package Concorde.Ships.Managers is

   type Root_Ship_Manager is
     abstract new Concorde.Managers.Root_Concorde_Manager
   with private;

   procedure Activate
     (Manager : in out Root_Ship_Manager)
   is abstract;

   procedure Set_Destination
     (Manager     : in out Root_Ship_Manager'Class;
      Destination : Accord.World.World_Class);

   function Create_Default_Trader
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
     with Pre => Accord.Db."=" (Managed.Top_Record, Accord.Db.R_Ship);

private

   type Root_Ship_Manager is
     abstract new Concorde.Managers.Root_Concorde_Manager with
      record
         Ship        : Accord.Ship.Ship_Handle;
         Destination : Accord.World.World_Handle;
      end record;

   overriding procedure Execute
     (Manager : in out Root_Ship_Manager);

   procedure Start_Activation
     (Manager : in out Root_Ship_Manager'Class);

   procedure Move_To_Jump_Point
     (Manager : in out Root_Ship_Manager'Class);

   procedure Start_Jump
     (Manager : in out Root_Ship_Manager'Class);

   procedure Move_To_World
     (Manager : in out Root_Ship_Manager'Class);

   procedure Arrive_At_World
     (Manager : in out Root_Ship_Manager'Class);

   procedure Idle
     (Manager : in out Root_Ship_Manager'Class);

end Concorde.Ships.Managers;
