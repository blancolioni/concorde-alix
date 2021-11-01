with Ada.Containers.Doubly_Linked_Lists;

with Concorde.Calendar;
with Concorde.Updates.Events;

with Concorde.Agents;
with Concorde.Pops;

with Accord.Employer;
with Accord.Pop;

package body Concorde.Employment is

   Employment_Updates_Started : Boolean := False;

   type Employment_Contracts_Update is
     new Concorde.Updates.Root_Update_Type with null record;

   overriding procedure Activate
     (Item : Employment_Contracts_Update);

   type Contract_Record is
      record
         Employer : Accord.Agent.Agent_Class;
         Employee : Accord.Agent.Agent_Class;
         Quantity : Concorde.Quantities.Quantity_Type;
         Salary   : Concorde.Money.Price_Type;
      end record;

   package Contract_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Contract_Record);

   Current : Contract_Lists.List;

   procedure Execute
     (Employer_Agent : Accord.Agent.Agent_Class;
      Employee_Agent : Accord.Agent.Agent_Class;
      Quantity       : Concorde.Quantities.Quantity_Type;
      Salary         : Concorde.Money.Price_Type);

   --------------
   -- Activate --
   --------------

   overriding procedure Activate
     (Item : Employment_Contracts_Update)
   is
   begin
      for Contract of Current loop
         Execute (Contract.Employer, Contract.Employee,
                  Contract.Quantity, Contract.Salary);
      end loop;
      Current.Clear;
      Concorde.Updates.Events.Update_With_Delay
        (Concorde.Calendar.Days (1.0),
         Item);
   end Activate;

   --------------------------------
   -- Create_Employment_Contract --
   --------------------------------

   procedure Create_Employment_Contract
     (Employer : Accord.Agent.Agent_Class;
      Employee : Accord.Agent.Agent_Class;
      Quantity : Concorde.Quantities.Quantity_Type;
      Salary   : Concorde.Money.Price_Type)
   is
   begin
      Current.Append ((Employer, Employee, Quantity, Salary));
      if not Employment_Updates_Started then
         declare
            use type Concorde.Calendar.Time;
            Update : Employment_Contracts_Update;
         begin
            Concorde.Updates.Events.Update_At
              (Clock  =>
                 Concorde.Calendar.Clock + Concorde.Calendar.Days (1.0),
               Update => Update);
            Employment_Updates_Started := True;
         end;
      end if;
   end Create_Employment_Contract;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Employer_Agent : Accord.Agent.Agent_Class;
      Employee_Agent : Accord.Agent.Agent_Class;
      Quantity       : Concorde.Quantities.Quantity_Type;
      Salary         : Concorde.Money.Price_Type)
   is
      Employer : constant Accord.Employer_Reference :=
                   Accord.Employer.Get_Employer (Employer_Agent)
                   .Get_Employer_Reference;
      Pop      : constant Accord.Pop.Pop_Type :=
        Accord.Pop.Get_Pop (Employee_Agent);
   begin

      if Accord.Pop.Is_Pop_Group_Employer (Pop.Pop_Group, Employer) then
         declare
            Employed : constant Accord.Pop.Pop_Type :=
                         Accord.Pop.Get_By_Pop_Group_Employer
                           (Pop.Pop_Group, Employer);
         begin
            Concorde.Pops.Move_Pops (Pop, Employed, Quantity);
            Employed.Set_Salary (Salary);
            Concorde.Agents.Log_Agent
              (Employed.Get_Agent_Reference,
               "executing employment contract: quantity "
               & Concorde.Quantities.Show (Quantity)
               & "; salary "
               & Concorde.Money.Show (Salary)
               & "; employer"
               & Accord.To_String (Employer));
         end;
      else
         declare
            Employed : constant Accord.Pop.Pop_Type :=
                         Concorde.Pops.New_Empty_Pop
                           (Faction => Pop.Faction,
                            Group   => Pop.Pop_Group,
                            World   => Pop.World,
                            Sector  => Pop.World_Sector);
         begin
            Concorde.Pops.Move_Pops (Pop, Employed, Quantity);
            Employed.Set_Salary (Salary);
            Employed.Set_Employer (Employer);
            Concorde.Agents.Log_Agent
              (Employed.Get_Agent_Reference,
               "executing employment contract: quantity "
               & Concorde.Quantities.Show (Quantity)
               & "; salary "
               & Concorde.Money.Show (Salary)
               & "; employer"
               & Accord.To_String (Employer));
         end;
      end if;
   end Execute;

end Concorde.Employment;
