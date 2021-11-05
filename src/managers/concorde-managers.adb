with Ada.Text_IO;

with WL.String_Maps;

with Reiko.Updates;

with Concorde.Managers.Loader;

package body Concorde.Managers is

   type Manager_Access is access all Root_Concorde_Manager'Class;

   package Manager_Maps is
     new WL.String_Maps (Manager_Launch_Function);

   Manager_Map : Manager_Maps.Map;

   package Active_Manager_Maps is
     new WL.String_Maps (Manager_Access);

   Active_Manager_Map : Active_Manager_Maps.Map;

   type Manager_Update is
     new Reiko.Root_Update_Type with
      record
         Manager : Manager_Access;
      end record;

   overriding function Name
     (Update : Manager_Update)
      return String
   is (Update.Manager.Name);

   overriding procedure Execute (Update : Manager_Update);

   -----------------
   -- Add_Manager --
   -----------------

   procedure Add_Manager
     (Name   : String;
      Launch : not null Manager_Launch_Function)
   is
   begin
      Manager_Map.Insert (Name, Launch);
   end Add_Manager;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Update : Manager_Update) is
   begin
      Update.Manager.Has_Update := False;

      Update.Manager.Execute;

      if Update.Manager.Has_Update then
         Reiko.Updates.Add_Update_At
           (Update    => Update,
            Update_At =>
              Reiko.Reiko_Time
                (Concorde.Calendar.To_Days
                     (Update.Manager.Next_Update_At)));
      end if;
   end Execute;

   --------------------
   -- Start_Managers --
   --------------------

   procedure Start_Managers is
   begin
      Loader.Load_Managers;
      for Managed of Accord.Managed.Scan_By_Top_Record loop
         declare
            Name : constant String := Managed.Manager;
            Launch : constant Manager_Launch_Function :=
                       (if Manager_Map.Contains (Name)
                        then Manager_Map (Name)
                        else null);
         begin
            if Launch = null then
               if Name /= "" then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "no such manager: " & Name);
               end if;
            else
               declare
                  Manager : constant Manager_Access :=
                              new Root_Concorde_Manager'Class'
                                (Launch (Managed));
               begin
                  if Managed.Scheduled then
                     declare
                        Update : constant Manager_Update :=
                                   Manager_Update'
                                     (Reiko.Root_Update_Type with
                                      Manager => Manager);
                     begin
                        Reiko.Updates.Add_Update_At
                          (Update    => Update,
                           Update_At =>
                             Reiko.Reiko_Time
                               (Concorde.Calendar.To_Days
                                    (Managed.Next_Event)));
                     end;
                  end if;
                  Active_Manager_Map.Insert (Managed.Identifier, Manager);
               end;
            end if;
         end;
      end loop;
   end Start_Managers;

   ---------------
   -- Update_At --
   ---------------

   procedure Update_At
     (Manager      : in out Root_Concorde_Manager'Class;
      Update_Time  : Concorde.Calendar.Time)
   is
   begin
      Manager.Next_Update_At := Update_Time;
      Manager.Has_Update := True;
   end Update_At;

   -----------------------
   -- Update_With_Delay --
   -----------------------

   procedure Update_With_Delay
     (Manager      : in out Root_Concorde_Manager'Class;
      Update_Delay : Concorde_Duration)
   is
      use type Concorde.Calendar.Time;
   begin
      Manager.Next_Update_At := Concorde.Calendar.Clock + Update_Delay;
      Manager.Has_Update := True;
   end Update_With_Delay;

end Concorde.Managers;
