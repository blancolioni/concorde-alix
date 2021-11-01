with Ada.Calendar;
with Ada.Exceptions;

with Ada.Text_IO;

with Reiko.Control;

with Concorde.Options;
with Concorde.Real_Images;

--  with Concorde.UI.Launch;

with Concorde.Calendar;

with Concorde.Logging;
with Concorde.Logs;

with Concorde.Server;
--  with Concorde.Updates;

with Concorde.Colonies.Updates;
with Concorde.Factions.Reports;

with Accord.Db.Database;

--  with Nazar.Main;

procedure Concorde.Driver is

   Database_Open   : Boolean := False;
   Updates_Running : Boolean := False;

begin

   Concorde.Server.Initialize;

   if Concorde.Options.Create then
      Concorde.Logging.Start_Logging ("create");
      Concorde.Server.Create_Scenario;
      Concorde.Logging.Stop_Logging;
      return;
   end if;

   Concorde.Logging.Start_Logging;

   Ada.Text_IO.Put_Line ("opening database ...");

   Accord.Db.Database.Open;
   Database_Open := True;

   Reiko.Control.Start
     (Current_Time =>
        Reiko.Reiko_Time (Concorde.Calendar.To_Real (Concorde.Calendar.Clock)),
      Task_Count   => Concorde.Options.Work_Threads);

   Ada.Text_IO.Put_Line ("starting server ...");

   Concorde.Server.Start;

   Concorde.Calendar.Load_Clock;

   Ada.Text_IO.Put_Line
     ("Start date: " & Concorde.Calendar.Image (Concorde.Calendar.Clock));

   Updates_Running := True;

   if Concorde.Options.Batch_Mode then
      declare
         Update_Days : constant Natural :=
                         Concorde.Options.Update_Count;
         Start_Time  : constant Ada.Calendar.Time :=
                         Ada.Calendar.Clock;
      begin
         if Update_Days > 0 then

            for Day_Index in 1 .. Update_Days loop
               Concorde.Colonies.For_All_Colonies
                 (Concorde.Colonies.Updates.Stability_Check'Access);

               Concorde.Colonies.For_All_Colonies
                 (Concorde.Colonies.Updates.Pay_Maintenance'Access);

               Concorde.Colonies.For_All_Colonies
                 (Concorde.Colonies.Updates.Collect_Taxes'Access);
            end loop;

            Concorde.Factions.For_All_Factions
              (Concorde.Factions.Reports.Put_Faction_Summary'Access);

            declare
               use Ada.Calendar;
               Elapsed_Time : constant Duration :=
                                Clock - Start_Time;
            begin
               Ada.Text_IO.Put_Line
                 ("Updated" & Update_Days'Image
                  & " day"
                  & (if Update_Days = 1 then "" else "s")
                  & " in "
                  & Concorde.Real_Images.Approximate_Image
                    (Real (Elapsed_Time))
                  & "s");
            end;
         end if;

      end;

   else

      null;

      --  declare
      --     UI : constant Concorde.UI.UI_Type :=
      --       Concorde.UI.Launch.Get_UI (Concorde.Options.User_Interface);
      --  begin
      --     UI.Start;
      --
      --     Ada.Text_IO.Put_Line ("Stopping ...");
      --
      --  end;

   end if;

   Updates_Running := False;
   Reiko.Control.Stop;

   Ada.Text_IO.Put_Line
     ("Stop date: " & Concorde.Calendar.Image (Concorde.Calendar.Clock));

   Concorde.Server.Stop;

   Ada.Text_IO.Put_Line ("Closing database");
   Accord.Db.Database.Close;
   Database_Open := False;

   Concorde.Logs.Flush_Logs (True);

   Ada.Text_IO.Put_Line ("exit");

   if Concorde.Options.Detailed_Logging then
      Concorde.Logging.Stop_Logging;
   end if;

exception

   when E : others =>
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "Caught exception: "
         & Ada.Exceptions.Exception_Message (E));
      if Updates_Running then
         Reiko.Control.Stop;
      end if;
      if Database_Open then
         Accord.Db.Database.Close;
      end if;
      Concorde.Logs.Flush_Logs (True);
      Concorde.Logging.Stop_Logging;
      raise;

end Concorde.Driver;
