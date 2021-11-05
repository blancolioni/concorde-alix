with Ada.Calendar;
with Ada.Exceptions;

with Ada.Text_IO;

with WL.Processes;

with Reiko.Control;

with Concorde.Options;
with Concorde.Real_Images;

--  with Concorde.UI.Launch;

with Concorde.Calendar;

with Concorde.Logging;
with Concorde.Logs;
with Concorde.Factions.Reports;

with Concorde.Managers;

with Concorde.Server;
--  with Concorde.Updates;

with Accord.Db.Database;

with Accord.Faction;

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

   Ada.Text_IO.Put_Line ("starting server ...");

   Concorde.Server.Start;

   Concorde.Calendar.Load_Clock;

   Ada.Text_IO.Put_Line
     ("Start date: " & Concorde.Calendar.Image (Concorde.Calendar.Clock));

   Reiko.Control.Start
     (Current_Time =>
        Reiko.Reiko_Time
          (Concorde.Calendar.To_Days (Concorde.Calendar.Clock)),
      Task_Count   => Natural'Max (Concorde.Options.Work_Threads, 1));

   Updates_Running := True;

   Concorde.Managers.Start_Managers;

   if Concorde.Options.Batch_Mode then
      declare
         Process      : WL.Processes.Process_Type;
         Day_Count    : constant Positive :=
                          Natural'Max (Concorde.Options.Update_Count, 1);
         Reiko_Minute : constant := 1.0 / 24.0 / 60.0;
         Start        : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin

         Ada.Text_IO.Put_Line ("tasks:" & Concorde.Options.Work_Threads'Image);
         Ada.Text_IO.Put_Line ("days: " & Concorde.Options.Update_Count'Image);

         Process.Start_Bar
           (Name            => "Updating",
            Finish          => Day_Count,
            With_Percentage => False,
            Bar_Length      => 40,
            Tick_Size       => 1);

         for I in 1 .. Day_Count loop
            for J in 1 .. 24 loop
               for K in 1 .. 60 loop
                  Concorde.Calendar.Advance (60.0);
                  Reiko.Control.Advance (Reiko_Minute);
               end loop;
            end loop;
            Process.Tick;
         end loop;
         Process.Finish;

         declare
            use Ada.Calendar;
            Elapsed : constant Duration := Clock - Start;
         begin
            Ada.Text_IO.Put_Line
              ("Advanced" & Day_Count'Image & " days in "
               & Concorde.Real_Images.Approximate_Image
                 (Real (Elapsed))
               & "s");
         end;

      end;

      for Faction of Accord.Faction.Scan_By_Top_Record loop
         Concorde.Factions.Reports.Put_Faction_Summary (Faction);
      end loop;

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

   Concorde.Logging.Stop_Logging;

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
