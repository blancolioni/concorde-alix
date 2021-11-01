with Concorde.Calendar;
with Concorde.Markets;
with Concorde.Installations.Update;
with Concorde.Pops.Update;

package body Concorde.Updates is

   ------------
   -- Update --
   ------------

   procedure Update is
   begin
      Concorde.Pops.Update.Update_Pops;
      Concorde.Installations.Update.Update_Installations;
      Concorde.Markets.Update_Markets;
      Concorde.Calendar.Advance (Concorde.Calendar.Days (1));
   end Update;

end Concorde.Updates;
