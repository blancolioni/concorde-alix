with WL.String_Maps;

with Concorde.Installations.Factories;
with Concorde.Installations.Farms;

package body Concorde.Installations.Update is

   type Installation_Updater is access
     procedure (Installation : Installation_Class);

   package Installation_Updater_Maps is
     new WL.String_Maps (Installation_Updater);

   Installation_Updater_Map : Installation_Updater_Maps.Map;

   procedure Update_Installation
     (Installation : Installation_Class);

   -------------------------
   -- Update_Installation --
   -------------------------

   procedure Update_Installation
     (Installation : Installation_Class)
   is
   begin
      if Installation_Updater_Map.Contains
        (Installation.Manager)
      then
         Installation_Updater_Map
           .Element (Installation.Manager) (Installation);
      else
         Log (Installation, "no such manager: " & Installation.Manager);
      end if;
   end Update_Installation;

   --------------------------
   -- Update_Installations --
   --------------------------

   procedure Update_Installations is
   begin
      for Installation of Accord.Installation.Scan_By_Top_Record loop
         Update_Installation (Installation);
      end loop;
   end Update_Installations;

begin
   Installation_Updater_Map.Insert
     ("default-factory", Factories.Manage_Factory'Access);
   Installation_Updater_Map.Insert
     ("default-farm", Farms.Manage_Farm'Access);
end Concorde.Installations.Update;
