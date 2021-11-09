with Concorde.Agents;
with Concorde.Attributes;
with Concorde.Calendar;
with Concorde.Colonies.Updates;
with Concorde.Commodities;
with Concorde.Modules.Updates;
with Concorde.Stock;

with Accord.Colony_Sector;
with Accord.Colony_Sector_Module;
with Accord.Commodity;

package body Concorde.Colonies.Managers is

   type Colony_Daily_Manager is new Root_Colony_Manager with
      record
         null;
      end record;

   overriding function Name
     (Manager : Colony_Daily_Manager)
      return String;

   overriding procedure Execute
     (Manager : in out Colony_Daily_Manager);

   type Colony_Monthly_Manager is new Root_Colony_Manager with
      record
         null;
      end record;

   overriding function Name
     (Manager : Colony_Monthly_Manager)
      return String;

   overriding procedure Execute
     (Manager : in out Colony_Monthly_Manager);

   -------------------------
   -- Create_Daily_Update --
   -------------------------

   function Create_Daily_Update
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
   is
      Manager : constant Colony_Daily_Manager :=
                  Colony_Daily_Manager'
                    (Concorde.Managers.Root_Concorde_Manager with
                     Colony => Accord.Colony.Get_From_Managed (Managed));
   begin
      return Manager;
   end Create_Daily_Update;

   ---------------------------
   -- Create_Monthly_Update --
   ---------------------------

   function Create_Monthly_Update
     (Managed : Accord.Managed.Managed_Class)
      return Concorde.Managers.Root_Concorde_Manager'Class
   is
      Manager : constant Colony_Monthly_Manager :=
                  Colony_Monthly_Manager'
                    (Concorde.Managers.Root_Concorde_Manager with
                     Colony => Accord.Colony.Get_From_Managed (Managed));
   begin
      return Manager;
   end Create_Monthly_Update;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Manager : in out Colony_Daily_Manager) is
      Colony : Accord.Colony.Colony_Handle renames Manager.Colony;
   begin
      for Sector of Accord.Colony_Sector.Select_By_Colony (Colony) loop
         for Sector_Module of
           Accord.Colony_Sector_Module.Select_By_Colony_Sector (Sector)
         loop
            Concorde.Modules.Updates.Daily_Update
              (Module => Sector_Module.Module,
               Sector => Sector.World_Sector,
               Stock  => Colony);
         end loop;
      end loop;

      declare
         use Concorde.Quantities;
         Food : constant Accord.Commodity.Commodity_Class :=
                  Concorde.Commodities.Food;
         Available : constant Quantity_Type :=
                       Concorde.Stock.Quantity (Colony, Food);
         Required  : constant Quantity_Type :=
                       Scale (Population (Colony), 0.1);
      begin
         Log (Colony,
              "food: available " & Show (Available)
              & "; required " & Show (Required));
         if Available < Required then
            declare
               Current_Unrest : constant Natural :=
                                  Concorde.Attributes.Get (Colony, "unrest");
               Minimum_Unrest : constant Natural :=
                                  Natural (To_Real (Required - Available)) + 1;
            begin
               if Current_Unrest < Minimum_Unrest then
                  Concorde.Attributes.Increase
                    (Colony, "unrest",
                     Minimum_Unrest - Current_Unrest);
                  Log (Colony,
                       "base unrest changed to"
                       & Concorde.Attributes.Get (Colony, "unrest")'Image);
               end if;
            end;
         end if;

         if Available > Zero then
            Concorde.Stock.Remove (Colony, Food, Min (Available, Required));
         end if;
      end;
      Concorde.Stock.Save_History (Colony);
      Manager.Update_With_Delay (Concorde.Calendar.Days (1));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute (Manager : in out Colony_Monthly_Manager) is
      Colony : Accord.Colony.Colony_Handle renames Manager.Colony;
   begin
      Updates.Stability_Check (Colony);
      Updates.Pay_Maintenance (Colony);
      Updates.Collect_Taxes (Colony);
      Log (Colony,
           "cash: "
           & Concorde.Money.Show
             (Concorde.Agents.Cash (Colony.Faction)));

      for Commodity of
        Accord.Commodity.Scan_By_Tag
      loop
         declare
            use Concorde.Quantities;
            Minimum   : constant Quantity_Type :=
                          Minimum_Required (Colony, Commodity);
            Available : constant Quantity_Type :=
                          Concorde.Stock.Quantity
                            (Colony, Commodity);
            Requested : constant Quantity_Type :=
                          Current_Request (Colony, Commodity);
         begin
            if Available + Requested < Minimum then
               Request (Colony, Commodity, Minimum - Available - Requested,
                        Concorde.Money.Adjust_Price
                          (Commodity.Base_Price, 2.0));
            end if;
         end;
      end loop;

      Manager.Update_With_Delay (Concorde.Calendar.Days (30));
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name (Manager : Colony_Daily_Manager) return String is
   begin
      return "daily: "
        & Manager.Colony.Faction.Name
        & " colony on "
        & Manager.Colony.World.Name;
   end Name;

   ----------
   -- Name --
   ----------

   overriding function Name (Manager : Colony_Monthly_Manager) return String is
   begin
      return "monthly: "
        & Manager.Colony.Faction.Name
        & " colony on "
        & Manager.Colony.World.Name;
   end Name;

end Concorde.Colonies.Managers;
