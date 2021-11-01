with Ada.Text_IO;

with Nazar.Main;

with Nazar.Views.Console;
with Nazar.Views.Draw;

with Nazar.Controllers.Console;
with Nazar.Controllers.Draw;

with Concorde.UI.Models.Console;
with Concorde.UI.Models.Galaxy;
with Concorde.UI.Models.World;

with Concorde.Paths;

with Accord.Faction;

with Accord.Db;

with Concorde.UI.Models.Current_Cash;
with Concorde.UI.Models.Current_Date;

package body Concorde.UI.Nazar_UI is

   type Root_Nazar_UI is
     new UI_Interface with
      record
         Top         : Nazar.Views.Nazar_View;
         Galaxy      : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
         Terrain     : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
         Moisture    : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
         Elevation   : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
         Temperature : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
         Console     : Nazar.Controllers.Console
           .Nazar_Console_Controller_Record;
      end record;

   overriding procedure Start (UI : in out Root_Nazar_UI);

   overriding procedure Stop (UI : in out Root_Nazar_UI;
                              Message : String);

   type Nazar_UI_Access is access all Root_Nazar_UI'Class;

   ------------------
   -- Get_Nazar_UI --
   ------------------

   function Get_Nazar_UI
     (Creator : Nazar.Builder.Nazar_Creator_Interface'Class) return UI_Type
   is
   begin
      Nazar.Main.Init;

      declare
         Result  : constant Nazar_UI_Access := new Root_Nazar_UI;
         Builder : constant Nazar.Builder.Nazar_Builder :=
                     Nazar.Builder.Nazar_Builder_New
                       (Creator     => Creator,
                        Config_Path =>
                          Concorde.Paths.Config_File ("ui/concorde.config"));
         Faction : constant Accord.Faction.Faction_Handle :=
                     Accord.Faction.First_By_Top_Record
                       (Accord.Db.R_Faction);
         --  Colony  : constant Accord.Colony.Colony_Handle :=
         --              Accord.Colony.First_By_World
         --                (Faction.Capital_World);
      begin
         Result.Top := Builder.Get_View ("Concorde");
         Result.Console.Start_Console
           (Model =>
              Models.Console.Console_Model
                (Default_Scope => "/home"),
            View  =>
              Nazar.Views.Console.Nazar_Console_View
                (Builder.Get_View ("console")));

         Result.Galaxy.Start_Draw
           (Model =>
              Models.Galaxy.Galaxy_Model (Faction),
            View  =>
              Nazar.Views.Draw.Nazar_Draw_View
                (Builder.Get_View ("galaxy")));

         Result.Terrain.Start_Draw
           (Model =>
              Models.World.World_Model
                (Faction, Faction.Capital_World,
                 Concorde.UI.Models.World.Terrain_Display,
                 Show_Owner => True, Show_Wind => True),
            View  =>
              Nazar.Views.Draw.Nazar_Draw_View
                (Builder.Get_View ("world-terrain")));

         Result.Terrain.Start_Draw
           (Model =>
              Models.World.World_Model
                (Faction, Faction.Capital_World,
                 Concorde.UI.Models.World.Moisture_Display, False, False),
            View  =>
              Nazar.Views.Draw.Nazar_Draw_View
                (Builder.Get_View ("world-moisture")));

         Result.Terrain.Start_Draw
           (Model =>
              Models.World.World_Model
                (Faction, Faction.Capital_World,
                 Concorde.UI.Models.World.Elevation_Display, False, False),
            View  =>
              Nazar.Views.Draw.Nazar_Draw_View
                (Builder.Get_View ("world-elevation")));

         Result.Terrain.Start_Draw
           (Model =>
              Models.World.World_Model
                (Faction, Faction.Capital_World,
                 Concorde.UI.Models.World.Temperature_Display, False, False),
            View  =>
              Nazar.Views.Draw.Nazar_Draw_View
                (Builder.Get_View ("world-temperature")));

         Builder.Get_View ("faction-label")
           .Set_Property ("text", Faction.Name);
         Builder.Get_View ("date-label").Set_Model
           (Concorde.UI.Models.Current_Date.Current_Date_Model_New);
         Builder.Get_View ("cash-label").Set_Model
           (Concorde.UI.Models.Current_Cash.Current_Cash_Model_New (Faction));

--        Builder.Get_View ("market").Set_Model
--          (Concorde.UI.Models.Market.Market_Model
--             (Accord.Market.Get
--                  (Accord.Market.Get_By_World
--                       (Faction.Capital_World.Reference_World))));

      --  if Builder.Has_View ("faction-colonies") then
      --     Builder.Get_View ("faction-colonies").Set_Model
      --       (Concorde.UI.Models.Colonies.Faction_Colony_Table (Faction));
      --  end if;
      --
      --  if Builder.Has_View ("colony-pop-groups") then
      --     Builder.Get_View ("colony-pop-groups").Set_Model
      --       (Concorde.UI.Models.Colonies.Colony_Pop_Group_Model
      --          (Colony));
      --  end if;

      --  if Builder.Has_View ("colony-policies") then
      --     Builder.Get_View ("colony-policies").Set_Model
      --       (Concorde.UI.Models.Colonies.Colony_Policy_Model (Colony));
      --  end if;
      --
      --  if Builder.Has_View ("colony-market") then
      --     Builder.Get_View ("colony-market").Set_Model
      --       (Concorde.UI.Models.Colonies.Colony_Market_Model (Colony));
      --  end if;
      --
      --  if Builder.Has_View ("food-market") then
      --     Builder.Get_View ("food-market").Set_Model
      --       (Concorde.UI.Models.Commodities.Commodity_Market_Model
      --          (Commodity =>
      --               Accord.Commodity.Get_By_Tag ("food")));
      --  end if;
      --
      --  if Builder.Has_View ("silicon-market") then
      --     Builder.Get_View ("silicon-market").Set_Model
      --       (Concorde.UI.Models.Commodities.Commodity_Market_Model
      --          (Commodity =>
      --               Accord.Commodity.Get_By_Tag ("silicon")));
      --  end if;
      --
      --  if Builder.Has_View ("plastic-market") then
      --     Builder.Get_View ("plastic-market").Set_Model
      --       (Concorde.UI.Models.Commodities.Commodity_Market_Model
      --          (Commodity =>
      --               Accord.Commodity.Get_By_Tag ("plastic")));
      --  end if;
      --
      --  if Builder.Has_View ("basic-electronics-market") then
      --     Builder.Get_View ("basic-electronics-market").Set_Model
      --       (Concorde.UI.Models.Commodities.Commodity_Market_Model
      --          (Commodity =>
      --               Accord.Commodity.Get_By_Tag
      --             ("basic-electronics")));
      --  end if;

--        Builder.Get_View ("population").Set_Model
--          (Concorde.UI.Models.Population.Population_Model
--             (Faction.Capital_World));

         return UI_Type (Result);
      end;
   end Get_Nazar_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Nazar_UI) is
   begin
      UI.Top.Show;
   end Start;

   ----------
   -- Stop --
   ----------

   overriding procedure Stop
     (UI      : in out Root_Nazar_UI;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Message);
      UI.Top.Destroy;
      Nazar.Main.Stop;
   end Stop;

end Concorde.UI.Nazar_UI;
