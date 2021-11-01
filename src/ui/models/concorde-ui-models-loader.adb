with WL.String_Maps;

--  with Concorde.UI.Models.Galaxy;
--  with Concorde.UI.Models.Markets;
--  with Concorde.UI.Models.Shell;
--  with Concorde.UI.Models.Worlds;

package body Concorde.UI.Models.Loader is

   package Model_Maps is
     new WL.String_Maps (Root_Concorde_Model'Class);

   Map : Model_Maps.Map;

   procedure Check_Map;

   ---------------
   -- Check_Map --
   ---------------

   procedure Check_Map is

      procedure Add
        (Name    : String;
         Model   : Root_Concorde_Model'Class);
      pragma Unreferenced (Add);

      ---------
      -- Add --
      ---------

      procedure Add
        (Name  : String;
         Model : Root_Concorde_Model'Class)
      is
      begin
         Map.Insert (Name, Model);
      end Add;

   begin
      if Map.Is_Empty then
         null;
--           Add ("shell", Concorde.UI.Models.Shell.Shell_Model);
--           Add ("market-price",
--                Concorde.UI.Models.Markets.Market_Price_Model);
--           Add ("galaxy",
--                Concorde.UI.Models.Galaxy.Galaxy_Model);
--           Add ("world",
--                Concorde.UI.Models.Worlds.World_Model);
      end if;
   end Check_Map;

   ------------
   -- Exists --
   ------------

   function Exists (Model_Name : String) return Boolean is
   begin
      Check_Map;
      return Map.Contains (Model_Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Model_Name : String)
      return Root_Concorde_Model'Class
   is
   begin
      Check_Map;
      return Map.Element (Model_Name);
   end Get;

end Concorde.UI.Models.Loader;
