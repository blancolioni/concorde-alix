with WL.Localisation;
with WL.String_Maps;

with Accord.Climate;

package body Concorde.Climates is

   Airless_Tag   : constant String := "airless";
   Iceball_Tag   : constant String := "iceball";
   Jovian_Tag    : constant String := "jovian";
   Martian_Tag   : constant String := "martian";
   Temperate_Tag : constant String := "temperate";
   Venusian_Tag  : constant String := "venusian";
   Water_Tag     : constant String := "water";

   package Tag_Maps is
     new WL.String_Maps (Accord.Climate_Reference, Accord."=");

   Tag_Map       : Tag_Maps.Map;

   function Get (Tag : String) return Accord.Climate_Reference;

   -------------
   -- Airless --
   -------------

   function Airless return Accord.Climate_Reference
   is (Get (Airless_Tag));

   -------------
   -- Iceball --
   -------------

   function Iceball return Accord.Climate_Reference
      is (Get (Iceball_Tag));

   ------------
   -- Jovian --
   ------------

   function Jovian return Accord.Climate_Reference
   is (Get (Jovian_Tag));

   -------------
   -- Martian --
   -------------

   function Martian return Accord.Climate_Reference
   is (Get (Martian_Tag));

   ---------------
   -- Temperate --
   ---------------

   function Temperate return Accord.Climate_Reference
   is (Get (Temperate_Tag));

   --------------
   -- Venusian --
   --------------

   function Venusian return Accord.Climate_Reference
   is (Get (Venusian_Tag));

   -----------
   -- Water --
   -----------

   function Water return Accord.Climate_Reference
   is (Get (Water_Tag));

   ---------------------
   -- Default_Terrain --
   ---------------------

   function Default_Terrain
     (Climate : Accord.Climate_Reference)
      return Accord.Terrain_Handle
   is
   begin
      return Accord.Climate.Get (Climate).Default_Terrain;
   end Default_Terrain;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Accord.Climate_Reference is
   begin
      if not Tag_Map.Contains (Tag) then
         declare
            use Accord.Db;
            Reference : constant Climate_Reference :=
                          Accord.Climate.Get_By_Tag (Tag);
         begin
            if Reference = Null_Climate_Reference then
               raise Constraint_Error with
                 "unknown climate tag: " & Tag;
            end if;
            Tag_Map.Insert (Tag, Reference);
         end;
      end if;
      return Tag_Map.Element (Tag);
   end Get;

   ------------------
   -- Habitability --
   ------------------

   function Habitability
     (Climate : Accord.Climate_Reference)
      return Unit_Real
   is
   begin
      return Accord.Climate.Get (Climate).Habitability;
   end Habitability;

   ----------
   -- Name --
   ----------

   function Name
     (Climate : Accord.Climate_Reference)
      return String
   is
   begin
      return WL.Localisation.Local_Text
        (Accord.Climate.Get (Climate).Tag);
   end Name;

end Concorde.Climates;
