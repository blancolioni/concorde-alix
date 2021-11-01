with WL.String_Maps;

with Accord.Consumer_Commodity;
with Accord.Service_Commodity;

with Accord.Db;

package body Concorde.Commodities is

   type Cached_Commodity_Record is
      record
         Handle : Accord.Commodity.Commodity_Handle;
      end record;

   package Commodity_Maps is
     new WL.String_Maps (Cached_Commodity_Record);

   Commodity_Map : Commodity_Maps.Map;

   function Load (Tag : String) return Boolean;

   ------------
   -- Exists --
   ------------

   function Exists (Tag : String) return Boolean is
   begin
      if Commodity_Map.Contains (Tag) then
         return True;
      else
         return Load (Tag);
      end if;
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Tag : String) return Commodity_Class is
      Position : constant Commodity_Maps.Cursor :=
                   Commodity_Map.Find (Tag);
   begin
      if Commodity_Maps.Has_Element (Position) then
         return Commodity_Map (Position).Handle;
      elsif Load (Tag) then
         return Commodity_Map (Tag).Handle;
      else
         raise Constraint_Error with
         Tag & ": unknown commodity";
      end if;
   end Get;

   ---------------
   -- Happiness --
   ---------------

   function Happiness (Commodity : Commodity_Class) return Unit_Real is
   begin
      case Commodity.Top_Record is
         when Accord.Db.R_Consumer_Commodity =>
            return Accord.Consumer_Commodity.Get_From_Commodity (Commodity)
              .Happiness;
         when Accord.Db.R_Service_Commodity =>
            return Accord.Service_Commodity.Get_From_Commodity (Commodity)
              .Happiness;
         when others =>
            raise Constraint_Error with
            Commodity.Tag & ": no happiness";
      end case;
   end Happiness;

   ----------
   -- Load --
   ----------

   function Load (Tag : String) return Boolean is
      Handle : constant Accord.Commodity.Commodity_Handle :=
                 Accord.Commodity.Get_By_Tag (Tag);
   begin
      if Handle.Has_Element then
         Commodity_Map.Insert (Tag, (Handle => Handle));
         return True;
      else
         return False;
      end if;
   end Load;

   --------------------
   -- Yield_Estimate --
   --------------------

   function Yield_Estimate
     (Resource : Resource_Class;
      Sector   : Accord.World_Sector.World_Sector_Class)
      return Non_Negative_Real
   is
      pragma Unreferenced (Sector);
   begin
      return Resource.Yield;
   end Yield_Estimate;

end Concorde.Commodities;
