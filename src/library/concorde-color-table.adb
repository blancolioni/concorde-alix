with WL.String_Maps;

package body Concorde.Color.Table is

   package Color_Maps is
     new WL.String_Maps (Concorde_Color);

   Map : Color_Maps.Map;

   ---------
   -- Add --
   ---------

   procedure Add
     (Name  : String;
      Color : Concorde_Color)
   is
   begin
      if Exists (Name) then
         Map.Replace (Name, Color);
      else
         Map.Insert (Name, Color);
      end if;
   end Add;

   ------------
   -- Exists --
   ------------

   function Exists (Name : String) return Boolean is
   begin
      return Map.Contains (Name);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get (Name : String) return Concorde_Color is
   begin
      return Map.Element (Name);
   end Get;

end Concorde.Color.Table;
