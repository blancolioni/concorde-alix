private package Concorde.Color.Table is

   function Exists (Name : String) return Boolean;
   function Get (Name : String) return Concorde_Color;
   procedure Add (Name : String;
                  Color : Concorde_Color);

end Concorde.Color.Table;
