with Accord.Has_Color;

package Concorde.Color is

   type Concorde_Color is
      record
         Red, Green, Blue, Alpha : Unit_Real;
      end record;

   Black : constant Concorde_Color := (0.0, 0.0, 0.0, 1.0);
   White : constant Concorde_Color := (1.0, 1.0, 1.0, 1.0);

   function From_String
     (Item : String)
      return Concorde_Color;

   function To_Html_String
     (Color : Concorde_Color)
      return String;

   function To_Html_String
     (R, G, B : Unit_Real;
      Alpha   : Unit_Real := 1.0)
      return String
   is (To_Html_String ((R, G, B, Alpha)));

   function Get_Color
     (Has_Color : Accord.Has_Color.Has_Color_Class)
      return Concorde_Color
     is (Has_Color.Red, Has_Color.Green, Has_Color.Blue, 1.0);

end Concorde.Color;
