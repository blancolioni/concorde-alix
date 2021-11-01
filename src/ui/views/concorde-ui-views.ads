private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;

with Concorde.Color;

with Concorde.UI.Values;

package Concorde.UI.Views is

   type View_Port is
      record
         Left, Top     : Real;
         Width, Height : Non_Negative_Real;
      end record;

   type Measurement is private;

   function Screen (M : Real) return Measurement;
   function View (M : Real) return Measurement;

   function Is_Screen (M : Measurement) return Boolean;
   function Is_View (M : Measurement) return Boolean;

   function Measure
     (M     : Measurement)
      return Real;

   type Draw_Command_Interface is interface;

   subtype Draw_Command is Draw_Command_Interface'Class;

   type Real_Property is (Line_Width);

   type Draw_Command_Factory is interface;

   function Move
     (Factory : Draw_Command_Factory;
      Relative : Boolean;
      Move_X   : Measurement;
      Move_Y   : Measurement)
      return Draw_Command
      is abstract;

   function Move_To
     (Factory  : Draw_Command_Factory'Class;
      X, Y     : Real)
      return Draw_Command
   is (Factory.Move (False, View (X), View (Y)));

   function Move
     (Factory  : Draw_Command_Factory'Class;
      X, Y     : Real)
      return Draw_Command
   is (Factory.Move (True, Screen (X), Screen (Y)));

   function Line_To
     (Factory : Draw_Command_Factory;
      X, Y    : Real)
      return Draw_Command
      is abstract;

   function Arc
     (Factory : Draw_Command_Factory;
      Radius  : Measurement;
      Start   : Real := 0.0;
      Finish  : Real := 360.0)
      return Draw_Command
      is abstract;

   function Render
     (Factory  : Draw_Command_Factory;
      Fill     : Boolean := False;
      Preserve : Boolean := False)
      return Concorde.UI.Views.Draw_Command
      is abstract;

   function Property
     (Factory : Draw_Command_Factory;
      Prop    : Real_Property;
      Value   : Real)
      return Draw_Command
      is abstract;

   function Color
     (Factory : Draw_Command_Factory;
      To      : Concorde.Color.Concorde_Color)
      return Draw_Command
      is abstract;

   function Font
     (Factory  : Draw_Command_Factory;
      Family   : String;
      Size     : Non_Negative_Real;
      Bold     : Boolean := False;
      Italic   : Boolean := False)
      return Draw_Command
      is abstract;

   function Text
     (Factor : Draw_Command_Factory;
      S      : String)
      return Draw_Command
      is abstract;

   function Save
     (Factory : Draw_Command_Factory)
      return Draw_Command
      is abstract;

   function Restore
     (Factory : Draw_Command_Factory)
      return Draw_Command
      is abstract;

   function Line_Width
     (Factory : Draw_Command_Factory'Class;
      Width   : Non_Negative_Real)
      return Draw_Command
   is (Factory.Property (Line_Width, Width));

   function Color
     (Factory : Draw_Command_Factory'Class;
      Red     : Unit_Real;
      Green   : Unit_Real;
      Blue    : Unit_Real;
      Alpha   : Unit_Real := 1.0)
      return Draw_Command;

   type Draw_Command_List is tagged private;

   procedure Add
     (List    : in out Draw_Command_List'Class;
      Command : Draw_Command);

   procedure Iterate
     (List : Draw_Command_List'Class;
      Process : not null access
        procedure (Command : Draw_Command'Class));

   type View_Object_Interface is interface;

   function Display_Title
     (Object : View_Object_Interface)
      return String
      is abstract;

   function Full_Size
     (Object : View_Object_Interface)
      return View_Port
      is abstract;

   function Initial_View
     (Object : View_Object_Interface)
      return View_Port
      is abstract;

   function Background_Color
     (Object : View_Object_Interface)
      return Concorde.Color.Concorde_Color
      is abstract;

   function Has_Changed
     (Object : View_Object_Interface)
      return Boolean
      is abstract;

   procedure Clear_Changed
     (Object : in out View_Object_Interface)
   is abstract;

   procedure Get_Draw_Commands
     (Object        : View_Object_Interface;
      Factory       : Draw_Command_Factory'Class;
      Screen_Width  : Non_Negative_Real;
      Screen_Height : Non_Negative_Real;
      Commands      : in out Draw_Command_List'Class)
   is abstract;

   procedure Iterate_Children
     (Object        : View_Object_Interface;
      Screen_Width  : Non_Negative_Real;
      Screen_Height : Non_Negative_Real;
      Process       : not null access
        procedure (Child : View_Object_Interface'Class;
                   Partial : View_Port))
   is abstract;

   procedure Iterate_Properties
     (Object  : View_Object_Interface;
      Process : not null access
        procedure (Property_Name : String;
                   Property_Value : Concorde.UI.Values.Value_Type))
   is abstract;

   type View_Interface is interface;

   procedure Draw
     (View   : in out View_Interface)
   is abstract;

   function Partial_View
     (View   : View_Interface;
      Object : View_Object_Interface'Class;
      Port   : View_Port)
      return View_Interface'Class
      is abstract;

private

   type Measurement_Type is (Screen, View);

   type Measurement is
      record
         Measure : Measurement_Type;
         Value   : Real;
      end record;

   function Screen (M : Real) return Measurement is (Screen, M);

   function View (M : Real) return Measurement is (View, M);

   function Is_Screen (M : Measurement) return Boolean
   is (M.Measure = Screen);

   function Is_View (M : Measurement) return Boolean
   is (M.Measure = View);

   function Measure
     (M     : Measurement)
      return Real
   is (M.Value);

   package Draw_Command_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Draw_Command'Class);

   type Draw_Command_List is tagged
      record
         Commands : Draw_Command_Lists.List;
      end record;

   function Color
     (Factory : Draw_Command_Factory'Class;
      Red     : Unit_Real;
      Green   : Unit_Real;
      Blue    : Unit_Real;
      Alpha   : Unit_Real := 1.0)
      return Draw_Command
   is (Factory.Color ((Red, Green, Blue, Alpha)));

   package View_Object_Holders is
     new Ada.Containers.Indefinite_Holders
       (View_Object_Interface'Class);

end Concorde.UI.Views;
