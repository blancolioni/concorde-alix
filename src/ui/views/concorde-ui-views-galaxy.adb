with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Accord.Faction;
with Accord.Star;
with Accord.Star_System_Distance;

with Accord.Star;
with Accord.Star_System.Selections;

package body Concorde.UI.Views.Galaxy is

   type System_Distance_Record is
      record
         To       : Accord.Star_System.Star_System_Handle;
         Index    : Positive;
         X, Y, Z  : Real;
         Distance : Non_Negative_Real;
      end record;

   package System_Distance_Lists is
     new Ada.Containers.Doubly_Linked_Lists (System_Distance_Record);

   type System_Record is
      record
         Handle  : Accord.Star_System.Star_System_Handle;
         Capital : Accord.Faction.Faction_Handle;
         Color   : Concorde.Color.Concorde_Color;
         X, Y, Z : Real;
         Nearby  : System_Distance_Lists.List;
      end record;

   package System_Record_Vectors is
     new Ada.Containers.Vectors (Positive, System_Record);

   type Root_Galaxy_View is
     new View_Object_Interface with
      record
         Faction       : Accord.Faction.Faction_Handle;
         Full_Port     : View_Port;
         Initial_Port  : View_Port;
         Systems       : System_Record_Vectors.Vector;
      end record;

   overriding function Display_Title
     (View : Root_Galaxy_View)
      return String
   is ("Galaxy");

   overriding function Full_Size
     (View : Root_Galaxy_View)
      return View_Port
   is (View.Full_Port);

   overriding function Initial_View
     (View : Root_Galaxy_View)
      return View_Port
   is (View.Initial_Port);

   overriding function Background_Color
     (View : Root_Galaxy_View)
      return Concorde.Color.Concorde_Color
   is (Concorde.Color.Black);

   overriding function Has_Changed
     (View : Root_Galaxy_View)
      return Boolean
   is (False);

   overriding procedure Clear_Changed
     (View : in out Root_Galaxy_View)
   is null;

   overriding procedure Get_Draw_Commands
     (View          : Root_Galaxy_View;
      Factory       : Draw_Command_Factory'Class;
      Screen_Width  : Non_Negative_Real;
      Screen_Height : Non_Negative_Real;
      Commands      : in out Draw_Command_List'Class);

   overriding procedure Iterate_Children
     (View     : Root_Galaxy_View;
      Process  : not null access
        procedure (Child : View_Object_Interface'Class))
   is null;

   overriding procedure Iterate_Properties
     (View     : Root_Galaxy_View;
      Process : not null access
        procedure (Property_Name : String;
                   Property_Value : Concorde.UI.Values.Value_Type))
   is null;

   -----------------
   -- Galaxy_View --
   -----------------

   function Galaxy_View
     (Faction : Accord.Faction.Faction_Handle)
      return View_Object_Interface'Class
   is
      package Index_Maps is new WL.String_Maps (Positive);
      Index_Map : Index_Maps.Map;

      Left, Top     : Real := Real'Last;
      Right, Bottom : Real := Real'First;
      Have_Capital  : Boolean := False;
   begin

      Ada.Text_IO.Put ("Creating galaxy view ...");
      Ada.Text_IO.Flush;

      return View : Root_Galaxy_View do
         View.Faction := Faction;
         for Star_System of
           Accord.Star_System.Selections.Select_All
         loop
            declare
               use type Accord.Faction.Faction_Handle;
               Star  : constant Accord.Star.Star_Handle :=
                 Accord.Star.Get
                   (Accord.Star.First_By_Star_System
                      (Star_System.Reference));
               Color : constant Concorde.Color.Concorde_Color :=
                 (Star.Red,
                  Star.Green,
                  Star.Blue,
                  1.0);
               Capital : constant Accord.Faction.Faction_Handle :=
                 Accord.Faction.First_By_Capital_System
                   (Star_System.Reference);
               Rec : constant System_Record := System_Record'
                 (Handle  => Star_System,
                  Capital => Accord.Faction.Get (Capital),
                  Color   => Color,
                  X       => Star_System.X,
                  Y       => Star_System.Y,
                  Z       => Star_System.Z,
                  Nearby  => System_Distance_Lists.Empty_List);
            begin
               View.Systems.Append (Rec);
               Left := Real'Min (Left, Star_System.X);
               Top  := Real'Min (Top, Star_System.Y);
               Right := Real'Max (Right, Star_System.X);
               Bottom  := Real'Max (Bottom, Star_System.Y);
               if Capital = Faction.Reference then
                  Have_Capital := True;
                  View.Initial_Port :=
                    (Star_System.X - 5.0, Star_System.Y - 5.0,
                     10.0, 10.0);
               end if;
               if Index_Map.Contains (Star_System.Name) then
                  raise Constraint_Error with
                    "multiple systems called " & Star_System.Name;
               end if;

               Index_Map.Insert (Star_System.Name, View.Systems.Last_Index);
            end;
         end loop;

         for Star_System_Index in 1 .. View.Systems.Last_Index loop
            for Nearby of
              Accord.Star_System_Distance
                .Select_Star_System_Range_Bounded_By_Distance
                  (View.Systems.Element (Star_System_Index).Handle.Reference,
                   0.0, 8.0)
            loop
               declare
                  To    : constant Handles.Star_System.Star_System_Handle :=
                    Handles.Star_System.Get (Nearby.To);
                  D     : constant Non_Negative_Real :=
                    Nearby.Distance;
                  Index : constant Positive := Index_Map.Element (To.Name);
                  X     : constant Real := View.Systems.Element (Index).X;
                  Y     : constant Real := View.Systems.Element (Index).Y;
                  Z     : constant Real := View.Systems.Element (Index).Z;
               begin
                  View.Systems (Star_System_Index).Nearby.Append
                    (System_Distance_Record'
                       (To       => To,
                        Index    => Index,
                        Distance => D,
                        X        => X,
                        Y        => Y,
                        Z        => Z));
               end;
            end loop;
         end loop;

         View.Full_Port := (Left, Top, Right - Left, Bottom - Top);
         if not Have_Capital then
            View.Initial_Port := View.Full_Port;
         end if;

         Ada.Text_IO.Put_Line ("done");

      end return;
   end Galaxy_View;

   -----------------------
   -- Get_Draw_Commands --
   -----------------------

   overriding procedure Get_Draw_Commands
     (View          : Root_Galaxy_View;
      Factory       : Draw_Command_Factory'Class;
      Screen_Width  : Non_Negative_Real;
      Screen_Height : Non_Negative_Real;
      Commands      : in out Draw_Command_List'Class)
   is
      pragma Unreferenced (Screen_Width, Screen_Height);
   begin
      for Rec of View.Systems loop
         for Link of Rec.Nearby loop
            declare
               X : constant Unit_Real := 0.75 / Link.Distance + 0.125;
            begin
               Commands.Add (Factory.Move_To (Rec.X, Rec.Y));
               Commands.Add (Factory.Color (X, X, X));
               Commands.Add (Factory.Line_To (Link.X, Link.Y));
               Commands.Add (Factory.Render);
            end;
         end loop;
      end loop;

      for Rec of View.Systems loop
         Commands.Add
           (Factory.Move_To (Rec.X, Rec.Y));
         Commands.Add
           (Factory.Color (Rec.Color));
         Commands.Add
           (Factory.Arc (Screen (2.0)));
         Commands.Add
           (Factory.Render (Fill => True));
         if Rec.Capital.Has_Element then
            Commands.Add (Factory.Save);
            Commands.Add
              (Factory.Color
                 (Rec.Capital.Red,
                  Rec.Capital.Green,
                  Rec.Capital.Blue));
            Commands.Add
              (Factory.Line_Width (3.0));
            Commands.Add
              (Factory.Arc (Screen (5.0)));
            Commands.Add
              (Factory.Render);
            Commands.Add (Factory.Restore);
         end if;
      end loop;
   end Get_Draw_Commands;

end Concorde.UI.Views.Galaxy;
