with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Nazar.Colors;

with Accord.Star;
with Accord.Star_System;
with Accord.Star_System_Distance;

package body Concorde.UI.Models.Galaxy is

   type System_Distance_Record is
      record
         To       : Accord.Star_System.Star_System_Handle;
         Index    : Positive;
         X, Y, Z  : Nazar.Nazar_Float;
         Distance : Non_Negative_Real;
      end record;

   package System_Distance_Lists is
     new Ada.Containers.Doubly_Linked_Lists (System_Distance_Record);

   type System_Record is
      record
         Handle  : Accord.Star_System.Star_System_Handle;
         Capital : Accord.Faction.Faction_Handle;
         Color   : Nazar.Colors.Nazar_Color;
         X, Y, Z : Nazar.Nazar_Float;
         Nearby  : System_Distance_Lists.List;
      end record;

   package System_Record_Vectors is
     new Ada.Containers.Vectors (Positive, System_Record);

   type Root_Galaxy_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Faction       : Accord.Faction.Faction_Handle;
         Systems       : System_Record_Vectors.Vector;
      end record;

   type Galaxy_Model_Access is access all Root_Galaxy_Model'Class;

   overriding function Background_Color
     (View : Root_Galaxy_Model)
      return Nazar.Colors.Nazar_Color
   is (0.0, 0.0, 0.0, 1.0);

   procedure Draw_Galaxy
     (Model : in out Root_Galaxy_Model'Class);

   -----------------
   -- Draw_Galaxy --
   -----------------

   procedure Draw_Galaxy
     (Model : in out Root_Galaxy_Model'Class)
   is
      use Nazar;
   begin
      for Rec of Model.Systems loop
         for Link of Rec.Nearby loop
            declare
               X : constant Nazar_Unit_Float :=
                     Nazar_Unit_Float
                       (Nazar_Float'Min
                          (Nazar_Float (0.55 / Link.Distance + 0.125),
                           1.0));
            begin
               Model.Move_To (Rec.X, Rec.Y);
               Model.Set_Color (X, X, X);
               Model.Line_To (Link.X, Link.Y);
            end;
         end loop;
      end loop;

      Model.Render;

      Model.Save_State;
      Model.Set_Fill (True);

      for Rec of Model.Systems loop
         Model.Move_To (Rec.X, Rec.Y);
         Model.Set_Color (Rec.Color);
         Model.Circle (2.0);
         Model.Render;

         if Rec.Capital.Has_Element then
            Model.Save_State;
            Model.Set_Fill (False);
            Model.Set_Color
              (Nazar.Nazar_Unit_Float (Rec.Capital.Red),
               Nazar.Nazar_Unit_Float (Rec.Capital.Green),
               Nazar.Nazar_Unit_Float (Rec.Capital.Blue));
            Model.Circle (5.0);
            Model.Render;
            Model.Restore_State;
         end if;

      end loop;

      Model.Restore_State;

   end Draw_Galaxy;

   ------------------
   -- Galaxy_Model --
   ------------------

   function Galaxy_Model
     (Faction : Accord.Faction.Faction_Handle)
      return Nazar.Models.Draw.Nazar_Draw_Model
   is
      pragma Unreferenced (Faction);

      package Index_Maps is new WL.String_Maps (Positive);
      Index_Map : Index_Maps.Map;

      Left, Top     : Real := Real'Last;
      Right, Bottom : Real := Real'First;
      Model         : constant Galaxy_Model_Access :=
                        new Root_Galaxy_Model;
   begin

      Ada.Text_IO.Put ("Creating galaxy view ...");
      Ada.Text_IO.Flush;

      for Star_System of
        Accord.Star_System.Scan_By_Top_Record
      loop
         declare
            Star  : constant Accord.Star.Star_Handle :=
                      Accord.Star.First_By_Star_System
                        (Star_System);
            Color : constant Nazar.Colors.Nazar_Color :=
                      (Nazar.Nazar_Unit_Float (Star.Red),
                       Nazar.Nazar_Unit_Float (Star.Green),
                       Nazar.Nazar_Unit_Float (Star.Blue),
                       1.0);
            Capital : constant Accord.Faction.Faction_Handle :=
                        Accord.Faction.First_By_Capital_System
                          (Star_System);
            Rec : constant System_Record := System_Record'
              (Handle  => Star_System.To_Star_System_Handle,
               Capital => Capital,
               Color   => Color,
               X       => Nazar.Nazar_Float (Star_System.X),
               Y       => Nazar.Nazar_Float (Star_System.Y),
               Z       => Nazar.Nazar_Float (Star_System.Z),
               Nearby  => System_Distance_Lists.Empty_List);
         begin
            Model.Systems.Append (Rec);
            Left := Real'Min (Left, Star_System.X);
            Top  := Real'Min (Top, Star_System.Y);
            Right := Real'Max (Right, Star_System.X);
            Bottom  := Real'Max (Bottom, Star_System.Y);
            if Index_Map.Contains (Star_System.Name) then
               raise Constraint_Error with
                 "multiple systems called " & Star_System.Name;
            end if;

            Index_Map.Insert (Star_System.Name, Model.Systems.Last_Index);
         end;
      end loop;

      for Star_System_Index in 1 .. Model.Systems.Last_Index loop
         for Nearby of
           Accord.Star_System_Distance
             .Select_Star_System_Range_Bounded_By_Distance
               (Model.Systems.Element (Star_System_Index).Handle,
                0.0, 4.0)
         loop
            declare
               subtype Nazar_Float is Nazar.Nazar_Float;
               To    : constant Handles.Star_System.Star_System_Class :=
                         Nearby.To;
               D     : constant Non_Negative_Real :=
                         Nearby.Distance;
               Index : constant Positive := Index_Map.Element (To.Name);
               X     : constant Nazar_Float := Model.Systems.Element (Index).X;
               Y     : constant Nazar_Float := Model.Systems.Element (Index).Y;
               Z     : constant Nazar_Float := Model.Systems.Element (Index).Z;
            begin
               Model.Systems (Star_System_Index).Nearby.Append
                 (System_Distance_Record'
                    (To       => To.To_Star_System_Handle,
                     Index    => Index,
                     Distance => D,
                     X        => X,
                     Y        => Y,
                     Z        => Z));
            end;
         end loop;
      end loop;

      Model.Set_Bounding_Box
        (Box => Nazar.Rectangle'
           (X => Nazar.Nazar_Float (Left),
            Y => Nazar.Nazar_Float (Top),
            W => Nazar.Nazar_Float (Right - Left),
            H => Nazar.Nazar_Float (Bottom - Top)));

      Model.Draw_Galaxy;

      Ada.Text_IO.Put_Line ("done");

      return Nazar.Models.Draw.Nazar_Draw_Model (Model);

   end Galaxy_Model;

end Concorde.UI.Models.Galaxy;
