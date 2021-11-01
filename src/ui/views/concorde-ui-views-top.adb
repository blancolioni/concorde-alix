with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with Concorde.UI.Views.Galaxy;

package body Concorde.UI.Views.Top is

   type Child_View is
      record
         Left, Top     : Real;
         Right, Bottom : Real;
         Object        : View_Object_Holders.Holder;
      end record;

   function Get_View_Port
     (View : Child_View;
      Width, Height : Non_Negative_Real)
      return View_Port;

   package Child_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Child_View);

   type Root_Top_View is
     new View_Object_Interface with
      record
         Faction  : Accord.Faction.Faction_Handle;
         Children : Child_Lists.List;
      end record;

   overriding function Display_Title
     (View : Root_Top_View)
      return String
   is ("Concorde - " & View.Faction.Name);

   overriding function Full_Size
     (View : Root_Top_View)
      return View_Port
   is (0.0, 0.0, 1920.0, 1080.0);

   overriding function Initial_View
     (View : Root_Top_View)
      return View_Port
   is (0.0, 0.0, 1920.0, 1080.0);

   overriding function Background_Color
     (View : Root_Top_View)
      return Concorde.Color.Concorde_Color
   is (0.8, 0.8, 0.9, 1.0);

   overriding function Has_Changed
     (View : Root_Top_View)
      return Boolean
   is (False);

   overriding procedure Clear_Changed
     (View : in out Root_Top_View)
   is null;

   overriding procedure Get_Draw_Commands
     (View          : Root_Top_View;
      Factory       : Draw_Command_Factory'Class;
      Screen_Width  : Non_Negative_Real;
      Screen_Height : Non_Negative_Real;
      Commands      : in out Draw_Command_List'Class);

   overriding procedure Iterate_Children
     (View     : Root_Top_View;
      Process  : not null access
        procedure (Child : View_Object_Interface'Class))
   is null;

   overriding procedure Iterate_Properties
     (View     : Root_Top_View;
      Process : not null access
        procedure (Property_Name : String;
                   Property_Value : Concorde.UI.Values.Value_Type))
   is null;

   -----------------------
   -- Get_Draw_Commands --
   -----------------------

   overriding procedure Get_Draw_Commands
     (View          : Root_Top_View;
      Factory       : Draw_Command_Factory'Class;
      Screen_Width  : Non_Negative_Real;
      Screen_Height : Non_Negative_Real;
      Commands      : in out Draw_Command_List'Class)
   is
   begin
      for Child of View.Children loop
         declare
            Child_Port : constant View_Port :=
              Get_View_Port (Child, Screen_Width, Screen_Height);
         begin
            Commands.Add
              (Factory.View_Object
                 (Object => Child.Object.Element,
                  Target => Child_Port));
         end;
      end loop;
   end Get_Draw_Commands;

   -------------------
   -- Get_View_Port --
   -------------------

   function Get_View_Port
     (View          : Child_View;
      Width, Height : Non_Negative_Real)
      return View_Port
   is
   begin
      return View_Port'
        (Left   => View.Left,
         Top    => View.Top,
         Width  => Width - View.Left - View.Right,
         Height => Height - View.Top - View.Bottom);
   end Get_View_Port;

   --------------
   -- Top_View --
   --------------

   function Top_View
     (Faction : Accord.Faction.Faction_Handle)
      return View_Object_Interface'Class
   is
   begin

      Ada.Text_IO.Put ("Creating top view ...");
      Ada.Text_IO.Flush;

      return View : Root_Top_View do
         View.Faction := Faction;
         View.Children.Append
           (Child_View'
              (Left     => 64.0,
               Top      => 160.0,
               Right    => 64.0,
               Bottom   => 64.0,
               Object   =>
                 View_Object_Holders.To_Holder
                   (Concorde.UI.Views.Galaxy.Galaxy_View (Faction))));
      end return;
   end Top_View;

end Concorde.UI.Views.Top;
