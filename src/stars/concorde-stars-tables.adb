with Ada.Containers.Doubly_Linked_Lists;

with Tropos.Reader;
with Tropos.Writer;

with Concorde.Paths;

package body Concorde.Stars.Tables is

   type Color_Type is
      record
         Red, Green, Blue, Alpha : Unit_Real;
      end record;

   type Star_Info_Record is
      record
         Solar_Masses : Non_Negative_Real;
         Class        : Accord.Db.Spectral_Class;
         Subclass     : Natural;
         Surface_Temp : Non_Negative_Real;
         Color        : Color_Type;
         Radius       : Non_Negative_Real;
         Luminosity   : Non_Negative_Real;
      end record;

   function Brighten
     (Color : Color_Type;
      Temperature : Non_Negative_Real)
      return Color_Type;

   package Star_Info_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Star_Info_Record);

   Main_Sequence_Table : Star_Info_Lists.List;

   procedure Read_Tables;

   procedure Check_Tables;

   --------------
   -- Brighten --
   --------------

   function Brighten
     (Color       : Color_Type;
      Temperature : Non_Negative_Real)
      return Color_Type
   is
      R : constant Real :=
            Color.Red * Temperature * (0.0534 / 255.0) - (43.0 / 255.0);
      G : constant Real :=
            Color.Green * Temperature * (0.0628 / 255.0) - (77.0 / 255.0);
      B : constant Real :=
            Color.Blue * Temperature * (0.0735 / 255.0) - (115.0 / 255.0);
   begin
      return (Unit_Clamp (R), Unit_Clamp (G), Unit_Clamp (B), Color.Alpha);
   end Brighten;

   ------------------
   -- Check_Tables --
   ------------------

   procedure Check_Tables is
   begin
      if Main_Sequence_Table.Is_Empty then
         Read_Tables;
      end if;
   end Check_Tables;

   ----------------------------
   -- Get_Main_Sequence_Info --
   ----------------------------

   procedure Get_Main_Sequence_Info
     (Solar_Masses : Non_Negative_Real;
      Class        : out Accord.Db.Spectral_Class;
      Subclass     : out Natural;
      Radius       : out Non_Negative_Real;
      Luminosity   : out Non_Negative_Real;
      R, G, B      : out Unit_Real)
   is
      use Star_Info_Lists;
      Position : Cursor;
   begin

      Check_Tables;

      Position := Main_Sequence_Table.First;

      while Has_Element (Position) loop
         declare
            Info : constant Star_Info_Record := Element (Position);
         begin
            exit when Solar_Masses >= Info.Solar_Masses;
         end;
         Next (Position);
      end loop;

      if not Has_Element (Position) then
         Position := Main_Sequence_Table.Last;
      end if;

      declare
         Info : Star_Info_Record renames Element (Position);
      begin
         Class := Info.Class;
         Subclass := Info.Subclass;
         Radius := Info.Radius;
         Luminosity := Info.Luminosity;
         R := Info.Color.Red;
         G := Info.Color.Green;
         B := Info.Color.Blue;
      end;

   end Get_Main_Sequence_Info;

   -----------------
   -- Read_Tables --
   -----------------

   procedure Read_Tables is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_CSV_Config
                   (Concorde.Paths.Config_File
                      ("star-systems/star-classification.txt"),
                    Header_Line   => True,
                    Separator     => ';',
                    Extend_Header => False);
   begin
      Tropos.Writer.Write_Config (Config, "test.txt");
      for Info_Config of Config loop
         declare

            function Get (Name : String) return Real
            is (Real (Float'(Info_Config.Get (Name))));

            Class        : constant String := Info_Config.Get ("type");
            Surface_Temp : constant Real := Get ("surface-temp");
            Radius       : constant Real := Get ("radius");
            Mass         : constant Real := Get ("mass");
            Luminosity   : constant Real := Get ("luminosity");
            Red          : constant Natural := Info_Config.Get ("r");
            Green        : constant Natural := Info_Config.Get ("g");
            Blue         : constant Natural := Info_Config.Get ("b");
            Color       : constant Color_Type :=
                             Color_Type'
                               (Red   => Real (Red) / 255.0,
                                Green => Real (Green) / 255.0,
                                Blue  => Real (Blue) / 255.0,
                                Alpha => 1.0);
            Info         : constant Star_Info_Record :=
                             (Solar_Masses => Mass,
                              Class        =>
                                Accord.Db.Spectral_Class'Value
                                  ((1 => Class (Class'First))),
                              Subclass     =>
                                Natural'Value
                                  ((1 => Class (Class'First + 1))),
                              Surface_Temp => Surface_Temp,
                              Color       =>
                                Brighten (Color, Surface_Temp),
                              Radius       => Radius,
                              Luminosity   => Luminosity);
         begin
            Main_Sequence_Table.Append (Info);
         end;
      end loop;
   end Read_Tables;

end Concorde.Stars.Tables;
