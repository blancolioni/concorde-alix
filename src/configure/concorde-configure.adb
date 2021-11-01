with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;

with Tropos.Reader;

with Concorde.Configure.Resources;

with Concorde.Options;
with Concorde.Paths;

with Concorde.Calendar;

with Accord.User;
with Accord.Calendar;

package body Concorde.Configure is

   function Initial_Root_Password return String;

   ---------------------------
   -- Configure_Association --
   ---------------------------

   procedure Configure_Association
     (Config    : Tropos.Configuration;
      Field     : String;
      Configure : not null access
        procedure (Left, Right : String))
   is
      Assoc_Config  : constant Tropos.Configuration :=
                        Config.Child (Field);
      Left_Config   : constant Tropos.Configuration :=
                        Assoc_Config.Child (1);
      Right_Config  : constant Tropos.Configuration :=
                        Assoc_Config.Child (2);
   begin
      for Left_Item of Left_Config loop
         Configure (Left_Item.Value,
                    Right_Config.Get (Left_Item.Config_Name));
      end loop;
   end Configure_Association;

   ---------------------
   -- Configure_Money --
   ---------------------

   function Configure_Money
     (Config  : Tropos.Configuration;
      Field   : String;
      Default : Non_Negative_Real)
      return Concorde.Money.Money_Type
   is
   begin
      return Concorde.Money.To_Money (Get_Real (Config, Field, Default));
   end Configure_Money;

   ---------------------
   -- Configure_Price --
   ---------------------

   function Configure_Price
     (Config : Tropos.Configuration;
      Field  : String)
      return Concorde.Money.Price_Type
   is
   begin
      return Concorde.Money.To_Price (Get_Real (Config, Field, 0.0));
   end Configure_Price;

   ------------------------
   -- Configure_Quantity --
   ------------------------

   function Configure_Quantity
     (Config : Tropos.Configuration;
      Field  : String)
      return Concorde.Quantities.Quantity_Type
   is
   begin
      return Concorde.Quantities.To_Quantity (Get_Real (Config, Field, 0.0));
   end Configure_Quantity;

   --------------
   -- Get_Real --
   --------------

   function Get_Real
     (Config  : Tropos.Configuration;
      Field   : String;
      Default : Real := 0.0)
      return Real
   is
   begin
      return Real (Float'(Config.Get (Field, Float (Default))));
   end Get_Real;

   ---------------------------
   -- Initial_Root_Password --
   ---------------------------

   function Initial_Root_Password return String is
      Password_Chars : constant String :=
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                         & "abcdefghijklmnopqrstuvwxyz"
                         & "0123456789"
                         & "!@#$%^&*_-=+<>";
   begin
      if Concorde.Options.Generate_Root_Password then
         declare
            package Random is
              new Ada.Numerics.Discrete_Random (Natural);
            Gen : Random.Generator;
         begin
            Random.Reset (Gen);

            return Password : String (1 .. 12) do
               for Ch of Password loop
                  declare
                     Index : constant Natural :=
                               Password_Chars'First
                                 + Random.Random (Gen)
                     mod Password_Chars'Length;
                  begin
                     Ch := Password_Chars (Index);
                  end;
               end loop;
               Ada.Text_IO.Put_Line ("root password: " & Password);
            end return;
         end;
      else
         return "root";
      end if;
   end Initial_Root_Password;

   -------------------------
   -- Initialize_Database --
   -------------------------

   procedure Initialize_Database is
   begin
      Accord.User.Create ("root", Initial_Root_Password, True);
      Accord.Calendar.Create
        (Clock => Concorde.Calendar.Clock);
      Concorde.Configure.Resources.Configure_Atmosphere_Components
        (Tropos.Reader.Read_Config
           (Concorde.Paths.Config_File
                ("star-systems/atmosphere.txt")));
   end Initialize_Database;

   ------------------------
   -- Load_Scenario_File --
   ------------------------

   function Load_Scenario_File
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Name       : String)
      return Tropos.Configuration
   is
   begin
      return Tropos.Reader.Read_Config
        (Scenario_File (Scenario_Name, Directory_Name, File_Name));
   end Load_Scenario_File;

   -------------------------
   -- Load_Scenario_Files --
   -------------------------

   procedure Load_Scenario_Files
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Class_Name : String;
      Process         : not null access
        procedure (Config : Tropos.Configuration))
   is
      procedure Call_Process (Path : String);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process (Path : String) is
      begin
         Process (Tropos.Reader.Read_Config (Path));
      end Call_Process;

   begin
      Scan_Scenario_Files
        (Scenario_Name, Directory_Name, File_Class_Name,
         Call_Process'Access);
   end Load_Scenario_Files;

   ----------------
   -- Real_Value --
   ----------------

   function Real_Value
     (Config  : Tropos.Configuration)
      return Real
   is
   begin
      return Real (Float'(Config.Value));
   end Real_Value;

   -------------------------
   -- Scan_Scenario_Files --
   -------------------------

   procedure Scan_Scenario_Files
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Class_Name : String;
      Process         : not null access
        procedure (Path : String))
   is
      procedure Call_Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      procedure Scan_Directory
        (Directory_Entry : Ada.Directories.Directory_Entry_Type);

      ------------------
      -- Call_Process --
      ------------------

      procedure Call_Process
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
      begin
         Process (Ada.Directories.Full_Name (Directory_Entry));
      end Call_Process;

      procedure Scan_Directory
        (Directory_Entry : Ada.Directories.Directory_Entry_Type)
      is
         Entry_Name : constant String :=
                        Ada.Directories.Simple_Name (Directory_Entry);
      begin
         if not Ada.Characters.Handling.Is_Alphanumeric
           (Entry_Name (Entry_Name'First))
         then
            return;
         end if;

         Ada.Directories.Search
           (Directory => Ada.Directories.Full_Name (Directory_Entry),
            Pattern   => "*." & File_Class_Name,
            Filter    =>
              (Ada.Directories.Ordinary_File => True, others => False),
            Process   => Call_Process'Access);
         Ada.Directories.Search
           (Directory => Ada.Directories.Full_Name (Directory_Entry),
            Pattern   => "*",
            Filter    => (Ada.Directories.Directory => True, others => False),
            Process   => Scan_Directory'Access);
      end Scan_Directory;

   begin
      Ada.Directories.Search
        (Directory =>
           Scenario_Directory (Scenario_Name, Directory_Name),
         Pattern   => "*." & File_Class_Name,
         Filter    => (Ada.Directories.Ordinary_File => True, others => False),
         Process   => Call_Process'Access);
      Ada.Directories.Search
        (Directory =>
           Scenario_Directory (Scenario_Name, Directory_Name),
         Pattern   => "*",
         Filter    => (Ada.Directories.Directory => True, others => False),
         Process   => Scan_Directory'Access);
   end Scan_Scenario_Files;

   ------------------------
   -- Scenario_Directory --
   ------------------------

   function Scenario_Directory
     (Scenario_Name  : String;
      Directory_Name : String)
      return String
   is
      Scenario_Path : constant String :=
                        Concorde.Paths.Config_File
                          ("scenarios/" & Scenario_Name
                           & "/" & Directory_Name);
      Regular_Path  : constant String :=
                        Concorde.Paths.Config_File (Directory_Name);
   begin
      if Ada.Directories.Exists (Scenario_Path) then
         return Scenario_Path;
      elsif Ada.Directories.Exists (Regular_Path) then
         return Regular_Path;
      else
         raise Constraint_Error with
         Scenario_Name
           & ": cannot find scenario directory " & Directory_Name;
      end if;
   end Scenario_Directory;

   -------------------
   -- Scenario_File --
   -------------------

   function Scenario_File
     (Scenario_Name   : String;
      Directory_Name  : String;
      File_Name       : String)
      return String

   is
      Scenario_Path : constant String :=
                        Concorde.Paths.Config_File
                          ("scenarios/" & Scenario_Name
                           & "/" & Directory_Name
                           & "/" & File_Name);
      Regular_Path  : constant String :=
                        Concorde.Paths.Config_File
                          (Directory_Name
                           & "/" & File_Name);
   begin
      if Ada.Directories.Exists (Scenario_Path) then
         return Scenario_Path;
      elsif Ada.Directories.Exists (Regular_Path) then
         return Regular_Path;
      else
         raise Constraint_Error with
         Scenario_Name
           & ": cannot find scenario file " & Directory_Name & "/" & File_Name;
      end if;
   end Scenario_File;

   --------------------
   -- To_Single_Line --
   --------------------

   function To_Single_Line
     (Value : String)
      return String
   is
   begin
      return Line : String := Value do
         for Ch of Line loop
            if Ch < ' ' then
               Ch := ' ';
            end if;
         end loop;
      end return;
   end To_Single_Line;

end Concorde.Configure;
