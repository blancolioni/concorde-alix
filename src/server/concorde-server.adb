with Ada.Directories;
with Ada.Text_IO;

with WL.Command_Line;
with WL.Localisation;
with WL.Random.Names;

with Tropos.Reader;

with Concorde.Configure.Scenarios;

with Concorde.Options;
with Concorde.Paths;
with Concorde.Random;
with Concorde.Random_Names;

with Concorde.Color;
with Concorde.Factions.Create;

with Accord.Db.Database;

with Accord.Faction;
with Accord.User;

package body Concorde.Server is

   Name_Generator    : WL.Random.Names.Name_Generator;
   Server_Start_Time : Ada.Calendar.Time;

   function Have_Required_Argument
     (Argument_Name  : String;
      Argument_Value : String)
      return Boolean;

   -----------------
   -- Add_Faction --
   -----------------

   procedure Add_Faction is
      Database_Open : Boolean := False;
   begin
      if not Have_Required_Argument
        ("account-name", Concorde.Options.Account_Name)
        or else not Have_Required_Argument
          ("faction-name", Concorde.Options.Faction_Name)
      then
         return;
      end if;

      Accord.Db.Database.Open;
      Database_Open := True;

      declare
         User           : constant Accord.User.User_Class :=
           Accord.User.Get_By_Login
             (Login => Concorde.Options.Account_Name);
         Scenario_Name  : constant String :=
           (if Concorde.Options.Scenario = ""
            then "default"
            else Concorde.Options.Scenario);
         Faction_Name   : constant String :=
           Concorde.Options.Faction_Name;
         Faction_Adj    : constant String :=
           Concorde.Options.Faction_Adjective;
         Faction_Plural : constant String :=
           Concorde.Options.Faction_Plural_Name;
         Faction_Color  : constant String :=
           Concorde.Options.Faction_Color;
         Faction_Setup  : constant String :=
           Concorde.Options.Faction_Setup_Path;
         Faction        : constant Accord.Faction.Faction_Class :=
           Accord.Faction.First_By_Name
             (Name => Faction_Name);
         Setup_Path     : constant String :=
           Concorde.Configure.Scenario_File
             (Scenario_Name  => Scenario_Name,
              Directory_Name => "factions",
              File_Name      =>
                (if Faction_Setup = ""
                 then "default"
                 else Faction_Setup)
              & ".faction");
      begin
         if not User.Has_Element then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Concorde.Options.Account_Name
               & ": unknown user name");
            Accord.Db.Database.Close;
            return;
         end if;

         if Faction.Has_Element then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Faction_Name
               & ": faction already exists");
            Accord.Db.Database.Close;
            return;
         end if;

         if not Ada.Directories.Exists (Setup_Path) then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Cannot find setup file " & Setup_Path);
            Accord.Db.Database.Close;
            return;
         end if;

         if Faction_Setup = "" then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Using default faction setup " & Setup_Path);
         end if;

         declare
            Color       : constant Concorde.Color.Concorde_Color :=
              (if Faction_Color /= ""
               then Concorde.Color.From_String (Faction_Color)
               else Concorde.Color.Concorde_Color'
                 (Red   => Concorde.Random.Unit_Random,
                  Green => Concorde.Random.Unit_Random,
                  Blue  => Concorde.Random.Unit_Random,
                  Alpha => 1.0));
            New_Faction : constant Accord.Faction.Faction_Class :=
              Concorde.Factions.Create.Create_Faction
                (User        => User,
                 Name        => Faction_Name,
                 Adjective   => Faction_Adj,
                 Plural_Name => Faction_Plural,
                 Color       => Color,
                 Setup       =>
                   Tropos.Reader.Read_Config (Setup_Path));
         begin
            if not New_Faction.Has_Element then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Failed to create faction");
            else
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "Faction created successfully");
            end if;
         end;

         Accord.Db.Database.Close;
         Database_Open := False;
         return;
      end;

   exception

      when others =>
         if Database_Open then
            Accord.Db.Database.Close;
         end if;
         raise;

   end Add_Faction;

   ---------------------
   -- Create_Scenario --
   ---------------------

   procedure Create_Scenario is
      Database_Open : Boolean := False;
   begin
      Accord.Db.Database.Create;
      Database_Open := True;

      Concorde.Configure.Initialize_Database;

      Concorde.Configure.Scenarios.Load_Scenario
        (Scenario_Name  => Concorde.Options.Scenario,
         Name_Generator => Name_Generator);

      Accord.Db.Database.Close;
      Database_Open := False;

   exception

      when others =>
         if Database_Open then
            Accord.Db.Database.Close;
         end if;
         raise;

   end Create_Scenario;

   ----------------------------
   -- Have_Required_Argument --
   ----------------------------

   function Have_Required_Argument
     (Argument_Name  : String;
      Argument_Value : String)
      return Boolean
   is
   begin
      if Argument_Value = "" then
         Ada.Text_IO.Put_Line
           ("missing required argument: --" & Argument_Name);
         return False;
      else
         return True;
      end if;
   end Have_Required_Argument;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Server_Start_Time := Ada.Calendar.Clock;

      if not Ada.Directories.Exists (".concorde-options") then
         Ada.Directories.Copy_File
           (Source_Name => Concorde.Paths.Config_File ("default-options.txt"),
            Target_Name => ".concorde-options");
      end if;

      WL.Command_Line.Load_Defaults (".concorde-options");

      WL.Localisation.Read_Localisation
        (Concorde.Paths.Config_File
           ("localisation/" & Concorde.Options.Language & ".txt"));

      WL.Random.Names.Load_Lexicon
        (Name_Generator,
         Concorde.Paths.Config_File ("totro-vowels.txt"),
         Concorde.Paths.Config_File ("totro-consonants.txt"));

      if Concorde.Options.Randomise then
         WL.Random.Randomise;
      end if;

      Concorde.Random_Names.Load_Names;

   end Initialize;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      null;
      --  Concorde.Markets.Load_Markets;
      --  Concorde.Managers.Execution.Load_Managers;
   end Start;

   ----------------
   -- Start_Time --
   ----------------

   function Start_Time return Ada.Calendar.Time is
   begin
      return Server_Start_Time;
   end Start_Time;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      null;
   end Stop;

end Concorde.Server;
