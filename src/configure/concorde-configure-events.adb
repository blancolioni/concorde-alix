with Ada.Directories;

with WL.String_Maps;

with Tropos.Reader;

with Accord.Ability;
with Accord.Change_Ability;
with Accord.Gain_Skill;
with Accord.Child_Event;
with Accord.Event;
with Accord.Event_Choice;
with Accord.Skill;

with Accord.Db;

package body Concorde.Configure.Events is

   type Configure_Procedure is access
     procedure (Choice : Accord.Event_Choice.Event_Choice_Class;
                Config : Tropos.Configuration);

   package Configure_Maps is
     new WL.String_Maps (Configure_Procedure);

   Configure_Table : Configure_Maps.Map;

   procedure Load_Configurers;

   procedure Configure_Effect
     (Choice        : Accord.Event_Choice.Event_Choice_Class;
      Effect_Config : Tropos.Configuration);

   procedure Configure_Event
     (Event_Config : Tropos.Configuration);

   procedure Configure_Ability_Change
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Configure_Ability_Category_Change
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Configure_Skill_Change
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Configure_Child_Event
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration);

   procedure Get_Change_Range
     (Change : String;
      Low    : out Integer;
      High   : out Integer);

   ---------------------------------------
   -- Configure_Ability_Category_Change --
   ---------------------------------------

   procedure Configure_Ability_Category_Change
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
      Tag : constant String := Config.Config_Name;
      Category : constant Accord.Db.Ability_Category :=
                   (if Tag = "physical-ability"
                    then Accord.Db.Physical
                    elsif Tag = "mental-ability"
                    then Accord.Db.Mental
                    elsif Tag = "social-ability"
                    then Accord.Db.Social
                    else raise Constraint_Error with
                    "unknown category: " & Tag);

      Value : constant String := Config.Value;
      Low   : Integer;
      High  : Integer;
   begin
      Get_Change_Range (Value, Low, High);
      Accord.Change_Ability.Create
        (Event_Choice => Choice,
         Category     => Category,
         Ability      => Accord.Ability.Empty_Handle,
         Low          => Low,
         High         => High);
   end Configure_Ability_Category_Change;

   ------------------------------
   -- Configure_Ability_Change --
   ------------------------------

   procedure Configure_Ability_Change
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
      Value : constant String := Config.Value;
      Low   : Integer;
      High  : Integer;
   begin
      Get_Change_Range (Value, Low, High);
      Accord.Change_Ability.Create
        (Event_Choice => Choice,
         Category     => Accord.Db.Physical,
         Ability      =>
           Accord.Ability.Get_By_Tag
             (Config.Config_Name),
         Low          => Low,
         High         => High);
   end Configure_Ability_Change;

   ---------------------------
   -- Configure_Child_Event --
   ---------------------------

   procedure Configure_Child_Event
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
   begin
      Accord.Child_Event.Create
        (Event_Choice => Choice,
         Tag          => Config.Value,
         Redirect     => Config.Config_Name = "redirect");
   end Configure_Child_Event;

   ----------------------
   -- Configure_Effect --
   ----------------------

   procedure Configure_Effect
     (Choice        : Accord.Event_Choice.Event_Choice_Class;
      Effect_Config : Tropos.Configuration)
   is
      Tag : constant String := Effect_Config.Config_Name;
   begin
      if Configure_Table.Contains (Tag) then
         Configure_Table.Element (Tag) (Choice, Effect_Config);
      else
         raise Constraint_Error with
           "no such effect: " & Tag;
      end if;
   end Configure_Effect;

   ---------------------
   -- Configure_Event --
   ---------------------

   procedure Configure_Event
     (Event_Config : Tropos.Configuration)
   is
      Event : constant Accord.Event.Event_Handle :=
                Accord.Event.Create
                  (Tag           => "evt-" & Event_Config.Config_Name,
                   Heading       =>
                     Event_Config.Get ("heading", Event_Config.Config_Name),
                   Text          =>
                     Event_Config.Get ("text", "default-event-text"),
                   Random_Choice => Event_Config.Contains ("random"),
                   Auto_Execute  => Event_Config.Contains ("auto"));
   begin
      for Choice_Config of Event_Config.Child ("choices") loop
         declare
            Choice               : constant Accord.Event_Choice
              .Event_Choice_Handle :=
                       Accord.Event_Choice.Create
                         (Event => Event,
                          Tag   => Choice_Config.Config_Name);
         begin
            for Effect_Config of Choice_Config loop
               Configure_Effect (Choice, Effect_Config);
            end loop;
         end;
      end loop;

   end Configure_Event;

   ----------------------
   -- Configure_Events --
   ----------------------

   procedure Configure_Events (Scenario_Name : String) is
      Path : constant String :=
               Scenario_Directory
                 (Scenario_Name  => Scenario_Name,
                  Directory_Name => "careers");
   begin
      Load_Configurers;
      Tropos.Reader.Read_Config
        (Path      => Ada.Directories.Compose (Path, "events"),
         Extension => "event",
         Configure => Configure_Event'Access);
   end Configure_Events;

   ----------------------------
   -- Configure_Skill_Change --
   ----------------------------

   procedure Configure_Skill_Change
     (Choice : Accord.Event_Choice.Event_Choice_Class;
      Config : Tropos.Configuration)
   is
   begin
      Accord.Gain_Skill.Create
        (Event_Choice => Choice,
         Skill        =>
           Accord.Skill.Get_By_Tag
             (Config.Config_Name),
         Level        => Config.Value);
   end Configure_Skill_Change;

   ----------------------
   -- Get_Change_Range --
   ----------------------

   procedure Get_Change_Range
     (Change : String;
      Low    : out Integer;
      High   : out Integer)
   is
   begin
      Low :=
        (if Change = "-d6" then -6
         elsif Change = "d6" then 1
         else Integer'Value (Change));
      High :=
        (if Change = "-d6" then -1
         elsif Change = "d6" then 6
         else Integer'Value (Change));
   end Get_Change_Range;

   ----------------------
   -- Load_Configurers --
   ----------------------

   procedure Load_Configurers is
   begin
      for Ability of
        Accord.Ability.Scan_By_Tag
      loop
         Configure_Table.Insert
           (Ability.Tag,
            Configure_Ability_Change'Access);
      end loop;

      for Skill of
        Accord.Skill.Scan_By_Tag
      loop
         Configure_Table.Insert
           (Skill.Tag,
            Configure_Skill_Change'Access);
      end loop;

      Configure_Table.Insert
        ("physical-ability", Configure_Ability_Category_Change'Access);
      Configure_Table.Insert
        ("mental-ability", Configure_Ability_Category_Change'Access);
      Configure_Table.Insert
        ("social-ability", Configure_Ability_Category_Change'Access);
      Configure_Table.Insert
        ("event", Configure_Child_Event'Access);
      Configure_Table.Insert
        ("redirect", Configure_Child_Event'Access);

   end Load_Configurers;

end Concorde.Configure.Events;
