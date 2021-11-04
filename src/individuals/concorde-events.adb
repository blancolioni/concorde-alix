with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;

with WL.Random;

with Concorde.Logging;

with Concorde.Individuals;

with Accord.Ability;
with Accord.Ability_Score;
with Accord.Change_Ability;
with Accord.Child_Event;
with Accord.Event_Choice;
with Accord.Event_Effect;
with Accord.Gain_Skill;
with Accord.Skill;

with Accord.Db;

package body Concorde.Events is

   package Choice_Vectors is
     new Ada.Containers.Vectors
       (Positive, Accord.Event_Choice.Event_Choice_Handle,
        Accord.Event_Choice."=");

   procedure Log
     (Event   : Accord.Event.Event_Class;
      Target  : Accord.Individual.Individual_Class;
      Message : String);

   procedure Log
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class;
      Message : String);

   procedure Execute_Effect
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class);

   type Effect_Handler is access
     procedure (Effect  : Accord.Event_Effect.Event_Effect_Class;
                Target  : Accord.Individual.Individual_Class);

   procedure Handle_Child_Event
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class);

   procedure Handle_Change_Ability
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class);

   procedure Handle_Gain_Skill
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class);

   Effect_Handlers : constant array (Accord.Db.Record_Type) of Effect_Handler
     := (Accord.Db.R_Child_Event    => Handle_Child_Event'Access,
         Accord.Db.R_Change_Ability => Handle_Change_Ability'Access,
         Accord.Db.R_Gain_Skill     => Handle_Gain_Skill'Access,
         others                       => null);

   --------------------
   -- Execute_Effect --
   --------------------

   procedure Execute_Effect
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class)
   is
      Handler : constant Effect_Handler := Effect_Handlers (Effect.Top_Record);
   begin
      if Handler = null then
         raise Constraint_Error with
           "no handler for effect " & Effect.Top_Record'Image;
      end if;

      Handler (Effect, Target);
   end Execute_Effect;

   -------------------
   -- Execute_Event --
   -------------------

   procedure Execute_Event
     (Event  : Accord.Event.Event_Class;
      Target : Accord.Individual.Individual_Class)
   is
      Choices : Choice_Vectors.Vector;
   begin

      Log (Event, Target, "executing");

      for Choice of
        Accord.Event_Choice.Select_By_Event (Event)
      loop
         Choices.Append (Choice.To_Event_Choice_Handle);
      end loop;

      if Choices.Is_Empty then
         Log (Event, Target, "done");
      else
         declare
            Chosen : constant Positive :=
                       (if Event.Random_Choice
                        then WL.Random.Random_Number (1, Choices.Last_Index)
                        else WL.Random.Random_Number (1, Choices.Last_Index));
         begin
            for Effect of
              Accord.Event_Effect.Select_By_Event_Choice
                (Choices (Chosen))
            loop
               Execute_Effect (Effect, Target);
            end loop;
         end;
      end if;
   end Execute_Event;

   ---------------------------
   -- Handle_Change_Ability --
   ---------------------------

   procedure Handle_Change_Ability
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class)
   is
      Change : constant Accord.Change_Ability.Change_Ability_Handle :=
                 Accord.Change_Ability.Get_From_Event_Effect (Effect);
      Ability : Accord.Ability.Ability_Handle :=
                  Change.Ability.To_Ability_Handle;
   begin
      if not Ability.Has_Element then
         declare
            Highest : Natural := 0;
         begin
            for A of Accord.Ability.Select_By_Category
              (Change.Category)
            loop
               if Concorde.Individuals.Ability_Score (Target, A)
                 > Highest
               then
                  Ability := A.To_Ability_Handle;
                  Highest := Concorde.Individuals.Ability_Score
                    (Target, Ability);
               end if;
            end loop;
         end;
      end if;

      declare
         Score_Change : constant Integer :=
                          (if Change.Low = Change.High then Change.Low
                           else WL.Random.Random_Number
                             (Change.Low, Change.High));
         Current : constant Natural :=
                     Concorde.Individuals.Ability_Score
                       (Target, Ability);
         New_Score : constant Natural :=
                       Integer'Max (0, Current + Score_Change);
      begin
         Log (Effect, Target,
              Ability.Tag
              & " changed from" & Current'Image & " to" & New_Score'Image);
         Accord.Ability_Score.Update_Ability_Score
           (Accord.Ability_Score.Get_By_Ability_Score
              (Target, Ability))
             .Set_Score (New_Score)
             .Done;
      end;
   end Handle_Change_Ability;

   ------------------------
   -- Handle_Child_Event --
   ------------------------

   procedure Handle_Child_Event
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class)
   is
      Child : constant Accord.Child_Event.Child_Event_Handle :=
                Accord.Child_Event.Get_From_Event_Effect (Effect);
      Event : constant Accord.Event.Event_Handle :=
                Accord.Event.Get_By_Tag (Child.Tag);
   begin
      Log (Event, Target, "executing child event");
      Execute_Event (Event, Target);
   end Handle_Child_Event;

   -----------------------
   -- Handle_Gain_Skill --
   -----------------------

   procedure Handle_Gain_Skill
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class)
   is
      Change : constant Accord.Gain_Skill.Gain_Skill_Handle :=
                 Accord.Gain_Skill.Get_From_Event_Effect (Effect);
      Skill  : constant Accord.Skill.Skill_Class := Change.Skill;
   begin
      Log (Effect, Target,
           Skill.Tag & Change.Level'Image);
      Concorde.Individuals.Advance_Skill
        (Individual => Target,
         Skill      => Skill,
         Level      => Change.Level);
   end Handle_Gain_Skill;

   ---------
   -- Log --
   ---------

   procedure Log
     (Event   : Accord.Event.Event_Class;
      Target  : Accord.Individual.Individual_Class;
      Message : String)
   is
   begin
      Concorde.Logging.Log
        (Target.First_Name,
         Event.Tag & Ada.Characters.Latin_1.HT & Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Effect  : Accord.Event_Effect.Event_Effect_Class;
      Target  : Accord.Individual.Individual_Class;
      Message : String)
   is
      pragma Unreferenced (Effect);
   begin
      Concorde.Logging.Log
        (Target.First_Name,
         Message);
   end Log;

end Concorde.Events;
