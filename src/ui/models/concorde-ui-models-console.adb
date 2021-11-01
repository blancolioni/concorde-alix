with Ada.Calendar.Formatting;

with Nazar.Models.Environment;

with Concorde.Options;
with Concorde.Version;

with Concorde.Commands;

with Concorde.Server;

with Concorde.UI.Entities;

package body Concorde.UI.Models.Console is

   -------------------
   -- Console_Model --
   -------------------

   function Console_Model
     (Default_Scope : String)
      return Concorde_Console_Model
   is
      use Nazar.Models.Environment;
      Environment : constant Nazar_Environment_Model :=
                      new Root_Environment_Model;

      procedure Env (Name, Value : String);

      ---------
      -- Env --
      ---------

      procedure Env (Name, Value : String) is
      begin
         Environment.Set_Environment_Value (Name, Value);
      end Env;

   begin
      Env ("LANG", Concorde.Options.Language);
      Env ("START_TIME",
           Ada.Calendar.Formatting.Image
             (Concorde.Server.Start_Time));
      Env ("VERSION", Concorde.Version.Version_String);
      Env ("HOME", Default_Scope);

      return Model : constant Concorde_Console_Model :=
        new Root_Console_Model
      do
         Model.Initialize
           (Concorde.UI.Entities.Root, Environment, Default_Scope);

         Model.Set_Command
           (Concorde.Commands.Set_Time_Acceleration);
         Model.Set_Command
           (Concorde.Commands.Start_Updates);
         Model.Set_Command
           (Concorde.Commands.Stop_Updates);

         Model.Put_Line
           ("Concorde " & Concorde.Version.Version_String);
      end return;
   end Console_Model;

end Concorde.UI.Models.Console;
