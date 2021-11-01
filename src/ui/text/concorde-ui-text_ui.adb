with Nazar.Controllers.Console;
with Nazar.Models;
with Nazar.Views;
with Nazar.Views.Text_Console;

with Concorde.UI.Models.Console;
with Nazar.Models.Console;
with Nazar.Views.Console;

package body Concorde.UI.Text_UI is

   type Root_Text_UI is
     new Nazar.Controllers.Console.Nazar_Console_Controller_Record
     and UI_Interface with
      record
         Model : access Nazar.Models.Console.Root_Console_Model'Class;
         View  : access Nazar.Views.Console.Console_View_Interface'Class;
      end record;

   overriding procedure Start (UI : in out Root_Text_UI);
   overriding procedure Stop
     (UI      : in out Root_Text_UI;
      Message : String)
   is null;

   -----------------
   -- Get_Text_UI --
   -----------------

   function Get_Text_UI return UI_Type is
      Model : constant Concorde.UI.Models.Console.Concorde_Console_Model :=
                Concorde.UI.Models.Console.Console_Model
                  (Default_Scope => "/");
      View : constant Root_Text_UI :=
        Root_Text_UI'
          (Nazar.Controllers.Console.Nazar_Console_Controller_Record with
           Model => Model,
           View  => Nazar.Views.Text_Console.Text_Console_View (Model));
   begin
      return new Root_Text_UI'(View);
   end Get_Text_UI;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Text_UI) is
   begin
      UI.Start_Console (UI.Model, UI.View);
   end Start;

end Concorde.UI.Text_UI;
