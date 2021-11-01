with Nazar.Builder.Gtk_Creator;

with Concorde.UI.Nazar_UI;
with Concorde.UI.Text_UI;
--  with Concorde.UI.Web_UI;

package body Concorde.UI.Launch is

   ------------
   -- Get_UI --
   ------------

   function Get_UI (Name : String) return UI_Type is
   begin
      if Name = "" then
         return Concorde.UI.Text_UI.Get_Text_UI;
--        elsif Name = "aws" then
--           return Concorde.UI.Web_UI.Get_Web_UI;
      elsif Name = "text" then
         return Concorde.UI.Text_UI.Get_Text_UI;
      elsif Name = "gtk" then
         return Concorde.UI.Nazar_UI.Get_Nazar_UI
           (Nazar.Builder.Gtk_Creator.Get_Gtk_Creator);
      elsif Name = "text" then
         return Concorde.UI.Text_UI.Get_Text_UI;
      else
         raise Constraint_Error with
           "undedefined user interface: " & Name;
      end if;
   end Get_UI;

end Concorde.UI.Launch;
