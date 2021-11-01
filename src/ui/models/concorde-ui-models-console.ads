with Nazar.Models.Console;

package Concorde.UI.Models.Console is

   type Root_Console_Model is
     new Nazar.Models.Console.Root_Console_Model with private;

   type Concorde_Console_Model is access all Root_Console_Model'Class;

   function Console_Model
     (Default_Scope : String)
     return Concorde_Console_Model;

private

   type Root_Console_Model is
     new Nazar.Models.Console.Root_Console_Model with
      record
         null;
      end record;

end Concorde.UI.Models.Console;
