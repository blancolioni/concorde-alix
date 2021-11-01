with Nazar.Models.Text;

package Concorde.UI.Models is

   type Regular_Root_Update_Type is interface;

   procedure Refresh
     (Model    : in out Regular_Root_Update_Type;
      Continue : out Boolean)
   is abstract;

   function Interval
     (Model : Regular_Root_Update_Type)
      return Concorde_Duration
      is abstract;

   procedure Start_Updates
     (Model    : not null access Regular_Root_Update_Type'Class);

   type Dynamic_Text_Model is
     abstract new Nazar.Models.Text.Nazar_Text_Model_Record
     and Regular_Root_Update_Type
   with private;

   overriding procedure Refresh
     (Model    : in out Dynamic_Text_Model;
      Continue : out Boolean);

   overriding function Interval
     (Model : Dynamic_Text_Model)
      return Concorde_Duration;

   procedure Initialize
     (Model        : not null access Dynamic_Text_Model'Class;
      Initial_Text : String;
      Interval     : Concorde_Duration);

   function Current_Text
     (Model : Dynamic_Text_Model)
      return String
      is abstract;

private

   type Dynamic_Text_Model is
     abstract new Nazar.Models.Text.Nazar_Text_Model_Record
     and Regular_Root_Update_Type with
      record
         Interval : Concorde_Duration;
      end record;

   overriding function Interval
     (Model : Dynamic_Text_Model)
      return Concorde_Duration
   is (Model.Interval);

end Concorde.UI.Models;
