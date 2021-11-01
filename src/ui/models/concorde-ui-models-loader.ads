package Concorde.UI.Models.Loader is

   function Exists (Model_Name : String) return Boolean;

   function Get
     (Model_Name : String)
      return Root_Concorde_Model'Class
     with Pre => Exists (Model_Name);

end Concorde.UI.Models.Loader;
