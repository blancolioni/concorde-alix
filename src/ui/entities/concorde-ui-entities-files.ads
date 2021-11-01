package Concorde.UI.Entities.Files is

   function File_Node
     (Contents : String := "")
      return Entity_Reference;

   type File_Contents_Function is
     access function return String;

   function Dynamic_File_Node
     (Contents : File_Contents_Function)
      return Entity_Reference;

end Concorde.UI.Entities.Files;
