with Ada.Strings.Unbounded;

with Concorde.UI.Entities.Store;

package body Concorde.UI.Entities.Files is

   type File_Node_Record is
     new Leaf_Node with
      record
         Contents : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Contents
     (Node : File_Node_Record)
      return String
   is (Ada.Strings.Unbounded.To_String (Node.Contents));

   type Dynamic_File_Node_Record is
     new Leaf_Node with
      record
         Fn : File_Contents_Function;
      end record;

   overriding function Contents
     (Node : Dynamic_File_Node_Record)
      return String
   is (Node.Fn.all);

   -----------------------
   -- Dynamic_File_Node --
   -----------------------

   function Dynamic_File_Node
     (Contents : File_Contents_Function) return Entity_Reference
   is
   begin
      return Store.Save_Node
        (Dynamic_File_Node_Record'
           (Fn => Contents));
   end Dynamic_File_Node;

   ---------------
   -- File_Node --
   ---------------

   function File_Node (Contents : String := "") return Entity_Reference
   is
   begin
      return Store.Save_Node
        (File_Node_Record'
           (Contents =>
                Ada.Strings.Unbounded.To_Unbounded_String (Contents)));
   end File_Node;

end Concorde.UI.Entities.Files;
