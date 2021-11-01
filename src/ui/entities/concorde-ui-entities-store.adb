with Ada.Containers.Indefinite_Vectors;

package body Concorde.UI.Entities.Store is

   type Node_Count is new Natural;
   subtype Node_Index is Node_Count range 1 .. Node_Count'Last;

   package Node_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Node_Index,
        Nazar.Interfaces.Hierarchy.Node_Interface'Class,
        Nazar.Interfaces.Hierarchy."=");

   Node_Store : Node_Vectors.Vector;

   type Store_Node_Reference is
     new Nazar.Interfaces.Hierarchy.Node_Reference_Interface with
      record
         Index : Node_Count;
      end record;

   overriding function Is_Empty
     (Reference : Store_Node_Reference)
      return Boolean
   is (Reference.Index = 0);

   overriding function Get
     (Id : Store_Node_Reference)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (Node_Store.Element (Id.Index));

   overriding function Update
     (Node : Store_Node_Reference)
      return access Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (Node_Store.Reference (Node.Index).Element);

   ---------------
   -- Save_Node --
   ---------------

   function Save_Node
     (Node : Nazar.Interfaces.Hierarchy.Node_Interface'Class)
      return Entity_Reference
   is
   begin
      Node_Store.Append (Node);
      return Store_Node_Reference'(Index => Node_Store.Last_Index);
   end Save_Node;

end Concorde.UI.Entities.Store;
