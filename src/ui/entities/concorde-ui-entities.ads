private with Ada.Containers.Indefinite_Holders;

with Nazar.Interfaces.Hierarchy;

package Concorde.UI.Entities is

   subtype Entity_Reference is
     Nazar.Interfaces.Hierarchy.Node_Reference_Interface'Class;

   type Leaf_Node is
     abstract new Nazar.Interfaces.Hierarchy.Node_Interface with null record;

   overriding function Is_Leaf
     (Node : Leaf_Node)
      return Boolean;

   overriding function Has_Child
     (Node : Leaf_Node;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node  : Leaf_Node;
      Child : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class;

   overriding procedure Bind_Child
     (Node  : in out Leaf_Node;
      Name  : String;
      Child : Entity_Reference);

   overriding procedure Delete_Child
     (Node   : in out Leaf_Node;
      Name   : String);

   overriding procedure Iterate_Children
     (Node    : Leaf_Node;
      Process : not null access
        procedure (Name : String;
                   Child : Entity_Reference))
   is null;

   type Branch_Node is
     abstract new Nazar.Interfaces.Hierarchy.Node_Interface with null record;

   overriding function Is_Leaf
     (Node : Branch_Node)
      return Boolean;

   overriding function Contents
     (Node : Branch_Node)
      return String;

   function Root
     return Entity_Reference;

private

   package Node_Reference_Holders is
     new Ada.Containers.Indefinite_Holders
       (Nazar.Interfaces.Hierarchy.Node_Reference_Interface'Class,
        Nazar.Interfaces.Hierarchy."=");

   type Read_Only_Entity_Reference is
     new Nazar.Interfaces.Hierarchy.Node_Reference_Interface with
      record
         RW_Reference : Node_Reference_Holders.Holder;
      end record;

   overriding function Is_Empty
     (Reference : Read_Only_Entity_Reference)
      return Boolean
   is (Reference.RW_Reference.Element.Is_Empty);

   overriding function Get
     (Id : Read_Only_Entity_Reference)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (Id.RW_Reference.Element.Get);

   overriding function Update
     (Node : Read_Only_Entity_Reference)
      return access Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (raise Constraint_Error with "read-only entity");

end Concorde.UI.Entities;
