with Concorde.UI.Entities.Directories;

package body Concorde.UI.Entities.Top is

--     type Top_Entity_Type is new Branch_Node with
--        record
--           null;
--        end record;
--
--     overriding function Has_Child
--       (Node : Top_Entity_Type;
--        Name : String)
--        return Boolean;
--
--     overriding function Get_Child
--       (Node  : Top_Entity_Type;
--        Child : String)
--        return Nazar.Interfaces.Hierarchy.Node_Reference_Class
--     is (raise Constraint_Error
--           with "/: get-child [" & Child & "]: not implemented yet");
--
--     overriding procedure Bind_Child
--       (Node  : in out Top_Entity_Type;
--        Name  : String;
--        Child : Nazar.Interfaces.Hierarchy.Node_Reference_Class)
--     is null;
--
--     overriding procedure Delete_Child
--       (Node   : in out Top_Entity_Type;
--        Name   : String)
--     is null;
--
--     overriding procedure Iterate_Children
--       (Node    : Top_Entity_Type;
--        Process : not null access
--          procedure (Name : String;
--                 Child : Nazar.Interfaces.Hierarchy.Node_Reference_Class))
--     is null;

   Local_Top_Entity : constant
     Nazar.Interfaces.Hierarchy.Node_Interface'Class :=
       Directories.Directory_Node;

   type Top_Entity_Reference is
     new Nazar.Interfaces.Hierarchy.Node_Reference_Interface with
      record
         null;
      end record;

   overriding function Is_Empty
     (Reference : Top_Entity_Reference)
      return Boolean
   is (False);

   overriding function Get
     (Reference : Top_Entity_Reference)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (Local_Top_Entity);

   overriding function Update
     (Node : Top_Entity_Reference)
      return access Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (raise Constraint_Error with
         "update: not implemented yet");

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Top_Entity_Type;
      Name : String)
      return Boolean
   is
      pragma Unreferenced (Node, Name);
   begin
      return False;
   end Has_Child;

   ----------------
   -- Top_Entity --
   ----------------

   function Top_Entity
     return Nazar.Interfaces.Hierarchy.Node_Reference_Class
   is
   begin
      return Top_Entity_Reference'(null record);
   end Top_Entity;

end Concorde.UI.Entities.Top;
