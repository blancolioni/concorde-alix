with Ada.Strings.Unbounded;

with Concorde.UI.Entities.Top;

package body Concorde.UI.Entities is

   Root_Holder : Node_Reference_Holders.Holder;

   overriding function Get_Child
     (Node  : Leaf_Node;
      Child : String)
      return Nazar.Interfaces.Hierarchy.Node_Reference_Class
   is (raise Constraint_Error with
         "Get_Child called on leaf");

   overriding function Has_Child
     (Node : Leaf_Node;
      Name : String)
      return Boolean
   is (False);

   overriding function Is_Leaf
     (Node : Leaf_Node)
      return Boolean
   is (True);

   overriding function Is_Leaf
     (Node : Branch_Node)
      return Boolean
   is (False);

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Leaf_Node;
      Name  : String;
      Child : Entity_Reference)
   is
      pragma Unreferenced (Node, Child);
   begin
      raise Constraint_Error with
        "bind-child [" & Name & "] called on leaf node";
   end Bind_Child;

   --------------
   -- Contents --
   --------------

   overriding function Contents
     (Node : Branch_Node)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;

      procedure Add_Child
        (Name  : String;
         Child : Entity_Reference);

      ---------------
      -- Add_Child --
      ---------------

      procedure Add_Child
        (Name  : String;
         Child : Entity_Reference)
      is
         pragma Unreferenced (Child);
      begin
         Append (Result, Name);
         Append (Result, Character'Val (10));
      end Add_Child;

   begin
      Branch_Node'Class (Node).Iterate_Children (Add_Child'Access);
      return To_String (Result);
   end Contents;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Leaf_Node;
      Name   : String)
   is
      pragma Unreferenced (Node);
   begin
      raise Constraint_Error with
        "delete-child [" & Name & " called on leaf node";
   end Delete_Child;

   ----------
   -- Root --
   ----------

   function Root
     return Entity_Reference
   is
   begin
      if Root_Holder.Is_Empty then
         Root_Holder :=
           Node_Reference_Holders.To_Holder (Top.Create_Top_Entity);
      end if;
      return Root_Holder.Element;
   end Root;

end Concorde.UI.Entities;
