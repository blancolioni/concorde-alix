with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

with WL.String_Maps;

with Concorde.UI.Entities.Store;

package body Concorde.UI.Entities.Directories is

   type Child_Node_Record is
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Node : Node_Reference_Holders.Holder;
      end record;

   package Child_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Child_Node_Record);

   package Child_Node_Maps is
     new WL.String_Maps
       (Nazar.Interfaces.Hierarchy.Node_Reference_Interface'Class,
        Nazar.Interfaces.Hierarchy."=");

   type Directory_Record is
     new Branch_Node with
      record
         Child_List : Child_Node_Lists.List;
         Child_Map  : Child_Node_Maps.Map;
      end record;

   overriding function Has_Child
     (Node : Directory_Record;
      Name : String)
      return Boolean
   is (Node.Child_Map.Contains (Name));

   overriding function Get_Child
     (Node  : Directory_Record;
      Child : String)
      return Entity_Reference
   is (Node.Child_Map.Element (Child));

   overriding procedure Iterate_Children
     (Node    : Directory_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Entity_Reference));

   overriding procedure Bind_Child
     (Node  : in out Directory_Record;
      Name  : String;
      Child : Entity_Reference);

   overriding procedure Delete_Child
     (Node   : in out Directory_Record;
      Name   : String);

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Directory_Record;
      Name  : String;
      Child : Entity_Reference)
   is
   begin
      Node.Child_List.Append
        (Child_Node_Record'
           (Name => Ada.Strings.Unbounded.To_Unbounded_String (Name),
            Node => Node_Reference_Holders.To_Holder (Child)));
      Node.Child_Map.Insert
        (Name, Child);
   end Bind_Child;

   ---------------------------
   -- Create_Directory_Node --
   ---------------------------

   function Create_Directory_Node
     return Entity_Reference
   is
   begin
      return Store.Save_Node
        (Node => Directory_Record'(others => <>));
   end Create_Directory_Node;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Directory_Record;
      Name   : String)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
      Position : Child_Node_Lists.Cursor := Node.Child_List.First;
   begin
      while Child_Node_Lists.Element (Position).Name /= Name loop
         Child_Node_Lists.Next (Position);
      end loop;

      Node.Child_List.Delete (Position);
      Node.Child_Map.Delete (Name);
   end Delete_Child;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Directory_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Entity_Reference))
   is
   begin
      for Child of Node.Child_List loop
         Process (Ada.Strings.Unbounded.To_String (Child.Name),
                  Child.Node.Element);
      end loop;
   end Iterate_Children;

end Concorde.UI.Entities.Directories;
