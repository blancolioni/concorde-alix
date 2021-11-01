package body Concorde.UI.Entities.Generic_Database_Entity is

   type Table_Record_Node_Reference is
     new Nazar.Interfaces.Hierarchy.Node_Reference_Interface with
      record
         Rec : Record_Handle;
      end record;

   overriding function Is_Empty
     (Item : Table_Record_Node_Reference)
      return Boolean
   is (not Item.Rec.Has_Element);

   overriding function Get
     (Id : Table_Record_Node_Reference)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class;

   overriding function Update
     (Id : Table_Record_Node_Reference)
      return access Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (raise Constraint_Error with "read-only filesystem");

   type Table_Record_Node is
     new Leaf_Node with
      record
         Rec : Record_Handle;
      end record;

   overriding function Contents
     (Node : Table_Record_Node)
      return String;

   type Container_Node_Record is
     new Branch_Node with
      record
           Handle : Container_Handle;
      end record;

   overriding function Has_Child
     (Node : Container_Node_Record;
      Name : String)
      return Boolean;

   overriding function Get_Child
     (Node : Container_Node_Record;
      Name : String)
      return Entity_Reference;

   overriding procedure Iterate_Children
     (Node    : Container_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Entity_Reference));

   overriding procedure Bind_Child
     (Node  : in out Container_Node_Record;
      Name  : String;
      Child : Entity_Reference);

   overriding procedure Delete_Child
     (Node   : in out Container_Node_Record;
      Name   : String);

   type Container_Node_Id_Record is
     new Nazar.Interfaces.Hierarchy.Node_Reference_Interface with
      record
         Handle : Container_Handle;
      end record;

   overriding function Is_Empty
     (Node : Container_Node_Id_Record)
      return Boolean
   is (False);

   overriding function Get
     (Node : Container_Node_Id_Record)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class;

   overriding function Update
     (Id : Container_Node_Id_Record)
      return access Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is (raise Constraint_Error with "read-only filesystem");

   ----------------
   -- Bind_Child --
   ----------------

   overriding procedure Bind_Child
     (Node  : in out Container_Node_Record;
      Name  : String;
      Child : Entity_Reference)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Bind_Child;

   --------------
   -- Contents --
   --------------

   overriding function Contents (Node : Table_Record_Node) return String is
   begin
      return Contents (Node.Rec);
   end Contents;

   ------------------
   -- Delete_Child --
   ------------------

   overriding procedure Delete_Child
     (Node   : in out Container_Node_Record;
      Name   : String)
   is
   begin
      raise Constraint_Error with
        "read-only filesystem";
   end Delete_Child;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Id : Table_Record_Node_Reference)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is
   begin
      return Table_Record_Node'
        (Rec => Id.Rec);
   end Get;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Node : Container_Node_Id_Record)
      return Nazar.Interfaces.Hierarchy.Node_Interface'Class
   is
   begin
      return Container_Node_Record'
        (Handle => Node.Handle);
   end Get;

   ---------------
   -- Get_Child --
   ---------------

   overriding function Get_Child
     (Node : Container_Node_Record;
      Name : String)
      return Entity_Reference
   is
   begin
      return Table_Record_Node_Reference'
        (Rec => Get_By_Name (Node.Handle, Name));
   end Get_Child;

   ------------------------
   -- Get_Container_Node --
   ------------------------

   function Get_Container_Node
     (Handle : Container_Handle)
      return Entity_Reference
   is
   begin
      return Container_Node_Id_Record'
        (Handle => Handle);
   end Get_Container_Node;

   ------------------------
   -- Get_Record_Node_Id --
   ------------------------

   function Get_Record_Node
     (Reference : Record_Handle)
      return Entity_Reference
   is
   begin
      return Table_Record_Node_Reference'(Rec => Reference);
   end Get_Record_Node;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Node : Container_Node_Record;
      Name : String)
      return Boolean
   is
   begin
      return Get_By_Name (Node.Handle, Name).Has_Element;
   end Has_Child;

   ----------------------
   -- Iterate_Children --
   ----------------------

   overriding procedure Iterate_Children
     (Node    : Container_Node_Record;
      Process : not null access
        procedure (Name : String;
                   Child : Entity_Reference))
   is
      procedure Internal_Process (Item : Record_Handle);

      ----------------------
      -- Internal_Process --
      ----------------------

      procedure Internal_Process (Item : Record_Handle) is
      begin
         Process (Item.Name,
                  Table_Record_Node_Reference'
                    (Rec => Item));
      end Internal_Process;

   begin
      Iterate (Node.Handle, Internal_Process'Access);
   end Iterate_Children;

end Concorde.UI.Entities.Generic_Database_Entity;
