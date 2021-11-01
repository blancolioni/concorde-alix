with Accord.Has_Name;

generic
   type Container_Handle is private;
   type Record_Handle is
     new Accord.Has_Name.Has_Name_Interface with private;
   with function Get_By_Name
     (Handle : Container_Handle;
      Name   : String)
      return Record_Handle;
   with procedure Iterate
     (Handle  : Container_Handle;
      Process : not null access
        procedure (Rec : Record_Handle));
   with function Contents
     (Item : Record_Handle)
     return String;
   --  with function "=" (Left, Right : Record_Handle) return Boolean
   --    is <>;
package Concorde.UI.Entities.Generic_Database_Entity is

   function Get_Record_Node
     (Reference : Record_Handle)
      return Entity_Reference;

   function Get_Container_Node
     (Handle : Container_Handle)
      return Entity_Reference;

end Concorde.UI.Entities.Generic_Database_Entity;
