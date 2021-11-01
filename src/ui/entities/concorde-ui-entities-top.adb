with Ada.Calendar.Formatting;

with Nazar.Interfaces.Text_Writer;

with Concorde.UI.Entities.Directories;
with Concorde.UI.Entities.Files;

with Concorde.UI.Entities.Home;

with Concorde.Server;

with Marlowe.Version;
with Kit.Version;
with Concorde.Version;

package body Concorde.UI.Entities.Top is

   function Show_Uptime return String
   is (Ada.Calendar.Formatting.Image
       (Ada.Calendar."-"
          (Ada.Calendar.Clock, Concorde.Server.Start_Time)));

   -----------------------
   -- Create_Top_Entity --
   -----------------------

   function Create_Top_Entity
     return Entity_Reference
   is
      function Line (S : String) return String
                     renames Nazar.Interfaces.Text_Writer.Line;

      Top : constant Nazar.Interfaces.Hierarchy.Node_Reference_Class :=
        Directories.Create_Directory_Node;
   begin
      Top.Update.Bind_Child
        ("version",
         Files.File_Node
           (Line
                (Concorde.Version.Name
                 & " " & Concorde.Version.Version_String)
            & Line
              (Kit.Version.Component_Name
               & " " & Kit.Version.Version_String)
            & Line
              (Marlowe.Version.Component_Name
               & " " & Marlowe.Version.Version_String)));

      Top.Update.Bind_Child
        ("uptime",
         Files.Dynamic_File_Node (Show_Uptime'Access));
      Top.Update.Bind_Child
        ("home", Home.Home_Node);
      Top.Update.Bind_Child
        ("proc",
         Directories.Create_Directory_Node);
      Top.Update.Bind_Child
        ("etc",
         Directories.Create_Directory_Node);
      Top.Update.Bind_Child
        ("log",
         Directories.Create_Directory_Node);
      Top.Update.Bind_Child
        ("tmp",
         Directories.Create_Directory_Node);
      return Read_Only_Entity_Reference'
        (RW_Reference => Node_Reference_Holders.To_Holder (Top));
   end Create_Top_Entity;

end Concorde.UI.Entities.Top;
