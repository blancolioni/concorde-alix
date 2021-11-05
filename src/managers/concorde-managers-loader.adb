with Concorde.Colonies.Managers;

package body Concorde.Managers.Loader is

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
   begin
      Add_Manager
        ("colony-monthly",
         Concorde.Colonies.Managers.Create_Monthly_Update'Access);
      Add_Manager
        ("colony-daily",
         Concorde.Colonies.Managers.Create_Daily_Update'Access);
   end Load_Managers;

end Concorde.Managers.Loader;
