package Concorde.Logging is

   procedure Log
     (Actor    : String;
      Location : String;
      Category : String;
      Message  : String);

   procedure Log
     (Category : String;
      Message  : String);

   procedure Start_Logging
     (Log_Name : String := "");

   procedure Stop_Logging;

   procedure Start_Update;
   procedure Finish_Update;

end Concorde.Logging;
