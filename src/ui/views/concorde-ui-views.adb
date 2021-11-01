package body Concorde.UI.Views is

   type Partial_View_Type is
     new View_Interface with
      record
         Object            : View_Object_Holders.Holder;
         Partial_View_Port : View_Port;
      end record;

   overriding procedure Draw
     (View   : in out Partial_View_Type);

   ---------
   -- Add --
   ---------

   procedure Add
     (List : in out Draw_Command_List'Class; Command : Draw_Command)
   is
   begin
      List.Commands.Append (Command);
   end Add;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (List    : Draw_Command_List'Class;
      Process : not null access
        procedure (Command : Draw_Command'Class))
   is
   begin
      for Command of List.Commands loop
         Process (Command);
      end loop;
   end Iterate;

end Concorde.UI.Views;
