with Ada.Containers.Indefinite_Vectors;

with Nazar.Interfaces.Work;
with Concorde.Configure.Galaxies;

package Concorde.Configure.Tasks is

   type Work_Item (Name_Length : Natural) is
      record
         Name : String (1 .. Name_Length);
         Step_Count : Natural;
      end record;

   package Work_Item_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, Work_Item);

   protected type Configuration_Work is
        new Concorde.Configure.Galaxies.Progress_Interface
        and Nazar.Interfaces.Work.Work_Interface with

      overriding procedure Add_Work_Item
        (Identifier : String;
         Step_Count : Natural);

      overriding procedure Step;

      overriding procedure Finish;

      overriding procedure Get_State
        (Current : out Natural;
         Total   : out Natural);

      function Current_Work_Item return String;
      function Finished return Boolean;

   private

      Work_Items : Work_Item_Vectors.Vector;
      Total_Steps : Natural  := 0;
      Current_Step : Natural := 0;
      Current_Item : Natural := 1;
      Current_Item_Step : Natural := 0;
      Finished_Flag : Boolean := False;
   end Configuration_Work;

end Concorde.Configure.Tasks;
