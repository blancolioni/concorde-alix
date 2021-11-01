private with Ada.Containers.Vectors;

with Tropos;

with Accord.Resource;
with Accord.World;

package Concorde.Configure.Resources is

   procedure Configure_Atmosphere_Components
     (Config : Tropos.Configuration);

   type Random_Deposit_Generator is private;

   function Create_Generator
     (X, Y, Z : Real)
      return Random_Deposit_Generator;

   procedure Create_Resource_Spheres
     (System_Count  : Positive;
      R_X, R_Y, R_Z : Non_Negative_Real);

   procedure Create_Deposits
     (World     : Accord.World.World_Class;
      Generator : Random_Deposit_Generator);

private

   type Resource_Record is
      record
         Resource  : Accord.Resource.Resource_Handle;
         Strength  : Non_Negative_Real;
      end record;

   package Resource_Vectors is
     new Ada.Containers.Vectors (Positive, Resource_Record);

   type Random_Deposit_Generator is
      record
         Resources      : Resource_Vectors.Vector;
         Total_Strength : Non_Negative_Real := 0.0;
      end record;

end Concorde.Configure.Resources;
