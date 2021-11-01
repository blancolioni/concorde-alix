with WL.Random.Names;

package Concorde.Configure.Galaxies is

   type Biased_Coordinate_Generator is access
     procedure (X, Y, Z : out Signed_Unit_Real);

   procedure Random_Centre_Bias_Sphere_Distribution
     (X, Y, Z : out Signed_Unit_Real);

   procedure Random_Sphere_Distribution
     (X, Y, Z : out Signed_Unit_Real);

   procedure Random_Cube_Distribution
     (X, Y, Z : out Signed_Unit_Real);

   type Progress_Interface is synchronized interface;

   procedure Add_Work_Item
     (Progress   : in out Progress_Interface;
      Identifier : String;
      Step_Count : Natural)
   is abstract;

   procedure Step
     (Progress : in out Progress_Interface)
   is abstract;

   procedure Finish
     (Progress : in out Progress_Interface)
   is abstract;

   procedure Generate_Galaxy
     (Number_Of_Systems  : Positive;
      Radius_X           : Non_Negative_Real;
      Radius_Y           : Non_Negative_Real;
      Radius_Z           : Non_Negative_Real;
      Create_Coordinates : Biased_Coordinate_Generator;
      Names              : WL.Random.Names.Name_Generator;
      Progress           : in out Progress_Interface'Class);

end Concorde.Configure.Galaxies;
