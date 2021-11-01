with Accord.Has_Attributes;

package Concorde.Attributes is

   function Get
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String)
      return Integer;

   function Get_Multiplier
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String)
      return Real;

   procedure Set
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Value          : Integer);

   procedure Set
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Multiplier     : Real);

   procedure Increase
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Amount         : Natural);

   procedure Decrease
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Amount         : Natural);

   function Economy
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class)
      return Integer
   is (Get (Has_Attributes, "economy"));

   function Economy_Multiplier
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class)
      return Real
   is (Get_Multiplier (Has_Attributes, "economy"));

end Concorde.Attributes;
