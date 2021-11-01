private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Accord.Has_Attributes;

package Concorde.Calculations is

   type Calculation is private;

   function Calculate
     (Tag   : String;
      Start : Integer)
      return Calculation;

   procedure Apply
     (Item           : in out Calculation;
      Tag            : String;
      Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Attribute_Name : String;
      Add_Factor     : Natural := 1);

   procedure Add
     (To    : in out Calculation;
      Tag   : String;
      Value : Integer);

   procedure Subtract
     (From  : in out Calculation;
      Tag   : String;
      Value : Natural);

   procedure Modify
     (Item   : in out Calculation;
      Tag    : String;
      Factor : Real);

   function Total
     (Item : Calculation)
      return Integer;

   function Show
     (Item      : Calculation)
      return String;

private

   type Add_Element_Type is
      record
         Tag   : Ada.Strings.Unbounded.Unbounded_String;
         Value : Integer;
         Count : Positive;
      end record;

   package Add_Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Add_Element_Type);

   type Multiply_Element_Type is
      record
         Tag   : Ada.Strings.Unbounded.Unbounded_String;
         Value : Real;
      end record;

   package Multiply_Element_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Multiply_Element_Type);

   type Calculation is
      record
         Tag        : Ada.Strings.Unbounded.Unbounded_String;
         Start      : Integer;
         Adds       : Add_Element_Lists.List;
         Multiplies : Multiply_Element_Lists.List;
      end record;

end Concorde.Calculations;
