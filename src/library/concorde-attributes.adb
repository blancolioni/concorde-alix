with Accord.Attribute;
with Accord.Attribute_Value;

package body Concorde.Attributes is

   function Attribute_Value
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String)
      return Accord.Attribute_Value.Attribute_Value_Class;

   ---------------------
   -- Attribute_Value --
   ---------------------

   function Attribute_Value
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String)
      return Accord.Attribute_Value.Attribute_Value_Class
   is
      Attribute : constant Accord.Attribute.Attribute_Handle :=
                    Accord.Attribute.Get_By_Tag (Name);
   begin
      pragma Assert (Attribute.Has_Element, "no such attribute: " & Name);
      return Accord.Attribute_Value.Get_By_Attribute_Value
        (Has_Attributes, Attribute);
   end Attribute_Value;

   --------------
   -- Decrease --
   --------------

   procedure Decrease
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Amount         : Natural)
   is
      Old_Value : constant Integer := Get (Has_Attributes, Name);
   begin
      Set (Has_Attributes, Name, Old_Value - Amount);
   end Decrease;

   ---------
   -- Get --
   ---------

   function Get
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String)
      return Integer
   is
      Value : constant Accord.Attribute_Value.Attribute_Value_Class :=
                Attribute_Value (Has_Attributes, Name);
   begin
      if Value.Has_Element then
         return Value.Add;
      else
         return 0;
      end if;
   end Get;

   --------------------
   -- Get_Multiplier --
   --------------------

   function Get_Multiplier
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String) return Real
   is
      Value : constant Accord.Attribute_Value.Attribute_Value_Class :=
                Attribute_Value (Has_Attributes, Name);
   begin
      if Value.Has_Element then
            return Value.Multiply;
      else
         return 1.0;
      end if;
   end Get_Multiplier;

   --------------
   -- Increase --
   --------------

   procedure Increase
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Amount         : Natural)
   is
      Old_Value : constant Integer := Get (Has_Attributes, Name);
   begin
      Set (Has_Attributes, Name, Old_Value + Amount);
   end Increase;

   ---------
   -- Set --
   ---------

   procedure Set
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Value          : Integer)
   is
      Rec : constant Accord.Attribute_Value.Attribute_Value_Class :=
              Attribute_Value (Has_Attributes, Name);
   begin
      if Rec.Has_Element then
         Rec.Update_Attribute_Value
           .Set_Add (Value)
           .Done;
      elsif Value /= 0 then
         Accord.Attribute_Value.Create
           (Has_Attributes => Has_Attributes,
            Attribute      => Accord.Attribute.Get_By_Tag (Name),
            Add            => Value,
            Multiply       => 1.0);
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String;
      Multiplier     : Real)
   is
      Rec : constant Accord.Attribute_Value.Attribute_Value_Class :=
              Attribute_Value (Has_Attributes, Name);
   begin
      if Rec.Has_Element then
         Rec.Update_Attribute_Value
           .Set_Multiply (Multiplier)
           .Done;
      elsif Multiplier /= 1.0 then
         Accord.Attribute_Value.Create
           (Has_Attributes => Has_Attributes,
            Attribute      => Accord.Attribute.Get_By_Tag (Name),
            Add            => 0,
            Multiply       => Multiplier);
      end if;
   end Set;

end Concorde.Attributes;
