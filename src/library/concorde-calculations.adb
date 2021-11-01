with Concorde.Attributes;
with Concorde.Real_Images;

package body Concorde.Calculations is

   ---------
   -- Add --
   ---------

   procedure Add
     (To  : in out Calculation;
      Tag   : String;
      Value : Integer)
   is
      use Ada.Strings.Unbounded;
      Found : Boolean := False;
   begin
      for Element of To.Adds loop
         if Element.Tag = Tag
           and then Element.Value = Value
         then
            Element.Count := Element.Count + 1;
            Found := True;
            exit;
         end if;
      end loop;

      if not Found then
         To.Adds.Append
           (Add_Element_Type'
              (Tag   => Ada.Strings.Unbounded.To_Unbounded_String (Tag),
               Value => Value,
               Count => 1));
      end if;
   end Add;

   -----------
   -- Apply --
   -----------

   procedure Apply
     (Item           : in out Calculation;
      Tag            : String;
      Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Attribute_Name : String;
      Add_Factor     : Natural := 1)
   is
      Add_Value      : constant Integer :=
                         Concorde.Attributes.Get
                           (Has_Attributes, Attribute_Name);
      Multiply_Value : constant Real :=
                         Concorde.Attributes.Get_Multiplier
                           (Has_Attributes, Attribute_Name);
   begin
      if Add_Value /= 0 then
         Add (Item, Tag, Add_Value * Add_Factor);
      end if;
      if Multiply_Value /= 1.0 then
         Modify (Item, Tag, Multiply_Value);
      end if;
   end Apply;

   ---------------
   -- Calculate --
   ---------------

   function Calculate
     (Tag   : String;
      Start : Integer)
      return Calculation
   is
   begin
      return Calculation'
        (Tag        => Ada.Strings.Unbounded.To_Unbounded_String (Tag),
         Start      => Start,
         Adds       => <>,
         Multiplies => <>);
   end Calculate;

   ------------
   -- Modify --
   ------------

   procedure Modify
     (Item   : in out Calculation;
      Tag    : String;
      Factor : Real)
   is
   begin
      Item.Multiplies.Append
        (Multiply_Element_Type'
           (Tag   => Ada.Strings.Unbounded.To_Unbounded_String (Tag),
            Value => Factor));
   end Modify;

   ----------
   -- Show --
   ----------

   function Show
     (Item      : Calculation)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
      Acc    : Integer := Item.Start;
      First  : Boolean := Item.Start = 0;
   begin
      for Element of Item.Adds loop
         Result := Result
           & (if First then "" else (if Element.Value < 0 then " -" else " +"))
           & (if First then ""
              else (if Element.Count = 1 then " "
                else Element.Count'Image & " x "))
           & Element.Tag
           & Natural'Image (abs Element.Value * Element.Count);
         Acc := Acc + Element.Value * Element.Count;
         First := False;
      end loop;
      if Item.Start /= 0 then
         Result := Item.Start'Image & Result;
      end if;

      if not Item.Multiplies.Is_Empty then
         Result := "(" & Result & ") =" & Acc'Image;
         for Element of Item.Multiplies loop
            Result := Result
              & " * "
              & Element.Tag
              & " "
              & Concorde.Real_Images.Approximate_Image (Element.Value * 100.0)
              & "%";
         end loop;
      end if;

      Result := Item.Tag & ":" & Total (Item)'Image
        & " = "
        & Result;
      return To_String (Result);
   end Show;

   --------------
   -- Subtract --
   --------------

   procedure Subtract
     (From : in out Calculation; Tag : String; Value : Natural)
   is
   begin
      Add (From, Tag, -Value);
   end Subtract;

   -----------
   -- Total --
   -----------

   function Total (Item : Calculation) return Integer is
      Factor : Real := 1.0;
   begin
      for Element of Item.Multiplies loop
         Factor := Factor * Element.Value;
      end loop;
      return Result : Integer := Item.Start do
         for Element of Item.Adds loop
            Result := Result + Element.Value * Element.Count;
         end loop;
         Result := Integer (Real (Result) * Factor);
      end return;
   end Total;

end Concorde.Calculations;
