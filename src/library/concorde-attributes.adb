with WL.String_Maps;

with Accord.Attribute;
with Accord.Attribute_Value;

package body Concorde.Attributes is

   type Cached_Attribute_Value is
      record
         Attribute_Value : Accord.Attribute_Value.Attribute_Value_Handle;
         Add             : Integer;
         Multiply        : Real;
      end record;

   package Attribute_Maps is
     new WL.String_Maps (Cached_Attribute_Value);

   Attribute_Cache : Attribute_Maps.Map;

   function Attribute_Value
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String)
      return Accord.Attribute_Value.Attribute_Value_Class;

   function Key (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
                 Attribute      : String)
                 return String
   is (Has_Attributes.Identifier & "--" & Attribute);

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
      return Value : constant Accord.Attribute_Value.Attribute_Value_Handle :=
        Accord.Attribute_Value.Get_By_Attribute_Value
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
      K : constant String := Key (Has_Attributes, Name);
      Pos : constant Attribute_Maps.Cursor :=
              Attribute_Cache.Find (K);
   begin
      if Attribute_Maps.Has_Element (Pos) then
         return Attribute_Cache (K).Add;
      else
         declare
            Value : constant Accord.Attribute_Value.Attribute_Value_Class :=
                      Attribute_Value (Has_Attributes, Name);
         begin
            if Value.Has_Element then
               Attribute_Cache.Insert
                 (K,
                  Cached_Attribute_Value'
                    (Attribute_Value => Value.To_Attribute_Value_Handle,
                     Add             => Value.Add,
                     Multiply        => Value.Multiply));
               return Value.Add;
            else
               Attribute_Cache.Insert
                 (K,
                  Cached_Attribute_Value'
                    (Attribute_Value => Value.To_Attribute_Value_Handle,
                     Add             => 0,
                     Multiply        => 1.0));
               return 0;
            end if;
         end;
      end if;
   end Get;

   --------------------
   -- Get_Multiplier --
   --------------------

   function Get_Multiplier
     (Has_Attributes : Accord.Has_Attributes.Has_Attributes_Class;
      Name           : String) return Real
   is
      K   : constant String := Key (Has_Attributes, Name);
      Pos : constant Attribute_Maps.Cursor :=
              Attribute_Cache.Find (K);
   begin
      if Attribute_Maps.Has_Element (Pos) then
         return Attribute_Cache (K).Multiply;
      else
         declare
            Value : constant Accord.Attribute_Value.Attribute_Value_Class :=
                      Attribute_Value (Has_Attributes, Name);
         begin
            if Value.Has_Element then
               Attribute_Cache.Insert
                 (K,
                  Cached_Attribute_Value'
                    (Attribute_Value => Value.To_Attribute_Value_Handle,
                     Add             => Value.Add,
                     Multiply        => Value.Multiply));
               return Value.Multiply;
            else
               Attribute_Cache.Insert
                 (K,
                  Cached_Attribute_Value'
                    (Attribute_Value => Value.To_Attribute_Value_Handle,
                     Add             => 0,
                     Multiply        => 1.0));
               return 1.0;
            end if;
         end;
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
      K : constant String := Key (Has_Attributes, Name);
      Pos : constant Attribute_Maps.Cursor :=
              Attribute_Cache.Find (K);
      Cached : constant Boolean := Attribute_Maps.Has_Element (Pos);
      Rec : constant Accord.Attribute_Value.Attribute_Value_Class :=
              Attribute_Value (Has_Attributes, Name);
   begin
      if Cached then
         Attribute_Cache (Pos).Add := Value;
      elsif Rec.Has_Element then
         Attribute_Cache.Insert
           (K, Cached_Attribute_Value'
              (Rec.To_Attribute_Value_Handle,
               Value, Rec.Multiply));
      end if;

      if Rec.Has_Element then
         Rec.Update_Attribute_Value
           .Set_Add (Value)
           .Done;
      elsif Value /= 0 then
         declare
            Handle : constant Accord.Attribute_Value.Attribute_Value_Handle :=
                       Accord.Attribute_Value.Create
                         (Has_Attributes => Has_Attributes,
                          Attribute      => Accord.Attribute.Get_By_Tag (Name),
                          Add            => Value,
                          Multiply       => 1.0);
         begin
            if not Cached then
               Attribute_Cache.Insert
                 (K, Cached_Attribute_Value'
                    (Handle, Value, 1.0));
            end if;
         end;
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
      K   : constant String := Key (Has_Attributes, Name);
      Pos : constant Attribute_Maps.Cursor :=
              Attribute_Cache.Find (K);
      Cached : constant Boolean := Attribute_Maps.Has_Element (Pos);
      Rec    : constant Accord.Attribute_Value.Attribute_Value_Class :=
              Attribute_Value (Has_Attributes, Name);
   begin
      if Cached then
         Attribute_Cache (Pos).Multiply := Multiplier;
      elsif Rec.Has_Element then
         Attribute_Cache.Insert
           (K, Cached_Attribute_Value'
              (Rec.To_Attribute_Value_Handle,
               Rec.Add, Multiplier));
      end if;

      if Rec.Has_Element then
         Rec.Update_Attribute_Value
           .Set_Multiply (Multiplier)
           .Done;
      elsif Multiplier /= 1.0 then
         declare
            Handle : constant Accord.Attribute_Value.Attribute_Value_Handle :=
                       Accord.Attribute_Value.Create
                         (Has_Attributes => Has_Attributes,
                          Attribute      => Accord.Attribute.Get_By_Tag (Name),
                          Add            => 0,
                          Multiply       => Multiplier);
         begin
            if not Cached then
               Attribute_Cache.Insert
                 (K, Cached_Attribute_Value'
                    (Handle, 0, Multiplier));
            end if;
         end;
      end if;
   end Set;

end Concorde.Attributes;
