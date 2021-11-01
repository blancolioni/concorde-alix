with Accord.Attribute;
with Accord.Attribute_Value;

package body Concorde.Configure.Attributes is

   ------------------------------
   -- Configure_Has_Attributes --
   ------------------------------

   procedure Configure_Has_Attributes
     (Has_Attributes    : Accord.Has_Attributes.Has_Attributes_Class;
      Attributes_Config : Tropos.Configuration)
   is
   begin
      for Attribute of Accord.Attribute.Scan_By_Tag loop
         declare
            Add : constant Integer :=
                    (if Attributes_Config.Contains (Attribute.Tag)
                     then (if Attributes_Config.Child (Attribute.Tag)
                       .Child_Count = 0
                       then 1 else Attributes_Config.Get (Attribute.Tag))
                     else 0);
            Mult : constant Real :=
                     Real (Float'
                             (Attributes_Config.Get
                              (Attribute.Tag & "-factor", 1.0)));
         begin
            if Add /= 0 or else Mult /= 1.0 then
               Accord.Attribute_Value.Create
                 (Has_Attributes => Has_Attributes,
                  Attribute      => Attribute,
                  Add            => Add,
                  Multiply       => Mult);
            end if;
         end;
      end loop;
   end Configure_Has_Attributes;

end Concorde.Configure.Attributes;
