with Tropos.Reader;

with Concorde.Commodities;

with Accord.Commodity_Group;
with Accord.Production;
with Accord.Input_Item;
with Accord.Output_Item;
with Accord.Efficiency_Item;
with Accord.Required_Item;
with Accord.District;

package body Concorde.Configure.Production is

   procedure Configure_Production
     (Production_Config : Tropos.Configuration);

   procedure Configure_Production_Items
     (Production : Accord.Production_Reference;
      Config     : Tropos.Configuration;
      Create : not null access
        procedure (Production : Accord.Production_Reference;
                   Commodity  : Accord.Commodity.Commodity_Class;
                   Category   : Accord.Commodity_Group_Reference;
                   Quantity   : Concorde.Quantities.Quantity_Type));

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Scenario_Name : String)
   is
   begin
      for Production_Config of
        Tropos.Reader.Read_Config
          (Scenario_Directory (Scenario_Name, "production"),
           "production")
      loop
         Configure_Production (Production_Config);
      end loop;
   end Configure_Production;

   --------------------------
   -- Configure_Production --
   --------------------------

   procedure Configure_Production
     (Production_Config : Tropos.Configuration)
   is
      use type Accord.District_Reference;
      District : constant Accord.District_Reference :=
        Accord.District.Get_By_Tag
          (Production_Config.Get ("district", ""));
      Size : constant Non_Negative_Real :=
        Real (Float'(Production_Config.Get ("size", 1.0)));
      P : constant Accord.Production_Reference :=
        Accord.Production.Create
          (Tag  => Production_Config.Config_Name,
           District => District,
           Size => Size);
   begin

      if District = Accord.Null_District_Reference then
         raise Constraint_Error with
           "in production " & Production_Config.Config_Name
           & ": no such district: "
           & Production_Config.Get ("district");
      end if;

      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("in"),
         Create     => Accord.Input_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("out"),
         Create     => Accord.Output_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("efficiency"),
         Create     => Accord.Efficiency_Item.Create'Access);
      Configure_Production_Items
        (Production => P,
         Config     => Production_Config.Child ("with"),
         Create     => Accord.Required_Item.Create'Access);
   end Configure_Production;

   --------------------------------
   -- Configure_Production_Items --
   --------------------------------

   procedure Configure_Production_Items
     (Production : Accord.Production_Reference;
      Config     : Tropos.Configuration;
      Create     : not null access
        procedure (Production : Accord.Production_Reference;
                   Commodity  : Accord.Commodity.Commodity_Class;
                   Category   : Accord.Commodity_Group_Reference;
                   Quantity   : Concorde.Quantities.Quantity_Type))
   is
   begin
      for Item of Config loop
         if Concorde.Commodities.Exists (Item.Config_Name) then
            Create (Production,
                    Concorde.Commodities.To_Database_Reference
                      (Concorde.Commodities.Get (Item.Config_Name)),
                    Accord.Null_Commodity_Group_Reference,
                    Concorde.Quantities.To_Quantity
                      (Real (Float'(Item.Value))));
         else
            declare
               use type Accord.Commodity_Group_Reference;
               Class : constant Accord.Commodity_Group_Reference :=
                 Accord.Commodity_Group.Get_By_Tag
                   (Item.Config_Name);
            begin
               if Class /= Accord.Null_Commodity_Group_Reference then
                  Create (Production, Accord.Null_Commodity_Reference,
                          Class,
                          Concorde.Quantities.To_Quantity
                            (Real (Float'(Item.Value))));
               else
                  raise Constraint_Error with
                    "no such commodity or class: " & Item.Config_Name;
               end if;
            end;
         end if;
      end loop;
   end Configure_Production_Items;

end Concorde.Configure.Production;
