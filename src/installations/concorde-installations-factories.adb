with WL.Random.Weighted_Random_Choices;

with Concorde.Money;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Markets;

with Accord.Facility_Production;
with Accord.Facility_Worker;
with Accord.Input_Commodity;

package body Concorde.Installations.Factories is

   package Commodity_Choice is
     new WL.Random.Weighted_Random_Choices
       (Accord.Commodity.Commodity_Handle,
        Accord.Commodity."=");

   procedure Execute_Production
     (Installation : Installation_Class);

   ------------------------
   -- Execute_Production --
   ------------------------

   procedure Execute_Production
     (Installation : Installation_Class)
   is
      use Concorde.Money, Concorde.Quantities;
      Market   : constant Concorde.Markets.Market_Type :=
                   Concorde.Markets.Get_Market
                     (Installation.Colony);
      Product  : constant Accord.Commodity.Commodity_Class :=
                   Installation.Previous;
      Sell_Price : constant Price_Type :=
                     Concorde.Markets.Current_Price (Market, Product);
      Efficiency  : constant Unit_Real := 1.0 - Installation.Inefficiency;
      Quantity    : constant Quantity_Type :=
                      Scale
                        (Installation.Facility.Capacity,
                         Efficiency / Product.Complexity);
   begin
      for Input_Commodity of
        Accord.Input_Commodity.Select_By_Commodity (Product)
      loop
         declare
            Input : constant Accord.Commodity.Commodity_Class :=
                      Input_Commodity.Input;
            Input_Price : constant Price_Type :=
                            Concorde.Markets.Current_Price (Market, Input);
            Input_Quantity : constant Quantity_Type :=
                               Input_Commodity.Quantity * Quantity;
         begin
            Concorde.Markets.Buy
              (Market, Input, Input_Quantity);
            Concorde.Agents.Remove_Cash
              (Agent => Installation,
               Cash  => Concorde.Money.Total (Input_Price, Input_Quantity),
               Tag   => "buy-input-" & Input.Tag);
         end;
      end loop;

      Concorde.Markets.Sell (Market, Product, Quantity);
      Concorde.Agents.Add_Cash
        (Agent => Installation,
         Cash  => Concorde.Money.Total (Sell_Price, Quantity),
         Tag   => "sell-product-" & Product.Tag);

   end Execute_Production;

   --------------------
   -- Manage_Factory --
   --------------------

   procedure Manage_Factory (Installation : Installation_Class) is
      use Concorde.Money, Concorde.Quantities;
      Market  : constant Concorde.Markets.Market_Type :=
                  Concorde.Markets.Get_Market
                    (Installation.Colony);
      Choices : Commodity_Choice.Weighted_Choice_Set;
      Worker_Cost : Money_Type := Zero;
   begin

      for Facility_Worker of
        Accord.Facility_Worker.Select_By_Facility
          (Installation.Facility)
      loop
         declare
            Salary : constant Price_Type :=
                       Concorde.Markets.Current_Price
                         (Market, Facility_Worker.Pop_Group);
            Cost   : constant Money_Type :=
                       Total (Salary, Facility_Worker.Quantity);
         begin
            Concorde.Markets.Buy (Market, Facility_Worker.Pop_Group,
                                  Facility_Worker.Quantity);
            Concorde.Agents.Remove_Cash
              (Installation, Cost, "pay-" & Facility_Worker.Pop_Group.Tag);
            Worker_Cost := Worker_Cost + Cost;
         end;
      end loop;

      for Facility_Production of
        Accord.Facility_Production.Select_By_Facility
          (Installation.Facility)
      loop
         declare
            Product : constant Concorde.Commodities.Commodity_Class :=
                        Facility_Production.Commodity;
            Efficiency : constant Unit_Real :=
                           (if Installation.Previous.Has_Element
                            and then Installation.Previous.Tag = Product.Tag
                            then 1.0 - Installation.Inefficiency
                            else 0.5);
            Quantity : constant Quantity_Type :=
                         Scale
                           (Installation.Facility.Capacity,
                            Efficiency / Product.Complexity);
            Input_Cost : Money_Type := Zero;
            Output_Earn : constant Money_Type :=
                            Total (Concorde.Markets.Current_Price
                                   (Market, Product),
                                   Quantity);
         begin

            if Quantity >= Unit then
               for Input_Commodity of
                 Accord.Input_Commodity.Select_By_Commodity (Product)
               loop
                  Input_Cost := Input_Cost
                    + Total (Concorde.Markets.Current_Price
                             (Market, Input_Commodity.Input),
                             Input_Commodity.Quantity * Quantity);
               end loop;

               Log (Installation,
                    "production: " & Product.Tag
                    & ": worker cost "
                    & Show (Worker_Cost)
                    & "; production quantity "
                    & Show (Quantity)
                    & "; input cost "
                    & Show (Input_Cost)
                    & "; output earn "
                    & Show (Output_Earn)
                    & "; forecast profit "
                    & Show (Output_Earn - Input_Cost - Worker_Cost));

               if Output_Earn > Input_Cost + Worker_Cost then
                  declare
                     Score : constant Natural :=
                               Natural (To_Real (Output_Earn - Input_Cost));
                  begin
                     Choices.Insert
                       (Item  => Product.To_Commodity_Handle,
                        Score => Score);
                  end;
               end if;
            end if;
         end;
      end loop;

      if Choices.Is_Empty then
         return;
      end if;

      declare
         Choice : constant Accord.Commodity.Commodity_Handle :=
                                   Choices.Choose;
      begin
         Log (Installation,
              "producing " & Choice.Tag);

         if Installation.Previous.Tag = Choice.Tag then
            Installation.Update_Installation
              .Set_Inefficiency (Installation.Inefficiency * 0.9)
              .Done;
         else
            Installation.Update_Installation
              .Set_Previous (Choice)
              .Set_Inefficiency (0.5)
              .Done;
         end if;
      end;

      Execute_Production (Installation);

   end Manage_Factory;

end Concorde.Installations.Factories;
