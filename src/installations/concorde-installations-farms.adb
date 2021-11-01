with WL.Random.Weighted_Random_Choices;

with Concorde.Money;
with Concorde.Random;

with Concorde.Agents;
with Concorde.Commodities;
with Concorde.Markets;

with Accord.Resource;

with Accord.Db;

package body Concorde.Installations.Farms is

   package Resource_Choice is
     new WL.Random.Weighted_Random_Choices
       (Accord.Resource.Resource_Handle,
        Accord.Resource."=");

   procedure Grow
     (Installation : Installation_Class);

   ----------
   -- Grow --
   ----------

   procedure Grow
     (Installation : Installation_Class)
   is
      Market   : constant Concorde.Markets.Market_Type :=
                   Concorde.Markets.Get_Market
                     (Installation.Colony);
      Resource : constant Accord.Resource.Resource_Class :=
                   Accord.Resource.Get_From_Commodity
                     (Installation.Previous);
      Yield    : constant Non_Negative_Real :=
                   Concorde.Commodities.Yield_Estimate
                     (Resource, Installation.World_Sector)
                     * (1.0 - Installation.Inefficiency)
                   * Real'Max (Concorde.Random.Normal_Random (0.1) + 1.0, 0.1);
      Quantity : constant Concorde.Quantities.Quantity_Type :=
                   Concorde.Quantities.Scale
                     (Installation.Facility.Capacity, Yield);
   begin
      Log (Installation,
           "grew " & Concorde.Quantities.Show (Quantity)
           & " " & Resource.Tag);
      Concorde.Markets.Sell (Market, Resource, Quantity);
      Concorde.Agents.Add_Cash
        (Installation,
         Concorde.Money.Total
           (Concorde.Markets.Current_Price (Market, Resource), Quantity),
         "sell-" & Resource.Tag);
   end Grow;

   -----------------
   -- Manage_Farm --
   -----------------

   procedure Manage_Farm (Installation : Installation_Class) is
      Market : constant Concorde.Markets.Market_Type :=
                 Concorde.Markets.Get_Market
                   (Installation.Colony);
      Choices : Resource_Choice.Weighted_Choice_Set;
   begin
      for Resource of
        Accord.Resource.Select_By_Category
          (Accord.Db.Organic)
      loop
         declare
            Yield : constant Non_Negative_Real :=
                      Concorde.Commodities.Yield_Estimate
                        (Resource => Resource,
                         Sector   => Installation.World_Sector);
         begin
            if Yield > 0.0 then
               declare
                  Score : constant Natural :=
                            Natural (Yield *
                                     (if Resource.Tag
                                        = Installation.Previous.Tag
                                        then 2.0
                                        * (1.0 - Installation.Inefficiency)
                                        else 0.5)
                                     * Concorde.Money.To_Real
                                       (Concorde.Markets.Current_Price
                                          (Market, Resource))
                                     * 1000.0);
               begin
                  Log (Installation,
                       Resource.Tag
                       & ": yield "
                       & Image (Yield * 100.0)
                       & "%"
                       & "; score"
                       & Score'Image);
                  Choices.Insert
                    (Item  => Resource.To_Resource_Handle,
                     Score => Score);
               end;
            end if;
         end;
      end loop;

      if Choices.Is_Empty then
         return;
      end if;

      declare
         Choice : constant Accord.Resource.Resource_Handle :=
                                   Choices.Choose;
      begin
         Installation.Update_Installation
           .Set_Previous (Choice)
           .Done;
      end;

      Grow (Installation);

   end Manage_Farm;

end Concorde.Installations.Farms;
