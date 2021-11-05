with Concorde.Logging;
with Concorde.Quantities;
with Concorde.Real_Images;
with Concorde.Stock;

with Accord.Deposit;
with Accord.Module_Production;
with Accord.Resource;

with Accord.Db;

package body Concorde.Modules.Updates is

   ------------------
   -- Daily_Update --
   ------------------

   procedure Daily_Update
     (Module : Module_Class;
      Sector : Accord.World_Sector.World_Sector_Class;
      Stock  : Accord.Has_Stock.Has_Stock_Class)
   is
      use all type Accord.Db.Module_Category;
   begin
      for Production of Accord.Module_Production.Select_By_Module (Module) loop
         case Module.Module_Group.Category is
            when Agriculture =>
               declare
                  Quantity : constant Concorde.Quantities.Quantity_Type :=
                               Concorde.Quantities.Scale
                                 (Production.Quantity,
                                  Sector.Habitability);
               begin
                  Concorde.Logging.Log
                    (Sector.World.Name
                     & ": habitability "
                     & Concorde.Real_Images.Approximate_Image
                       (Sector.Habitability * 100.0)
                     & "%; "
                     & Production.Commodity.Tag
                     & " production "
                     & Concorde.Quantities.Show (Quantity));

                  Concorde.Stock.Add (Stock, Production.Commodity, Quantity);
               end;

            when Commerce =>
               null;

            when Cultural =>
               null;

            when Development =>
               null;

            when Extraction =>
               declare
                  Deposit : constant Accord.Deposit.Deposit_Class :=
                              Accord.Deposit.Get_By_Deposit
                                (Sector,
                                 Accord.Resource.Get_From_Commodity
                                   (Production.Commodity));
                  Quantity : constant Concorde.Quantities.Quantity_Type :=
                               (if Deposit.Has_Element
                                then Concorde.Quantities.Scale
                                  (Production.Quantity,
                                   Deposit.Concentration)
                                else Concorde.Quantities.Zero);
               begin
                  if Deposit.Has_Element then
                     Concorde.Logging.Log
                       (Sector.World.Name
                        & ": concentration "
                        & Concorde.Real_Images.Approximate_Image
                          (Deposit.Concentration * 100.0)
                        & "%; "
                        & Production.Commodity.Tag
                        & " production "
                        & Concorde.Quantities.Show (Quantity));
                     Concorde.Stock.Add (Stock, Production.Commodity,
                                         Quantity);
                  end if;
               end;

            when Governance =>
               null;

            when Military =>
               null;

            when Monument =>
               null;

            when Residential =>
               null;

            when Transit =>
               null;
         end case;
      end loop;

   end Daily_Update;

end Concorde.Modules.Updates;
