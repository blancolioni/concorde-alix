with Concorde.Agents;
with Concorde.Calendar;
with Concorde.Identifiers;
with Concorde.Money;
with Concorde.Quantities;
with Concorde.Random;

with Concorde.Individuals.Create;
with Concorde.Sectors;
with Concorde.Worlds;

with Concorde.Configure.Commodities;

with Accord.Colony;
with Accord.Colony_Edict;
with Accord.Colony_Sector;
with Accord.Edict;
with Accord.Edict_Group;
with Accord.Manager;
with Accord.Module_Group;
with Accord.Owned_World;

package body Concorde.Colonies.Create is

   function Is_Good_Homeworld
     (World : Accord.World.World_Class)
      return Boolean
     with Unreferenced;

   function Evaluate_Capital_Sector
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Non_Negative_Real
     with Unreferenced;

   -----------------------------
   -- Evaluate_Capital_Sector --
   -----------------------------

   function Evaluate_Capital_Sector
     (Sector : Accord.World_Sector.World_Sector_Class)
      return Non_Negative_Real
   is
   begin
      if Sector.Elevation < Sector.World.Sea_Level then
         return 0.0;
      end if;
      return Sector.Habitability;
   end Evaluate_Capital_Sector;

   -------------------
   -- Create_Colony --
   -------------------

   procedure Initial_Colony
     (Faction : Accord.Faction.Faction_Class;
      World   : Accord.World.World_Class;
      Capital : Accord.World_Sector.World_Sector_Class;
      Init    : Tropos.Configuration)
   is
      use Concorde.Quantities;

      Colony : constant Accord.Colony.Colony_Handle :=
                 Accord.Colony.Create
                   (Account       => Concorde.Agents.New_Account
                      (Concorde.Money.Zero, Faction.Account),
                    Last_Earn     => Concorde.Money.Zero,
                    Last_Spend    => Concorde.Money.Zero,
                    Identifier    => Concorde.Identifiers.Next_Identifier,
                    World         => World,
                    Faction       => Faction,
                    Capital       => Capital,
                    Plurality     => 1.0);
      Capital_Sector : constant Accord.Colony_Sector.Colony_Sector_Handle :=
                         Accord.Colony_Sector.Create
                           (Colony       => Colony,
                            Max_Slots    => Slots_Per_Sector,
                            Slot_Count   => 0,
                            World_Sector => Capital);
      Capital_Pop : Quantity_Type := Zero;
   begin

      declare
         use Concorde.Calendar;
      begin
         Accord.Manager.Create
           (Managed    => Colony,
            Active     => True,
            Scheduled  => True,
            Next_Event => Concorde.Calendar.Clock + Days (30),
            Manager    => "colony-monthly");
         Accord.Manager.Create
           (Managed    => Colony,
            Active     => True,
            Scheduled  => True,
            Next_Event => Concorde.Calendar.Clock + Days (1),
            Manager    => "colony-daily");
      end;

      for Module_Config of Init.Child ("capital").Child ("modules") loop
         declare
            Module : constant Accord.Module.Module_Handle :=
                       Accord.Module.Get_By_Tag (Module_Config.Config_Name);
         begin
            pragma Assert (Module.Has_Element);
            Add_Module (Colony, Capital, Module);
            Capital_Pop := Capital_Pop + Module.Population;
         end;
      end loop;

      Capital_Sector.Update_Colony_Sector
        .Set_Population (Capital_Pop)
        .Done;

      Concorde.Configure.Commodities.Configure_Stock
        (Has_Stock => Colony,
         Config    => Init.Child ("stock"));

      for I in 1 .. Init.Child ("capital").Get ("individuals") loop
         declare
            use Concorde.Calendar;
            Age_In_Years : constant Non_Negative_Real :=
                             Real'Max
                               (18.0,
                                Concorde.Random.Normal_Random (10.0) + 40.0);
            Age_Duration : constant Concorde_Duration :=
                             Concorde_Duration (Age_In_Years) * Days (360.0);
         begin
            Concorde.Individuals.Create.New_Individual
              (Colony, Concorde.Calendar.Clock - Age_Duration);
         end;
      end loop;

      declare
         Urban_Sector_Count : constant Natural :=
                                Init.Child ("urban").Get ("count", 0);
         Primary_Sector_Count : constant Natural :=
                                  Init.Child ("primary").Get ("count", 0);
         Max_Sectors          : constant Natural :=
                                  Urban_Sector_Count + Primary_Sector_Count;
         Count       : Natural := 0;

         function Add_Sector
           (Sector : Accord.World_Sector.World_Sector_Class)
            return Boolean;

         ----------------
         -- Add_Sector --
         ----------------

         function Add_Sector
           (Sector : Accord.World_Sector.World_Sector_Class)
            return Boolean
         is
            Lowest_Factor : Non_Negative_Real := Non_Negative_Real'Last;
            Lowest_Module : Accord.Module.Module_Handle;
         begin
            if Sector.Identifier = Capital.Identifier
              or else Sector.Terrain.Is_Water
            then
               return True;
            end if;

            if Count < Urban_Sector_Count then
               declare
                  use Accord.Colony_Sector;
                  This_Pop : Quantity_Type := Zero;
                  Colony_Sector : constant Colony_Sector_Handle :=
                                    Accord.Colony_Sector.Create
                                      (Colony       => Colony,
                                       Max_Slots    => Slots_Per_Sector,
                                       Slot_Count   => 0,
                                       World_Sector => Sector,
                                       Population   => Zero);
               begin
                  for Module_Config of
                    Init.Child ("urban").Child ("modules")
                  loop
                     declare
                        Module : constant Accord.Module.Module_Handle :=
                                   Accord.Module.Get_By_Tag
                                     (Module_Config.Config_Name);
                     begin
                        pragma Assert (Module.Has_Element);
                        Add_Module (Colony, Sector, Module);
                        This_Pop := This_Pop + Module.Population;
                     end;
                  end loop;
                  Colony_Sector.Update_Colony_Sector
                    .Set_Population (This_Pop)
                    .Done;
               end;
            else
               for Module_Group of
                 Accord.Module_Group.Scan_By_Category
               loop
                  declare
                     Module : constant Accord.Module.Module_Class :=
                                Accord.Module.Get_By_Group_Level
                                  (Module_Group, 1);
                     Factor : constant Non_Negative_Real :=
                                Concorde.Sectors.Module_Group_Cost
                                  (Sector, Module_Group);
                  begin
                     if Module.Has_Element
                       and then Factor < Lowest_Factor
                     then
                        Lowest_Factor := Factor;
                        Lowest_Module := Module.To_Module_Handle;
                     end if;
                  end;
               end loop;

               if Lowest_Module.Has_Element then
                  Accord.Colony_Sector.Create
                    (Colony       => Colony,
                     Max_Slots    => Slots_Per_Sector,
                     Slot_Count   => 0,
                     World_Sector => Sector,
                     Population   => Lowest_Module.Population);

                  Add_Module (Colony, Sector, Lowest_Module);

               end if;
            end if;

            Count := Count + 1;
            return Count < Max_Sectors;
         end Add_Sector;

      begin
         Concorde.Worlds.Circular_Scan
           (Start   => Capital,
            Process => Add_Sector'Access);
      end;

      for Edict_Config of Init.Child ("edicts") loop
         declare
            Group : constant Accord.Edict_Group.Edict_Group_Class :=
                      Accord.Edict_Group.Get_By_Tag (Edict_Config.Config_Name);
         begin
            for Edict of Accord.Edict.Select_By_Tag (Edict_Config.Value) loop
               if Edict.Edict_Group.Tag = Group.Tag then
                  Accord.Colony_Edict.Create
                    (Colony      => Colony,
                     Edict_Group => Group,
                     Edict       => Edict);
                  exit;
               end if;
            end loop;
         end;
      end loop;

   end Initial_Colony;

   -----------------------
   -- Is_Good_Homeworld --
   -----------------------

   function Is_Good_Homeworld
     (World : Accord.World.World_Class)
      return Boolean
   is
   begin
      return not Accord.Owned_World
        .First_By_World (World)
        .Has_Element
        and then World.Habitability >= 0.8;
   end Is_Good_Homeworld;

end Concorde.Colonies.Create;
