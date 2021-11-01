with Tropos.Reader;

with Concorde.Configure.Attributes;

with Accord.Db;

with Accord.Module;
with Accord.Module_Group;

package body Concorde.Configure.Modules is

   procedure Configure_Module_Category
     (Category_Config : Tropos.Configuration);

   -------------------------------
   -- Configure_Module_Category --
   -------------------------------

   procedure Configure_Module_Category
     (Category_Config : Tropos.Configuration)
   is
      Category_Tag : constant String := Category_Config.Config_Name;
      Category     : constant Accord.Db.Module_Category :=
                       Accord.Db.Module_Category'Value (Category_Tag);
   begin
      for Group_Config of Category_Config.Child ("groups") loop

         declare
            function Get (Name : String) return Real
            is (Real (Float'(Group_Config.Get (Name, 0.0))));
         begin
            Accord.Module_Group.Create
              (Tag              => Group_Config.Config_Name,
               Category         => Category,
               Habitable_Factor => Get ("habitable"),
               Deposit_Factor   => Get ("deposits"));
         end;
      end loop;

      for Level in 1 .. 5 loop
         declare
            Level_Config : constant Tropos.Configuration :=
                             Category_Config.Child
                               ((1 => Character'Val (48 + Level)));
            Cost         : constant Natural := Level_Config.Get ("cost", 10);
            Slots        : constant Natural := Level_Config.Get ("slots", 1);
         begin
            for Module_Config of Level_Config.Child ("modules") loop
               declare
                  Base_Tag : constant String := Module_Config.Config_Name;
                  Tag      : constant String :=
                               Base_Tag & Integer'Image (-Level);
                  Group    : constant Accord.Module_Group.Module_Group_Class :=
                               (if Accord.Module_Group.Get_By_Tag (Base_Tag)
                                .Has_Element
                                then Accord.Module_Group.Get_By_Tag (Base_Tag)
                                else Accord.Module_Group.Create
                                  (Base_Tag, Category));

                  Module : constant Accord.Module.Module_Handle :=
                             Accord.Module.Create
                               (Tag          => Tag,
                                Module_Group => Group,
                                Level        => Level,
                                Cost         =>
                                  Concorde.Money.To_Money (Real (Cost)),
                                Slots        => Slots);
               begin
                  Concorde.Configure.Attributes.Configure_Has_Attributes
                    (Has_Attributes    => Module,
                     Attributes_Config => Module_Config);
               end;
            end loop;
         end;
      end loop;
   end Configure_Module_Category;

   -----------------------
   -- Configure_Modules --
   -----------------------

   procedure Configure_Modules (Scenario_Name : String) is
      Path : constant String :=
               Scenario_Directory
                 (Scenario_Name  => Scenario_Name,
                  Directory_Name => "modules");
   begin
      Tropos.Reader.Read_Config
        (Path      => Path,
         Extension => "module",
         Configure => Configure_Module_Category'Access);
   end Configure_Modules;

end Concorde.Configure.Modules;
