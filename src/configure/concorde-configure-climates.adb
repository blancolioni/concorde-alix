with Tropos.Reader;

with Accord.Climate;
with Accord.Climate_Terrain;
with Accord.Terrain;

package body Concorde.Configure.Climates is

   procedure Configure_Climate
     (Config : Tropos.Configuration);

   -----------------------
   -- Configure_Climate --
   -----------------------

   procedure Configure_Climate
     (Config : Tropos.Configuration)
   is
      use Accord.Db;
      Default_Name : constant String :=
                       Config.Get
                         ("default-terrain",
                          "missing default terrain");
      Default_Terrain : constant Terrain_Reference :=
                          Accord.Terrain.Get_By_Tag
                            (Default_Name);
      Habitability    : constant Natural := Config.Get ("habitability", 999);
      Climate         : constant Accord.Climate_Reference :=
                          Accord.Climate.Create
                            (Tag             => Config.Config_Name,
                             Habitability    => Real (Habitability) / 100.0,
                             Default_Terrain => Default_Terrain);
   begin
      if Default_Terrain = Null_Terrain_Reference then
         raise Constraint_Error with
           "in climate " & Config.Config_Name
           & ": unknown default terrain type: "
           & Default_Name;
      end if;

      for Terrain_Config of Config.Child ("terrain") loop
         declare
            Tag     : constant String :=
                        Terrain_Config.Config_Name;
            Terrain : constant Terrain_Reference :=
                        Accord.Terrain.Get_By_Tag (Tag);
         begin
            if Terrain = Null_Terrain_Reference then
               raise Constraint_Error with
                 "in climate " & Config.Config_Name
                 & ": unknown terrain type: "
                 & Tag;
            end if;

            Accord.Climate_Terrain.Create
              (Climate => Climate,
               Terrain => Terrain,
               Chance  => Real_Value (Terrain_Config) / 100.0);
         end;
      end loop;

   end Configure_Climate;

   ------------------------
   -- Configure_Climates --
   ------------------------

   procedure Configure_Climates
     (Scenario_Name : String)
   is
   begin
      Tropos.Reader.Read_Config
        (Path      => Scenario_Directory (Scenario_Name, "climate"),
         Extension => "climate",
         Configure => Configure_Climate'Access);
   end Configure_Climates;

end Concorde.Configure.Climates;
