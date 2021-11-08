with Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

with WL.Numerics.Roman;
with WL.String_Sets;

with Concorde.Configure;
with Concorde.Identifiers;
with Concorde.Logging;
with Concorde.Money;

with Concorde.Agents;
with Concorde.Individuals;
with Concorde.Star_Systems;
with Concorde.Worlds;

with Concorde.Colonies.Create;
with Concorde.Factions.Reports;
with Concorde.Ships;

with Accord.Account;
with Accord.Colony;
with Accord.Company;
with Accord.Deposit;
with Accord.Individual;
with Accord.Office;
with Accord.Owned_World;
with Accord.Script;
with Accord.Script_Line;
with Accord.Shareholder;
with Accord.Ship_Design;
with Accord.Star_System;
with Accord.Star_System_Distance;
with Accord.World;
with Accord.World_Sector;

package body Concorde.Factions.Create is

--     Log_Faction_Creation : constant Boolean := False;

   Faction_Company_Shares : constant := 1000;

   function Find_Homeworld
     return Accord.World.World_Class;

   function Find_Home_Sector
     (World : Accord.World.World_Class)
      return Accord.World_Sector.World_Sector_Class;

   function Find_Office_Holder
     (Faction : Faction_Class;
      Office  : Accord.Office.Office_Class)
      return Accord.Individual.Individual_Class;

   function To_Ship_Name
     (Design_Name : String;
      Index       : Positive;
      Count       : Positive)
      return String;

   --------------------
   -- Create_Faction --
   --------------------

   function Create_Faction
     (User        : Accord.User.User_Class;
      Name        : String;
      Adjective   : String;
      Plural_Name : String;
      Color       : Concorde.Color.Concorde_Color;
      Setup       : Tropos.Configuration)
      return Accord.Faction.Faction_Class
   is
      Capital : constant Accord.World.World_Class :=
                  Find_Homeworld;
   begin
      if not Capital.Has_Element then
         return Accord.Faction.Empty_Handle;
      end if;

      declare
         Cash    : constant Concorde.Money.Money_Type :=
                     Concorde.Configure.Configure_Money
                       (Setup, "start-cash", 1000.0);
         Account : constant Accord.Account.Account_Handle :=
                     Concorde.Agents.New_Account (Cash);
         Sector  : constant Accord.World_Sector.World_Sector_Class :=
           Find_Home_Sector (Capital);
         Faction : constant Accord.Faction.Faction_Handle :=
                     Accord.Faction.Create
                       (Identifier    => Concorde.Identifiers.Next_Identifier,
                        Name          => Name,
                        Adjective     =>
                          (if Adjective = "" then Name else Adjective),
                        Plural_Name   =>
                          (if Plural_Name = "" then Name else Plural_Name),
                        Account       => Account,
                        Last_Earn     => Concorde.Money.Zero,
                        Last_Spend    => Concorde.Money.Zero,
                        Human         => False,
                        Red           => Color.Red,
                        Green         => Color.Green,
                        Blue          => Color.Blue,
                        User          => User,
                        Capital_System => Capital.Star_System,
                        Capital_World  => Capital,
                        Fame           => 1,
                        Infamy         => 0);

         Company    : constant Accord.Company.Company_Handle
           := Accord.Company.Create
             (Identifier   =>
                  Concorde.Identifiers.Next_Identifier,
              Account      =>
                Concorde.Agents.New_Account (Concorde.Money.Zero),
              Last_Earn     => Concorde.Money.Zero,
              Last_Spend    => Concorde.Money.Zero,
              Name          => Name,
              Faction       => Faction,
              Headquarters  => Capital,
              Shares        => Faction_Company_Shares,
              Dividend      => 0.2);
         Remaining_Shares : constant Natural := Faction_Company_Shares;
         Script           : constant Accord.Script.Script_Handle :=
                              Accord.Script.Create ("rc", User);
         Line_Index       : Natural := 0;

      begin

         Sector.Update_World_Sector
           .Set_Faction (Faction)
           .Done;

         Concorde.Colonies.Create.Initial_Colony
           (Faction => Faction,
            World   => Capital,
            Capital => Sector,
            Init    => Setup);

         Accord.Shareholder.Create
           (Company => Company,
            Agent   => Faction,
            Shares  => Remaining_Shares);

         if not Setup.Contains ("init-script") then
            Ada.Text_IO.Put_Line
              ("warning: no initial script in " & Setup.Config_Name);
         end if;

         for Command of Setup.Child ("init-script") loop
            Line_Index := Line_Index + 1;
            Accord.Script_Line.Create
              (Script => Script,
               Index  => Line_Index,
               Line   => Command.Config_Name);
         end loop;

         Accord.Owned_World.Create
           (Faction => Faction,
            World   => Capital);

         for Office of Accord.Office.Scan_By_Rank loop
            declare
               Holder : constant Accord.Individual.Individual_Class :=
                          Find_Office_Holder (Faction, Office);
            begin
               if not Holder.Has_Element then
                  Ada.Text_IO.Put_Line
                    ("cannot find suitable candidate for "
                     & Office.Tag);
               else
                  Ada.Text_IO.Put_Line
                    (Office.Tag & ": " & Holder.First_Name
                     & " " & Holder.Last_Name);
                  Holder.Update_Individual
                    .Set_Office (Office)
                    .Done;
               end if;
            end;
         end loop;

         for Ship_Config of Setup.Child ("ships") loop
            declare
               Design_Name : constant String := Ship_Config.Config_Name;
               Count       : constant Natural := Ship_Config.Value;
               Design      : constant Accord.Ship_Design.Ship_Design_Class :=
                               Accord.Ship_Design.First_By_Name (Design_Name);
               Home        : constant Accord.Colony.Colony_Class :=
                               Accord.Colony.First_By_World
                                 (Capital);
            begin
               if not Design.Has_Element then
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     "no such ship design: " & Design_Name);
               else
                  for I in 1 .. Count loop
                     Concorde.Ships.Create_Ship
                       (Owner   => Faction,
                        Home    => Home,
                        World   => Capital,
                        Design  => Design,
                        Manager => Design.Default_Manager,
                        Name    => To_Ship_Name (Design_Name, I, Count));
                  end loop;
               end if;
            end;
         end loop;

         Concorde.Factions.Reports.Put_Faction_Summary (Faction);

         return Faction;
      end;
   end Create_Faction;

   ----------------------
   -- Find_Home_Sector --
   ----------------------

   function Find_Home_Sector
     (World : Accord.World.World_Class)
      return Accord.World_Sector.World_Sector_Class
   is
      function Score_Sector
        (Sector : Accord.World_Sector.World_Sector_Class)
         return Real;

      ------------------
      -- Score_Sector --
      ------------------

      function Score_Sector
        (Sector : Accord.World_Sector.World_Sector_Class)
         return Real
      is
         Score : Real := 0.0;
      begin
         if Sector.Terrain.Is_Water then
            return Real'First;
         end if;

         declare
            Ns : constant Concorde.Worlds.World_Sector_Array :=
                   Concorde.Worlds.Get_Neighbours (Sector);
         begin
            for N of Ns loop
               for Deposit of
                 Accord.Deposit.Select_By_World_Sector (N)
               loop
                  Score :=
                    Real'Max
                      (Score,
                         Deposit.Concentration / (1.0 + Deposit.Difficulty)
                       * (1.0 - Concorde.Worlds.Get_Terrain (N).Hazard));
               end loop;
            end loop;

            return Score * (1.0 - Sector.Terrain.Hazard);
         end;
      end Score_Sector;

   begin
      return Concorde.Worlds.Best_Sector (World, Score_Sector'Access);
   end Find_Home_Sector;

   --------------------
   -- Find_Homeworld --
   --------------------

   function Find_Homeworld
     return Accord.World.World_Class
   is

      package Star_System_Lists is
        new Ada.Containers.Doubly_Linked_Lists
          (Accord.Star_System.Star_System_Handle,
           Accord.Star_System."=");

      Queue : Star_System_Lists.List;
      Checked : WL.String_Sets.Set;

      function Check_World
        (World : Accord.World.World_Class)
         return Boolean;

      -----------------
      -- Check_World --
      -----------------

      function Check_World
        (World : Accord.World.World_Class)
         return Boolean
      is
      begin
         return World.Habitability > 0.7;
      end Check_World;

   begin

      Queue.Append (Concorde.Star_Systems.First.To_Star_System_Handle);
      Checked.Include
        (Concorde.Star_Systems.First.Name);

      while not Queue.Is_Empty loop
         declare
            use Concorde.Star_Systems;
            subtype Star_System_Handle is
              Accord.Star_System.Star_System_Handle;
            Star_System : constant Star_System_Handle :=
                            Queue.First_Element;
         begin
            Queue.Delete_First;

            if not Star_System.Claimed then
               declare
                  Selection : constant Concorde.Worlds.World_Selection :=
                                Concorde.Star_Systems.Terrestrial_Worlds
                                  (Star_System);
               begin
                  if not Selection.Is_Empty then
                     for W of Selection.Get_Worlds loop
                        if Check_World (W) then
                           Claim (Star_System);
                           return W;
                        end if;
                     end loop;
                  end if;
               end;
            end if;

            for Neighbour of
              Accord.Star_System_Distance
                .Select_Star_System_Range_Bounded_By_Distance
                  (Star_System, 0.0, 99.0)
            loop
               declare
                  Neighbour_Name : constant String :=
                                     Neighbour.To.Name;
               begin
                  if not Checked.Contains (Neighbour_Name) then
                     Checked.Include (Neighbour_Name);
                     Queue.Append (Neighbour.To.To_Star_System_Handle);
                  end if;
               end;
            end loop;
         end;
      end loop;

      return Accord.World.Empty_Handle;
   end Find_Homeworld;

   ------------------------
   -- Find_Office_Holder --
   ------------------------

   function Find_Office_Holder
     (Faction : Faction_Class;
      Office  : Accord.Office.Office_Class)
      return Accord.Individual.Individual_Class
   is
      Best_Score  : Natural := 0;
      Best_Holder : Accord.Individual.Individual_Handle;
   begin
      for Individual of Accord.Individual.Select_By_Faction (Faction) loop
         if not Individual.Office.Has_Element then
            declare
               This_Score : constant Integer :=
                              Concorde.Individuals.Ruling_Bonus
                                (Individual => Individual,
                                 Office     => Office);
            begin
               if This_Score > 0 then
                  Concorde.Logging.Log
                    (Category => Office.Tag,
                     Message  =>
                       Individual.First_Name & " " & Individual.Last_Name
                     & " has ruling bonus" & This_Score'Image);
               end if;

               if This_Score > Best_Score then
                  Best_Score := This_Score;
                  Best_Holder := Individual.To_Individual_Handle;
               end if;
            end;
         end if;
      end loop;

      Concorde.Logging.Log
        (Category => Office.Tag,
         Message  =>
           "best fit is "
         & Best_Holder.First_Name & " " & Best_Holder.Last_Name
         & " with ruling bonus" & Best_Score'Image);
      return Best_Holder;
   end Find_Office_Holder;

   ------------------
   -- To_Ship_Name --
   ------------------

   function To_Ship_Name
     (Design_Name : String;
      Index       : Positive;
      Count       : Positive)
      return String
   is
      Ship_Name : String := Design_Name;
      Capital   : Boolean := True;
   begin
      for Ch of Ship_Name loop
         if Capital then
            Ch := Ada.Characters.Handling.To_Upper (Ch);
            Capital := False;
         elsif Ch = '-' or else Ch = '_' then
            Ch := ' ';
            Capital := True;
         end if;
      end loop;

      return Ship_Name
        & (if Count > 1
           then " " & WL.Numerics.Roman.Roman_Image (Index)
           else "");
   end To_Ship_Name;

end Concorde.Factions.Create;
