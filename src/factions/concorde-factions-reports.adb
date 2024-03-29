with Ada.Text_IO;
with WL.Reports.Tables;

with Concorde.Money;
with Concorde.Quantities;
with Concorde.Real_Images;
with Concorde.Trigonometry;

with Concorde.Agents;
with Concorde.Colonies;
with Concorde.Markets;
with Concorde.Orbits;
with Concorde.Ships;

with Accord.Colony;
with Accord.Ship;

package body Concorde.Factions.Reports is

   function Image (X : Real) return String
                   renames Concorde.Real_Images.Approximate_Image;

   -------------------------
   -- Put_Faction_Summary --
   -------------------------

   procedure Put_Faction_Summary (Faction : Faction_Class) is
   begin
      Ada.Text_IO.Put_Line (Faction.Name);
      Ada.Text_IO.Put_Line ("Cash:       "
                            & Concorde.Money.Show
                              (Concorde.Agents.Cash (Faction)));
      Ada.Text_IO.Put_Line ("Economy:   " & Image (Economy (Faction)));
      Ada.Text_IO.Put_Line ("Loyalty:   " & Image (Loyalty (Faction)));
      Ada.Text_IO.Put_Line ("Stability: " & Image (Stability (Faction)));
      Ada.Text_IO.Put_Line ("Unrest:    " & Image (Unrest (Faction)));
      Ada.Text_IO.Put_Line ("Control DC:" & Control_DC (Faction)'Image);

      for Colony of
        Accord.Colony.Select_By_Faction (Faction)
      loop
         declare
            use Ada.Text_IO;
            use Concorde.Colonies;
         begin
            Put_Line (Faction.Name & " colony on " & Colony.World.Name);
            Put_Line ("Market:      "
                      & Concorde.Markets.Show
                        (Concorde.Markets.Classify_Colony (Colony)));
            Put_Line ("GDP:         "
                      & Concorde.Money.Show (Gdp (Colony)));
            Put_Line ("GDP-PC:      "
                      & Concorde.Money.Show (Gdp_Per_Capita (Colony)));
            Put_Line ("Investment:  "
                      & Concorde.Money.Show (Total_Infrastructure (Colony)));
            Put_Line ("Maintenance: "
                      & Concorde.Money.Show (Total_Maintenance (Colony)));
            Put_Line ("Population:  "
                      & Concorde.Quantities.Show (Population (Colony)));
            Put_Line ("Employment:  "
                      & Concorde.Quantities.Show (Employment (Colony)));
            Put_Line ("Level:      " & Level (Colony)'Image);
            Put_Line ("Complexity: " & Ruling_Difficulty (Colony)'Image);
            Put_Line ("Capability: " & Capability (Colony)'Image);
            Put_Line ("Culture:    " & Culture (Colony)'Image);
            Put_Line ("Defense:    " & Defense (Colony)'Image);
            Put_Line ("Economy:    " & Economy (Colony)'Image);
            Put_Line ("Law:        " & Law (Colony)'Image);
            Put_Line ("Loyalty:    " & Loyalty (Colony)'Image);
            Put_Line ("Stability:  " & Stability (Colony)'Image);
            Put_Line ("Supply:     " & Supply (Colony)'Image);
            Put_Line ("Unrest:     " & Unrest (Colony)'Image);
         end;
      end loop;

      Ada.Text_IO.New_Line;

      declare
         function Image (X : Real) return String
                         renames Concorde.Real_Images.Approximate_Image;

         Ship_Report : WL.Reports.Tables.Table_Report;
      begin
         Ship_Report.Add_Column ("NAME");
         Ship_Report.Add_Column ("STATUS");
         Ship_Report.Add_Column ("SYSTEM");
         Ship_Report.Add_Column ("WORLD");
         Ship_Report.Add_Column ("DEST");
         Ship_Report.Add_Column ("MASS");
         Ship_Report.Add_Column ("ALTITUDE", WL.Reports.Tables.Right);
         Ship_Report.Add_Column ("LONGITUDE", WL.Reports.Tables.Right);

         for Ship of Accord.Ship.Select_By_Faction (Faction) loop
            Ship_Report.Append_Row;
            Ship_Report.Append_Cell (Ship.Name);
            Ship_Report.Append_Cell (Ship.Status'Image);
            Ship_Report.Append_Cell (if Ship.Star_System.Has_Element
                                     then Ship.Star_System.Name
                                     else "--");
            Ship_Report.Append_Cell (if Ship.World.Has_Element
                                     then Ship.World.Name
                                     else "--");
            Ship_Report.Append_Cell (if Ship.Destination.Has_Element
                                     then Ship.Destination.Name
                                     else "--");
            Ship_Report.Append_Cell
              (Image (Concorde.Ships.Current_Mass (Ship)) & "t");

            if Ship.World.Has_Element then
               Ship_Report.Append_Cell
                 (Image ((Ship.Semimajor_Axis - Ship.World.Radius) / 1000.0)
                  & "km");
               declare
                  Long : constant Real :=
                           Concorde.Trigonometry.To_Degrees
                             (Concorde.Orbits.Calculate_Current_Longitude
                                (Large_Mass => Ship.World.Mass,
                                 Orbit      => Ship.Semimajor_Axis,
                                 Epoch      => Ship.Epoch));
               begin
                  Ship_Report.Append_Cell
                    (Image (abs Long)
                     & (if Long < 0.0 then " west" else " east"));
               end;
            else
               Ship_Report.Append_Cell ("");
               Ship_Report.Append_Cell ("");
            end if;
         end loop;
         Ship_Report.Put;
      end;

   end Put_Faction_Summary;

end Concorde.Factions.Reports;
