with Athena.Logging;
with Athena.Real_Images;

with Athena.Colonies;
with Athena.Stars;
with Athena.Turns;

with Athena.Knowledge.Stars;

with Athena.Handles.Colony;
with Athena.Handles.Fleet_Journey;
with Athena.Handles.Ship_Journey;
with Athena.Handles.Star;

with Athena.Db.Ship_Order;

package body Athena.Ships.Updates is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   function Move_Ship
     (Ship : Athena.Handles.Ship.Ship_Class)
      return Boolean;
   --  return true if ship arrives at its destination

   function Execute_Load
     (Ship     : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
      return Boolean;
   --  return true if the load was completed

   function Execute_Unload
     (Ship     : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
      return Boolean;
   --  return true if the unload was completed

   ------------------
   -- Execute_Load --
   ------------------

   function Execute_Load
     (Ship     : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
      return Boolean
   is
      Max_Quantity : constant Non_Negative_Real :=
                       Real'Min (Quantity,
                                 Available_Space (Ship));
   begin

      Athena.Logging.Log
        (Ship.Empire.Name
         & " ship " & Ship.Name
         & ": loading " & Image (Quantity) & " " & Cargo'Image);

      if Ship.Star.Owner.Has_Element
        and then Ship.Empire.Identifier /= Ship.Star.Owner.Identifier
      then
         --  loading impossible
         return True;
      else
         declare
            Colony : constant Athena.Handles.Colony.Colony_Class :=
                       Athena.Stars.Get_Colony (Ship.Star);
         begin
            case Cargo is
               when Athena.Db.Colonists =>
                  declare
                     Required_Pop : constant Non_Negative_Real :=
                                      10.0 * Max_Quantity;
                     Available_Pop : constant Non_Negative_Real :=
                                       Colony.Pop;
                     Loaded_Pop    : constant Non_Negative_Real :=
                                       Real'Min (Required_Pop, Available_Pop);
                     Remaining_Pop : constant Non_Negative_Real :=
                                       Available_Pop - Loaded_Pop;
                     Current_Colonists : constant Non_Negative_Real :=
                                           Ship.Colonists;
                  begin
                     Ship.Update_Ship
                       .Set_Colonists
                         (Current_Colonists + Loaded_Pop / 10.0)
                       .Done;
                     Colony.Update_Colony
                       .Set_Pop (Remaining_Pop)
                       .Done;
                     return True;
                  end;

               when Athena.Db.Material =>

                  declare
                     Loaded : constant Non_Negative_Real :=
                                Real'Min (Colony.Material, Max_Quantity);
                     Remaining : constant Non_Negative_Real :=
                                   Colony.Material - Loaded;
                     Current : constant Non_Negative_Real :=
                                 Ship.Material;
                  begin
                     Ship.Update_Ship
                       .Set_Material
                         (Current + Loaded)
                       .Done;
                     Colony.Update_Colony
                       .Set_Material (Remaining)
                       .Done;
                     return Loaded = Max_Quantity;
                  end;

               when Athena.Db.Industry =>

                  declare
                     Loaded    : constant Non_Negative_Real :=
                                   Real'Min (Colony.Industry, Max_Quantity);
                     Remaining : constant Non_Negative_Real :=
                                   Colony.Industry - Loaded;
                     Current   : constant Non_Negative_Real :=
                                   Ship.Industry;
                  begin
                     Ship.Update_Ship
                       .Set_Industry
                         (Current + Loaded)
                       .Done;
                     Colony.Update_Colony
                       .Set_Industry (Remaining)
                       .Done;
                     return Loaded = Max_Quantity;
                  end;
            end case;
         end;
      end if;
   end Execute_Load;

   --------------------
   -- Execute_Unload --
   --------------------

   function Execute_Unload
     (Ship     : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
      return Boolean
   is
      Cargo_Quantity : constant Non_Negative_Real :=
                         (case Cargo is
                             when Athena.Db.Colonists =>
                               Ship.Colonists,
                             when Athena.Db.Industry  =>
                               Ship.Industry,
                             when Athena.Db.Material  =>
                               Ship.Material);
      Unloaded_Quantity : constant Real :=
                            Real'Min (Quantity, Cargo_Quantity);
      Colony            : constant Athena.Handles.Colony.Colony_Class :=
                            Athena.Stars.Get_Colony (Ship.Star);
   begin

      Athena.Logging.Log
        (Ship.Empire.Name
         & " ship " & Ship.Name
         & ": unloading " & Image (Quantity) & " " & Cargo'Image);

      if Unloaded_Quantity = 0.0 then
         return True;
      end if;

      case Cargo is
         when Athena.Db.Colonists =>
            if not Colony.Has_Element then
               --  create a new colony

               Athena.Colonies.New_Colony
                 (At_Star => Ship.Star,
                  Owner   => Ship.Empire,
                  Pop     => Unloaded_Quantity * 10.0,
                  Ind     => 0.0,
                  Mat     => 0.0);

               Ship.Update_Ship
                 .Set_Colonists (Cargo_Quantity - Unloaded_Quantity)
                 .Done;

               Athena.Knowledge.Stars.Clear_Colonizing
                 (Ship.Empire, Ship.Star);

               return True;
            elsif Colony.Empire.Identifier /= Ship.Empire.Identifier then
               --  this means war!
               return True;
               --  but that's not implemented
            else
               declare
                  New_Pop : constant Non_Negative_Real :=
                              Colony.Pop + Unloaded_Quantity * 10.0;
               begin
                  Ship.Update_Ship
                    .Set_Colonists
                      (Cargo_Quantity - Unloaded_Quantity)
                    .Done;
                  Colony.Update_Colony
                    .Set_Pop (New_Pop)
                    .Done;
               end;
               return True;
            end if;

         when Athena.Db.Material =>

            if not Colony.Has_Element then

               --  Canceled
               return True;

               --  create a new colony
--                 Athena.Handles.Colony.Create
--                   (Star      => Ship.Star,
--                    Empire    => Ship.Empire,
--                    Construct => 0.0,
--                    Pop       => 0.0,
--                    Colonists => 0.0,
--                    Industry  => 0.0,
--                    Material  => Unloaded_Quantity);
--
--                 Ship.Star.Update_Star
--                   .Set_Owner (Ship.Empire.Reference_Empire)
--                   .Done;

            else
               declare
                  New_Quantity : constant Non_Negative_Real :=
                                   Colony.Material + Unloaded_Quantity;
               begin
                  Colony.Update_Colony
                    .Set_Material (New_Quantity)
                    .Done;
               end;
            end if;

            Ship.Update_Ship
              .Set_Material (Cargo_Quantity - Unloaded_Quantity)
              .Done;

            return True;

         when Athena.Db.Industry =>

            if not Colony.Has_Element then

               --  Canceled
               return True;

               --  create a new colony
--                 Athena.Handles.Colony.Create
--                   (Star      => Ship.Star,
--                    Empire    => Ship.Empire,
--                    Construct => 0.0,
--                    Pop       => 0.0,
--                    Colonists => 0.0,
--                    Industry  => Unloaded_Quantity,
--                    Material  => 0.0);
--
--                 Ship.Star.Update_Star
--                   .Set_Owner (Ship.Empire.Reference_Empire)
--                   .Done;

            else
               declare
                  New_Quantity : constant Non_Negative_Real :=
                                   Colony.Industry + Unloaded_Quantity;
               begin
                  Colony.Update_Colony
                    .Set_Industry (New_Quantity)
                    .Done;
               end;
            end if;

            Ship.Update_Ship
              .Set_Material (Cargo_Quantity - Unloaded_Quantity)
              .Done;

            return True;
      end case;
   end Execute_Unload;

   ---------------
   -- Move_Ship --
   ---------------

   function Move_Ship
     (Ship : Athena.Handles.Ship.Ship_Class)
      return Boolean
   is
   begin
      if not Ship.Destination.Has_Element then
         return True;
      elsif Ship.Destination.Identifier = Ship.Star.Identifier then
         Ship.Update_Ship
           .Set_Destination (Athena.Db.Null_Star_Reference)
           .Done;
         return True;
      else

         declare
            Total_Distance : constant Non_Negative_Real :=
                               Athena.Stars.Distance
                                 (Ship.Star, Ship.Destination);
            Travelled      : constant Non_Negative_Real :=
                               Total_Distance * Ship.Progress;
            Remaining      : constant Non_Negative_Real :=
                               Total_Distance - Travelled;
            Speed          : constant Non_Negative_Real :=
                               Athena.Ships.Speed (Ship);
            XP             : constant Non_Negative_Real := Ship.Experience;
            New_XP         : constant Non_Negative_Real :=
                               XP + Real'Min (Speed, Remaining) * 0.001;
         begin

            Athena.Logging.Log
              (Ship.Empire.Adjective
               & " ship " & Ship.Name
               & " moving to "
               & Ship.Destination.Name
               & ": speed " & Image (Speed)
               & "; travelled " & Image (Travelled)
               & "; remaining " & Image (Remaining));

            Ship.Update_Ship.Set_Experience (New_XP).Done;

            Athena.Handles.Ship_Journey.Create
              (Turn       => Athena.Turns.Current_Turn,
               Ship       => Ship,
               Empire     => Ship.Empire,
               Mass       => Mass (Ship),
               From       => Ship.Star,
               To         => Ship.Destination,
               Progress   =>
                 Real'Min ((Travelled + Speed) / Total_Distance, 1.0));

            if Speed >= Remaining then
               Athena.Logging.Log
                 (Ship.Empire.Adjective
                  & " ship " & Ship.Name
                  & " arrives at " & Ship.Destination.Name);
               Ship.Update_Ship
                 .Set_Star (Ship.Destination.Reference_Star)
                 .Set_Destination (Athena.Db.Null_Star_Reference)
                 .Set_Progress (0.0)
                 .Done;
               On_Arrival (Ship);
               return True;
            else
               Ship.Update_Ship
                 .Set_Progress ((Travelled + Speed) / Total_Distance)
                   .Done;
               return False;
            end if;
         end;
      end if;
   end Move_Ship;

   ------------
   -- Repair --
   ------------

   procedure Repair
     (Ship : Athena.Handles.Ship.Ship_Class)
   is
      use Athena.Handles.Ship_Component;
      Component : constant Ship_Component_Class :=
                           Get_Repair (Ship);
   begin
      if not Component.Has_Element then
         return;
      end if;

      declare
         Repair_Mass : Non_Negative_Real :=
                         Component.Tec_Level * Component.Condition
                           * Component.Design_Component.Mass;

         procedure Repair_Component
           (Damaged : Ship_Component_Class);

         ----------------------
         -- Repair_Component --
         ----------------------

         procedure Repair_Component
           (Damaged : Ship_Component_Class)
         is
            Damage : constant Non_Negative_Real :=
                       Damaged.Damage;
            Mass     : constant Non_Negative_Real :=
                         Damaged.Design_Component.Mass;
            Repaired : constant Non_Negative_Real :=
                         Real'Min (Damage, Repair_Mass / Damaged.Tec_Level);
            New_Condition : constant Unit_Real :=
                              (1.0 - ((Damage - Repaired) / Mass) ** 2);
         begin
            if Repaired > 0.0 then
--                 Athena.Logging.Log
--                   (Ship.Empire.Name & " ship " & Ship.Name
--                    & " repairs " & Image (Repaired)
--                    & "/" & Image (Damage)
--                    & " damage to "
--                    & Damaged.Ship.Name
--                    & " "
--                    & Damaged.Component.Tag);
               Damaged.Update_Ship_Component
                 .Set_Damage (Damage - Repaired)
                 .Set_Condition (New_Condition)
                 .Done;
               Repair_Mass := Repair_Mass - Repaired;
            end if;
         end Repair_Component;

      begin
         Iterate_Components (Ship, Repair_Component'Access);

         declare
            use Athena.Handles.Ship.Selections;
         begin
            for Neighbour of
              Select_Where (Star = Ship.Star and Empire = Ship.Empire)
            loop
               if Neighbour.Identifier /= Ship.Identifier
                 and then not Get_Repair (Neighbour).Has_Element
               then
                  Iterate_Components (Neighbour, Repair_Component'Access);
               end if;
            end loop;
         end;
      end;
   end Repair;

   ------------
   -- Update --
   ------------

   procedure Update
     (Ship : Athena.Handles.Ship.Ship_Class)
   is
      Moved : Boolean := False;
      XP    : constant Non_Negative_Real := Ship.Experience;
   begin

      Ship.Update_Ship.Set_Experience (XP + 0.01).Done;

      if Ship.Fleet.Has_Element then
         return;
      end if;

      if Ship.Destination.Has_Element then
         if not Move_Ship (Ship) then
            return;
         end if;
         Moved := True;
      end if;

      declare
         Index : Natural := Ship.First_Order;
      begin
         if Index = 0 then
            return;
         end if;

         while Index <= Ship.Last_Order loop
            declare
               use all type Athena.Db.Ship_Action;
               Order : constant Athena.Db.Ship_Order.Ship_Order_Type :=
                         Athena.Db.Ship_Order.Get_By_Ship_Order
                           (Ship.Reference_Ship, Index);
            begin
               pragma Assert (Order.Has_Element);

               case Order.Action is
                  when Load =>
                     exit when not Execute_Load
                       (Ship, Order.Cargo, Order.Quantity);

                  when Unload =>
                     exit when not Execute_Unload
                       (Ship, Order.Cargo, Order.Quantity);

                  when Move =>
                     exit when Moved;

                     Athena.Logging.Log
                       (Ship.Empire.Name & " ship "
                        & Ship.Name & ": starting journey to "
                        & Athena.Handles.Star.Get (Order.Star).Name);

                     Ship.Update_Ship
                       .Set_Destination (Order.Star)
                       .Set_Progress (0.0)
                       .Done;

                     declare
                        Arrived : constant Boolean :=
                                    Move_Ship (Ship);
                     begin
                        pragma Unreferenced (Arrived);
                     end;

                     Index := Index + 1;
                     exit;

               end case;

               Index := Index + 1;
            end;
         end loop;

         if Index > Ship.Last_Order then
            Ship.Update_Ship
              .Set_First_Order (0)
              .Set_Last_Order (0)
              .Done;
         else
            Ship.Update_Ship
              .Set_First_Order (Index)
              .Done;
         end if;
      end;
   end Update;

   ------------
   -- Update --
   ------------

   procedure Update
     (Fleet : Athena.Handles.Fleet.Fleet_Class)
   is
      Origin : constant Athena.Handles.Star.Star_Class :=
                 Fleet.Location;
      Destination : constant Athena.Handles.Star.Star_Class :=
                      Fleet.Destination;
   begin
      if not Destination.Has_Element then
         return;
      end if;

      if Fleet.Progress = 0.0 then
         declare
            use Athena.Handles.Ship.Selections;
            Slowest   : Non_Negative_Real := Non_Negative_Real'Last;
            Mass      : Non_Negative_Real := 0.0;
            Has_Ships : Boolean := False;
         begin
            for Ship of Select_Where
              (Athena.Handles.Ship.Selections.Fleet = Fleet)
            loop
               Ship.Update_Ship
                 .Set_Destination (Fleet.Destination.Reference_Star)
                 .Done;
               Has_Ships := True;
               Mass := Mass + Athena.Ships.Mass (Ship);
               Slowest := Non_Negative_Real'Min
                 (Slowest, Speed (Ship));
            end loop;

            if not Has_Ships then
               Fleet.Update_Fleet
                 .Set_Location (Destination.Reference_Star)
                 .Set_Destination (Athena.Db.Null_Star_Reference)
                 .Done;
               return;
            end if;

            Fleet.Update_Fleet
              .Set_Speed (Slowest)
              .Set_Mass (Mass)
              .Done;
         end;
      end if;

      declare
         Total_Distance : constant Non_Negative_Real :=
                            Athena.Stars.Distance
                              (Origin, Destination);
         Travelled      : constant Non_Negative_Real :=
                            Total_Distance * Fleet.Progress;
         Remaining      : constant Non_Negative_Real :=
                            Total_Distance - Travelled;
         Speed          : constant Non_Negative_Real := Fleet.Speed;
      begin

         Athena.Logging.Log
           (Fleet.Empire.Adjective
            & " fleet " & Fleet.Name
            & " moving to "
            & Destination.Name
            & ": speed " & Image (Speed)
            & "; travelled " & Image (Travelled)
            & "; remaining " & Image (Remaining));

         Athena.Handles.Fleet_Journey.Create
           (Turn     => Athena.Turns.Current_Turn,
            Fleet    => Fleet,
            Empire   => Fleet.Empire,
            Mass     => Fleet.Mass,
            From     => Origin,
            To       => Destination,
            Progress =>
              Real'Min ((Travelled + Speed) / Total_Distance, 1.0));

         if Speed >= Remaining then
            Athena.Logging.Log
              (Fleet.Empire.Adjective
               & " fleet " & Fleet.Name
               & " arrives at " & Destination.Name);
            Fleet.Update_Fleet
              .Set_Location (Destination.Reference_Star)
              .Set_Destination (Athena.Db.Null_Star_Reference)
              .Set_Progress (0.0)
              .Set_Speed (0.0)
              .Done;

         else
            Fleet.Update_Fleet
              .Set_Progress ((Travelled + Speed) / Total_Distance)
              .Done;
         end if;

         declare
            use Athena.Handles.Ship.Selections;
         begin
            for Ship of Select_Where
              (Athena.Handles.Ship.Selections.Fleet = Fleet)
            loop
               declare
                  XP             : constant Non_Negative_Real :=
                                     Ship.Experience;
                  New_XP         : constant Non_Negative_Real :=
                                     XP + Real'Min (Speed, Remaining) * 0.001;
               begin
                  Ship.Update_Ship
                    .Set_Star (Fleet.Location.Reference_Star)
                    .Set_Destination (Fleet.Destination.Reference_Star)
                    .Set_Progress (Fleet.Progress)
                    .Set_Experience (New_XP)
                    .Done;
               end;
            end loop;
         end;
      end;

   end Update;

end Athena.Ships.Updates;
