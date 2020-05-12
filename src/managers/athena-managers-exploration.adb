with Ada.Containers.Doubly_Linked_Lists;

with Athena.Colonies;
with Athena.Empires;
with Athena.Knowledge.Stars;
with Athena.Orders;
with Athena.Ships;
with Athena.Stars;
with Athena.Technology;

with Athena.Logging;

with Athena.Handles.Colony;
with Athena.Handles.Fleet;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Component;
with Athena.Handles.Star;

package body Athena.Managers.Exploration is

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
   is
      Max_Range : constant Non_Negative_Real :=
                    Athena.Empires.Current_Tec_Level
                      (For_Empire, Athena.Technology.Drive)
                      * 20.0;

      type Star_Score_Record is
         record
            Star  : Athena.Handles.Star.Star_Handle;
            Score : Non_Negative_Real;
         end record;

      package Star_Score_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Star_Score_Record);

      function Better (Left, Right : Star_Score_Record) return Boolean
      is (Left.Score > Right.Score);

      package Star_Score_Sorting is
        new Star_Score_Lists.Generic_Sorting (Better);

      Scout_Ships : Athena.Ships.Ship_Lists.List;
      Neighbours  : Star_Score_Lists.List;

      Knowledge   : Athena.Knowledge.Stars.Star_Knowledge;

      procedure Check_Scout_Target
        (Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean)
        with Unreferenced;

      function Check_Upgrade
        (Scout : Athena.Handles.Ship.Ship_Class)
         return Boolean;

      procedure Score_Star
        (Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean);

      procedure Assign_Ship
        (Available : in out Athena.Ships.Ship_Lists.List;
         To_Star   : Athena.Handles.Star.Star_Class);

      -----------------
      -- Assign_Ship --
      -----------------

      procedure Assign_Ship
        (Available : in out Athena.Ships.Ship_Lists.List;
         To_Star   : Athena.Handles.Star.Star_Class)
      is
         Assigned : Athena.Ships.Ship_Lists.Cursor :=
                      Athena.Ships.Ship_Lists.No_Element;
         Closest  : Non_Negative_Real := Non_Negative_Real'Last;
      begin
         for Position in Available.Iterate loop
            declare
               D : constant Non_Negative_Real :=
                     Athena.Stars.Distance
                       (Athena.Ships.Ship_Lists.Element (Position).Star,
                        To_Star);
            begin
               if D < Closest then
                  Closest := D;
                  Assigned := Position;
               end if;
            end;
         end loop;

         pragma Assert (Athena.Ships.Ship_Lists.Has_Element (Assigned),
                        "expected a non-empty available list");

         declare
            Ship : constant Athena.Handles.Ship.Ship_Class :=
                     Athena.Ships.Ship_Lists.Element (Assigned);
         begin
            Athena.Ships.Move_To (Ship, To_Star);
         end;

         Available.Delete (Assigned);
      end Assign_Ship;

      ------------------------
      -- Check_Scout_Target --
      ------------------------

      procedure Check_Scout_Target
        (Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean)
      is
         use type Athena.Handles.Star.Star_Class;
         Closest : Athena.Handles.Ship.Ship_Handle :=
                     Athena.Handles.Ship.Empty_Handle;
         Min_D   : Non_Negative_Real := Non_Negative_Real'Last;
      begin
         Stop := False;
         if Knowledge.Visited (Star) then
            return;
         end if;

         for Ship of Scout_Ships loop
            if Ship.Destination = Star then
               return;
            elsif Ship.First_Order = 0
              and then not Ship.Destination.Has_Element
            then
               if Ship.Star = Nearest.Star then
                  Closest := Ship;
                  exit;
               else
                  declare
                     D : constant Non_Negative_Real :=
                           Athena.Stars.Distance (Ship.Star, Star);
                  begin
                     if D < Min_D then
                        Min_D := D;
                        Closest := Ship;

                     end if;
                  end;
               end if;
            end if;
         end loop;

         if Closest.Has_Element then
            Athena.Ships.Move_To (Closest, Star);
         else
            Stop := True;
         end if;

      end Check_Scout_Target;

      -------------------
      -- Check_Upgrade --
      -------------------

      function Check_Upgrade
        (Scout : Athena.Handles.Ship.Ship_Class)
         return Boolean
      is

         function Needs_Upgrade
           (Ship : Athena.Handles.Ship.Ship_Class)
            return Boolean;

         -------------------
         -- Needs_Upgrade --
         -------------------

         function Needs_Upgrade
           (Ship : Athena.Handles.Ship.Ship_Class)
            return Boolean
         is
            Result : Boolean := False;

            procedure Check
              (Component : Athena.Handles.Ship_Component.Ship_Component_Class);

            -----------
            -- Check --
            -----------

            procedure Check
              (Component : Athena.Handles.Ship_Component.Ship_Component_Class)
            is
            begin
               if Component.Tec_Level + 2.0 <
                 Athena.Empires.Current_Tec_Level
                   (Ship.Empire,
                    Component.Design_Component.Component.Technology)
               then
                  Result := True;
               end if;
            end Check;

         begin
            Athena.Ships.Iterate_Components (Ship, Check'Access);
            return Result;
         end Needs_Upgrade;

      begin

         if Scout.First_Order = 0
           and then not Scout.Destination.Has_Element
           and then Needs_Upgrade (Scout)
         then
            if not Scout.Star.Owner.Has_Element
              or else Scout.Star.Owner.Identifier /= For_Empire.Identifier
            then
               declare
                  Colony : constant Athena.Handles.Colony.Colony_Class :=
                             Athena.Colonies.Nearest_Colony
                               (Scout.Empire, Scout.Star);
               begin
                  Athena.Logging.Log
                    (For_Empire.Name
                     & "/exploration: sending "
                     & Scout.Name
                     & " to "
                     & Colony.Star.Name
                     & " for an upgrade");
                  Athena.Orders.Set_Destination
                    (Ship        => Scout,
                     Destination => Colony.Star,
                     Priority    => Manager.Priority);
               end;
            end if;

            return True;

         else
            return False;
         end if;

      end Check_Upgrade;

      ----------------
      -- Score_Star --
      ----------------

      procedure Score_Star
        (Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean)
      is
         Score     : constant Non_Negative_Real :=
                       (Nearest.Pop + Nearest.Industry)
                       / Athena.Stars.Distance (Nearest.Star, Star);
         Is_Target : Boolean := False;
      begin
         if not Knowledge.Visited (Star) then
            for Ship of Scout_Ships loop
               if Ship.Destination.Has_Element
                 and then Ship.Destination.Identifier = Star.Identifier
               then
                  Is_Target := True;
                  exit;
               end if;
            end loop;

            if not Is_Target then
               Neighbours.Append
                 ((Athena.Handles.Star.Get (Star.Reference_Star), Score));
            end if;
         end if;
         Stop := False;
      end Score_Star;

   begin
      Knowledge.Load (For_Empire);

      for Ship of
        Athena.Ships.Select_Managed_Ships
          (Athena.Empires.Exploration_Manager (For_Empire))
      loop
         if not Check_Upgrade (Ship) then
            Scout_Ships.Append
              (Athena.Handles.Ship.Get (Ship.Reference_Ship));
         end if;
      end loop;

      Knowledge.Iterate_Neighbours
        (Max_Range, Score_Star'Access);

      Star_Score_Sorting.Sort (Neighbours);

      declare
         Previous_Score : Non_Negative_Real := 0.0;
      begin
         while not Scout_Ships.Is_Empty
           and then not Neighbours.Is_Empty
         loop
            Log ("explore", For_Empire,
                 "scouting "
                 & Neighbours.First_Element.Star.Name & ": score "
                 & Image (Neighbours.First_Element.Score));
            Assign_Ship (Scout_Ships, Neighbours.First_Element.Star);
            Previous_Score := Neighbours.First_Element.Score;
            Neighbours.Delete_First;
         end loop;

         declare
            Required : Natural := 0;
         begin
            if Scout_Ships.Is_Empty
              and then not Neighbours.Is_Empty
            then
               while not Neighbours.Is_Empty
                 and then Neighbours.First_Element.Score > Previous_Score * 0.9
                 and then Neighbours.First_Element.Score > 300.0
               loop
                  Log ("explore", For_Empire,
                       "ordering scout so we can explore "
                       & Neighbours.First_Element.Star.Name & ": score "
                       & Image (Neighbours.First_Element.Score));
                  Required := Required + 1;
                  Previous_Score := Neighbours.First_Element.Score;
                  Neighbours.Delete_First;
               end loop;

               if Required > 0 then
                  Athena.Orders.Build_Ships
                    (Empire   => For_Empire,
                     Design   => Athena.Empires.Scout_Design (For_Empire),
                     Fleet    => Athena.Handles.Fleet.Empty_Handle,
                     Manager  => Manager,
                     Send_To  => Athena.Handles.Star.Empty_Handle,
                     Count    => Required,
                     Priority => Manager.Priority);
               end if;
            end if;
         end;
      end;

   end Create_Orders;

end Athena.Managers.Exploration;
