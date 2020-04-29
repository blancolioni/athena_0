with Athena.Colonies;
with Athena.Empires;
with Athena.Knowledge.Stars;
with Athena.Orders;
with Athena.Ships;
with Athena.Stars;
with Athena.Technology;

with Athena.Logging;

with Athena.Handles.Colony;
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

      Scout_Ships : Athena.Ships.Ship_Lists.List;

      Knowledge   : Athena.Knowledge.Stars.Star_Knowledge;

      procedure Check_Scout_Target
        (Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean);

      function Check_Upgrade
        (Scout : Athena.Handles.Ship.Ship_Class)
         return Boolean;

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
        (Max_Range, Check_Scout_Target'Access);

   end Create_Orders;

end Athena.Managers.Exploration;
