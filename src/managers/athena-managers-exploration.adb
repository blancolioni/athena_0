with Athena.Empires;
with Athena.Knowledge.Stars;
with Athena.Ships;
with Athena.Stars;
with Athena.Technology;

with Athena.Handles.Colony;
with Athena.Handles.Ship;
with Athena.Handles.Star;

package body Athena.Managers.Exploration is

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Priority   : Positive)
   is
      pragma Unreferenced (Priority);
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

   begin
      Knowledge.Load (For_Empire);
      Athena.Ships.Get_Ships (Athena.Empires.Scout_Fleet (For_Empire),
                              Scout_Ships);

      Knowledge.Iterate_Neighbours
        (Max_Range, Check_Scout_Target'Access);

   end Create_Orders;

end Athena.Managers.Exploration;
