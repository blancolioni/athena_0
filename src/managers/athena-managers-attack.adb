with Athena.Colonies;
with Athena.Empires;
with Athena.Orders;
with Athena.Ships;
with Athena.Stars;
with Athena.Technology;

with Athena.Knowledge.Stars;

with Athena.Identifiers;

with Athena.Handles.Colony;
with Athena.Handles.Empire_Manager;
with Athena.Handles.Fleet.Selections;
with Athena.Handles.Ship_Design;
with Athena.Handles.Star;

package body Athena.Managers.Attack is

   procedure Evaluate_Threats
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class);

   function Find_Available_Fleet
     (For_Empire  : Athena.Handles.Empire.Empire_Class;
      Manager     : Athena.Handles.Empire_Manager.Empire_Manager_Class;
      Origin      : Athena.Handles.Star.Star_Class;
      Destination : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Fleet.Fleet_Class;

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
   is
      Total_Industry : Non_Negative_Real := 0.0;

      procedure Add_Industry
        (Colony : Athena.Handles.Colony.Colony_Class);

      procedure Check_Ships
        (Design            : Athena.Handles.Ship_Design.Ship_Design_Class;
         Industry_Per_Ship : Positive);

      ------------------
      -- Add_Industry --
      ------------------

      procedure Add_Industry
        (Colony : Athena.Handles.Colony.Colony_Class)
      is
      begin
         Total_Industry := Total_Industry + Colony.Industry;
      end Add_Industry;

      -----------------
      -- Check_Ships --
      -----------------

      procedure Check_Ships
        (Design            : Athena.Handles.Ship_Design.Ship_Design_Class;
         Industry_Per_Ship : Positive)
      is
         Required  : constant Natural :=
                       Natural
                         (Real'Truncation
                            (Total_Industry / Real (Industry_Per_Ship)));
         Available : Natural := 0;
      begin

         for Ship of Athena.Ships.Select_Managed_Ships
           (Athena.Empires.Attack_Manager (For_Empire))
         loop
            if Ship.Ship_Design.Identifier = Design.Identifier then
               Available := Available + 1;
            end if;
         end loop;

         if Available < Required then
            Log ("attack", For_Empire,
                 "design: " & Design.Name
                 & ": required" & Required'Image
                 & "; available" & Available'Image);

            Athena.Orders.Build_Ships
              (Empire   => For_Empire,
               Design   => Design,
               Fleet    => Athena.Handles.Fleet.Empty_Handle,
               Manager  => Manager,
               Send_To  => Athena.Empires.Capital (For_Empire),
               Count    => Required - Available,
               Priority => Manager.Priority);
         end if;
      end Check_Ships;

   begin
      Athena.Colonies.For_All_Colonies (For_Empire, Add_Industry'Access);

      Check_Ships (Athena.Empires.Destroyer_Design (For_Empire), 500);
      Check_Ships (Athena.Empires.Cruiser_Design (For_Empire), 2000);
      Check_Ships (Athena.Empires.Battleship_Design (For_Empire), 5000);
      Check_Ships (Athena.Empires.Carrier_Design (For_Empire), 10_000);

      Evaluate_Threats (For_Empire, Manager);

   end Create_Orders;

   ----------------------
   -- Evaluate_Threats --
   ----------------------

   procedure Evaluate_Threats
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
   is
      Max_Range   : constant Non_Negative_Real :=
                      Athena.Empires.Current_Tec_Level
                        (For_Empire, Athena.Technology.Drive)
                        * 5.0;
      Knowledge   : Athena.Knowledge.Stars.Star_Knowledge;

      procedure Check_Threat
        (Threat  : Athena.Handles.Empire.Empire_Class;
         Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean);

      ------------------
      -- Check_Threat --
      ------------------

      procedure Check_Threat
        (Threat  : Athena.Handles.Empire.Empire_Class;
         Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean)
      is
         Fleet : constant Athena.Handles.Fleet.Fleet_Class :=
                   Find_Available_Fleet
                     (For_Empire,
                      Athena.Empires.Get_Manager (For_Empire, Manager),
                      Nearest.Star, Star);
         Can_Launch : Boolean := True;
      begin

         if Fleet.Destination.Identifier = Star.Identifier then
            Stop := False;
            return;
         end if;

         Log ("attack", For_Empire, "checking threat from "
              & Threat.Name & " at " & Star.Name
              & ": nearest colony on "
              & Nearest.Star.Name
              & "; distance "
              & Image (Athena.Stars.Distance (Star, Nearest.Star)));

         if Fleet.Location.Identifier /= Nearest.Star.Identifier
           and then not Fleet.Destination.Has_Element
         then
            Athena.Orders.Move_Fleet (Fleet, Nearest.Star);
            Can_Launch := False;
         end if;

         for Ship of
           Athena.Ships.Select_Managed_Ships
             (Athena.Empires.Get_Manager (For_Empire, Manager))
         loop
            if not Ship.Fleet.Has_Element
              and then not Ship.Destination.Has_Element
              and then Ship.First_Order = 0
            then
               if Ship.Star.Identifier /= Nearest.Star.Identifier then
                  Athena.Ships.Move_To (Ship, Nearest.Star);
                  Can_Launch := False;
               elsif Fleet.Location.Identifier = Nearest.Star.Identifier then
                  Ship.Update_Ship.Set_Fleet (Fleet.Reference_Fleet).Done;
               end if;
            elsif Ship.Destination.Has_Element
              and then Ship.Destination.Identifier = Nearest.Star.Identifier
            then
               Can_Launch := False;
            end if;
         end loop;

         if Can_Launch then
            Athena.Orders.Move_Fleet (Fleet, Star);
         end if;

         Stop := True;

      end Check_Threat;

   begin
      Knowledge.Load (For_Empire);
      Knowledge.Iterate_Threats
        (Max_Range, Check_Threat'Access);
   end Evaluate_Threats;

   --------------------------
   -- Find_Available_Fleet --
   --------------------------

   function Find_Available_Fleet
     (For_Empire  : Athena.Handles.Empire.Empire_Class;
      Manager     : Athena.Handles.Empire_Manager.Empire_Manager_Class;
      Origin      : Athena.Handles.Star.Star_Class;
      Destination : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Fleet.Fleet_Class
   is
      use Athena.Handles.Fleet.Selections;
   begin
      for Fleet of Select_Where (Empire = For_Empire) loop
         if Fleet.Destination.Has_Element
           and then Fleet.Destination.Identifier = Origin.Identifier
         then
            return Fleet;
         elsif not Fleet.Destination.Has_Element
           and then Fleet.Location.Identifier = Origin.Identifier
         then
            return Fleet;
         elsif Fleet.Destination.Has_Element
           and then Fleet.Destination.Identifier = Destination.Identifier
         then
            return Fleet;
         end if;
      end loop;

      declare
         Closest_D : Non_Negative_Real := Non_Negative_Real'Last;
         Closest_F : Athena.Handles.Fleet.Fleet_Handle :=
                       Athena.Handles.Fleet.Empty_Handle;
      begin
         for Fleet of Select_Where (Empire = For_Empire) loop
            if not Fleet.Destination.Has_Element then
               declare
                  D : constant Non_Negative_Real :=
                        Athena.Stars.Distance
                          (Fleet.Location, Origin);
               begin
                  if D < Closest_D then
                     Closest_D := D;
                     Closest_F := Athena.Handles.Fleet.Get
                       (Fleet.Reference_Fleet);
                  end if;
               end;
            end if;
         end loop;

         if Closest_F.Has_Element then
            return Closest_F;
         end if;
      end;

      Log ("attack", For_Empire,
           "creating new fleet for attack from "
           & Origin.Name & " to " & Destination.Name);

      return Athena.Handles.Fleet.Create
        (Identifier  => Athena.Identifiers.Next_Identifier,
         Name        => Athena.Ships.Name_Fleet (For_Empire, "Task Force"),
         Empire      => For_Empire,
         Manager     => Manager,
         Location    => Origin,
         Destination => Athena.Handles.Star.Empty_Handle,
         Mass        => 0.0,
         Speed       => 0.0,
         Progress    => 0.0);

   end Find_Available_Fleet;

end Athena.Managers.Attack;
