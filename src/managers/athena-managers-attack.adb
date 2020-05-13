with Athena.Colonies;
with Athena.Empires;
with Athena.Orders;
with Athena.Ships;
with Athena.Stars;
with Athena.Technology;
with Athena.Treaties;

with Athena.Knowledge.Stars;

with Athena.Identifiers;
with Athena.Logging;

with Athena.Handles.Colony;
with Athena.Handles.Empire_Manager;
with Athena.Handles.Fleet.Selections;
with Athena.Handles.Ship;
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

   function Sufficient_Attack_Force
     (Knowledge : Athena.Knowledge.Stars.Star_Knowledge'Class;
      Fleet     : Athena.Handles.Fleet.Fleet_Class;
      Target    : Athena.Handles.Star.Star_Class)
      return Boolean;

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

      Monitored_Count : Natural := 0;
      Required_Ships  : Natural := 0;
      Recon_Ships     : Athena.Ships.Ship_Lists.List;

      procedure Check_Threat
        (Threat  : Athena.Handles.Empire.Empire_Class;
         Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean);

      procedure Monitor_Threat
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
         Fleet          : constant Athena.Handles.Fleet.Fleet_Class :=
                            Find_Available_Fleet
                              (For_Empire,
                               Athena.Empires.Get_Manager
                                 (For_Empire, Manager),
                               Nearest.Star, Star);
         Can_Launch     : Boolean := True;
         Recon_Id       : constant String :=
                            Athena.Empires.Recon_Design (For_Empire)
                            .Identifier;
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
            if Ship.Ship_Design.Identifier /= Recon_Id then
               if not Ship.Fleet.Has_Element
                 and then not Ship.Destination.Has_Element
                 and then Ship.First_Order = 0
               then
                  if Ship.Star.Identifier /= Nearest.Star.Identifier then
                     Athena.Ships.Move_To (Ship, Nearest.Star);
                  elsif Fleet.Location.Identifier
                    = Nearest.Star.Identifier
                  then
                     Ship.Update_Ship
                       .Set_Fleet (Fleet.Reference_Fleet)
                       .Set_Script ("attack")
                       .Done;
                  end if;
               elsif Ship.Destination.Has_Element
                 and then Ship.Destination.Identifier = Nearest.Star.Identifier
               then
                  null;
                  --  Can_Launch := False;
               end if;
            end if;
         end loop;

         if Can_Launch
           and then Sufficient_Attack_Force (Knowledge, Fleet, Star)
         then
            if not Athena.Treaties.At_War (For_Empire, Star.Owner) then
               Athena.Treaties.Declare_War (For_Empire, Star.Owner);
            end if;
            Athena.Orders.Move_Fleet (Fleet, Star);
         end if;

         Stop := True;

      end Check_Threat;

      --------------------
      -- Monitor_Threat --
      --------------------

      procedure Monitor_Threat
        (Threat  : Athena.Handles.Empire.Empire_Class;
         Star    : Athena.Handles.Star.Star_Class;
         Nearest : Athena.Handles.Colony.Colony_Class;
         Stop    : out Boolean)
      is
         pragma Unreferenced (Nearest);
         Min_Distance : Non_Negative_Real := Non_Negative_Real'Last;
         Assigned     : Athena.Handles.Ship.Ship_Handle :=
                          Athena.Handles.Ship.Empty_Handle;
      begin
         Stop := False;

         Monitored_Count := Monitored_Count + 1;
         if Knowledge.Turns_Since_Last_Visit (Star) <= Monitored_Count then
            return;
         end if;

         for Ship of Recon_Ships loop
            if Athena.Ships.Is_Idle (Ship) then
               declare
                  D : constant Non_Negative_Real :=
                        Athena.Stars.Distance (Ship.Star, Star);
               begin
                  if D < Min_Distance then
                     Min_Distance := D;
                     Assigned := Athena.Handles.Ship.Get (Ship.Reference_Ship);
                  end if;
               end;
            end if;
         end loop;

         if Assigned.Has_Element then
            Log ("attack", For_Empire,
                 "ordering " & Assigned.Name
                 & " to observe "
                 & Threat.Name & " colony on " & Star.Name);

            Athena.Orders.Set_Destination (Assigned, Star, Manager.Priority);
         else
            Required_Ships := Required_Ships + 1;
         end if;

      end Monitor_Threat;

   begin
      Knowledge.Load (For_Empire);

      for Recon_Ship of
        Athena.Ships.Select_Managed_Ships
          (Athena.Empires.Attack_Manager (For_Empire))
      loop
         if Recon_Ship.Ship_Design.Identifier
           = Athena.Empires.Recon_Design (For_Empire).Identifier
         then
            Recon_Ships.Append
              (Athena.Handles.Ship.Get (Recon_Ship.Reference_Ship));
         end if;
      end loop;

      Knowledge.Iterate_Threats
        (Max_Range, Monitor_Threat'Access);
      Knowledge.Iterate_Threats
        (Max_Range, Check_Threat'Access);

      if Required_Ships > 0 then
         Athena.Orders.Build_Ships
           (Empire   => For_Empire,
            Design   => Athena.Empires.Recon_Design (For_Empire),
            Fleet    => Athena.Handles.Fleet.Empty_Handle,
            Manager  => Manager,
            Send_To  => Athena.Handles.Star.Empty_Handle,
            Count    => Required_Ships,
            Priority => Manager.Priority);
      end if;

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

   function Sufficient_Attack_Force
     (Knowledge : Athena.Knowledge.Stars.Star_Knowledge'Class;
      Fleet     : Athena.Handles.Fleet.Fleet_Class;
      Target    : Athena.Handles.Star.Star_Class)
      return Boolean
   is
      Fleet_Ships   : Athena.Ships.Ship_Lists.List;
      Opposition    : constant Athena.Knowledge.Stars.Known_Ship_Lists.List :=
                        Knowledge.Get_Known_Ships (Target);
      Their_Weapons : Non_Negative_Real := 0.0;
      Our_Weapons   : Non_Negative_Real := 0.0;
   begin
      for Ship of Opposition loop
         Their_Weapons := Their_Weapons + Ship.Weapon_Mass;
      end loop;
      Athena.Ships.Get_Ships (Fleet, Fleet_Ships);
      for Ship of Fleet_Ships loop
         Our_Weapons := Our_Weapons + Athena.Ships.Weapon_Mass (Ship);
      end loop;

      Athena.Logging.Log
        (Fleet.Empire.Name & ": checking forces for attack on "
         & Target.Name & " owned by " & Target.Owner.Name
         & ": we have" & Fleet_Ships.Length'Image
         & " ships with weapon mass "
         & Image (Our_Weapons)
         & "; they have " & Opposition.Length'Image
         & " ships with weapon mass "
         & Image (Their_Weapons));
      return Our_Weapons > Their_Weapons * 1.5;

   end Sufficient_Attack_Force;

end Athena.Managers.Attack;
