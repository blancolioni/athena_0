with Athena.Empires;
with Athena.Ships;
with Athena.Turns;

with Athena.Orders;

with Athena.Logging;
with Athena.Real_Images;

with Athena.Handles.Fleet;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Design;
with Athena.Handles.Star;

with Athena.Db.Transport_Order;

package body Athena.Managers.Transportation is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Get_Transport
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      From_Star  : Athena.Handles.Star.Star_Handle;
      Capacity   : Non_Negative_Real;
      Ships      : out Athena.Ships.Ship_Lists.List;
      Remaining  : out Non_Negative_Real);

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
   is
   begin
      for Order of
        Athena.Db.Transport_Order.Select_Priority_Order_Bounded_By_Priority
          (Turn            => Athena.Turns.Current_Turn.Reference_Turn,
           Empire          => For_Empire.Reference_Empire,
           Start_Priority  => 1,
           Finish_Priority => Natural'Last)
      loop
         declare
            Ships   : Athena.Ships.Ship_Lists.List;
            Missing : Non_Negative_Real;
         begin
            Get_Transport
              (For_Empire => For_Empire,
               From_Star  => Athena.Handles.Star.Get (Order.From),
               Capacity   => Order.Quantity,
               Ships      => Ships,
               Remaining  => Missing);

            if Missing > 0.0 then
               declare
                  Transport : constant Handles.Ship_Design.Ship_Design_Class :=
                                Athena.Empires.Transport_Design (For_Empire);
                  Cargo     : constant Non_Negative_Real :=
                                Athena.Ships.Design_Cargo (Transport);
                  Required  : constant Natural :=
                                Natural (Missing / Cargo);
               begin
                  if Required > 0 then
                     Athena.Logging.Log
                       (For_Empire.Name & "/transport: "
                        & "missing capacity: " & Image (Missing)
                        & "; transport cargo: " & Image (Cargo)
                        & "; required transports:" & Required'Image);

                     Athena.Orders.Build_Ships
                       (Empire   => For_Empire,
                        Design   => Transport,
                        Count    => Required,
                        Fleet    => Athena.Handles.Fleet.Empty_Handle,
                        Manager  => Manager,
                        Send_To  =>
                          Athena.Handles.Star.Get (Order.From),
                        Priority => Manager.Priority);
                  end if;
               end;
            end if;

            declare
               use type Athena.Db.Star_Reference;
               Remaining : Non_Negative_Real := Order.Quantity;
            begin
               for Ship of Ships loop
                  declare
                     Loaded : constant Non_Negative_Real :=
                                Real'Min (Remaining,
                                          Athena.Ships.Available_Space (Ship));
                     Have   : constant Non_Negative_Real :=
                                Athena.Ships.Current_Cargo
                                  (Ship, Order.Cargo);
                  begin
                     if Have < Loaded then
                        if Ship.Star.Reference_Star /= Order.From then
                           Athena.Ships.Move_To
                             (Ship,
                              Athena.Handles.Star.Get (Order.From));
                        end if;

                        Athena.Ships.Load (Ship, Order.Cargo, Loaded - Have);
                     end if;

                     Remaining := Remaining - Loaded;

                     Athena.Ships.Move_To
                       (Ship,
                        Athena.Handles.Star.Get (Order.To));
                     Athena.Ships.Unload (Ship, Order.Cargo, Loaded);
                  end;
               end loop;
            end;
         end;
      end loop;
   end Create_Orders;

   -------------------
   -- Get_Transport --
   -------------------

   procedure Get_Transport
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      From_Star  : Athena.Handles.Star.Star_Handle;
      Capacity   : Non_Negative_Real;
      Ships      : out Athena.Ships.Ship_Lists.List;
      Remaining  : out Non_Negative_Real)
   is
      pragma Unreferenced (From_Star);
      Available : Non_Negative_Real := 0.0;

      procedure Check_Available
        (Ship : Athena.Handles.Ship.Ship_Class);

      ---------------------
      -- Check_Available --
      ---------------------

      procedure Check_Available
        (Ship : Athena.Handles.Ship.Ship_Class)
      is
      begin
         if Available >= Capacity then
            return;
         end if;

         if Ship.First_Order = 0
           and then not Ship.Destination.Has_Element
           and then Athena.Ships.Total_Cargo_Space (Ship) > 0.0
         then
            Available := Available
              + Athena.Ships.Total_Cargo_Space (Ship);
            Ships.Append (Athena.Handles.Ship.Get (Ship.Reference_Ship));
         end if;
      end Check_Available;

   begin
      Ships.Clear;

      Athena.Ships.For_All_Ships (For_Empire, Check_Available'Access);

      Remaining := Real'Max (Capacity - Available, 0.0);
   end Get_Transport;

end Athena.Managers.Transportation;
