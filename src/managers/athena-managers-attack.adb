with Athena.Colonies;
with Athena.Empires;
with Athena.Orders;
with Athena.Ships;

with Athena.Handles.Colony;
with Athena.Handles.Fleet;
with Athena.Handles.Ship_Design;

package body Athena.Managers.Attack is

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

   end Create_Orders;

end Athena.Managers.Attack;
