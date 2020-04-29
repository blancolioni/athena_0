with Athena.Colonies;
with Athena.Empires;
with Athena.Ships;

with Athena.Orders;

with Athena.Handles.Colony;
with Athena.Handles.Fleet;
with Athena.Handles.Ship;

package body Athena.Managers.Defend is

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
   is

      procedure Check_Defenders (Colony : Athena.Handles.Colony.Colony_Class);

      ---------------------
      -- Check_Defenders --
      ---------------------

      procedure Check_Defenders
        (Colony : Athena.Handles.Colony.Colony_Class)
      is
         Required  : constant Positive :=
                       Natural'Max
                         (Natural (Colony.Industry / 1000.0),
                          Natural (Colony.Pop / 1000.0))
                       + 1;
         Available : Natural := 0;
      begin
         for Ship of
           Athena.Ships.Select_Managed_Ships
             (Athena.Empires.Defense_Manager (For_Empire))
         loop
            if (Ship.Star.Identifier = Colony.Star.Identifier
                and then not Ship.Destination.Has_Element)
              or else Ship.Destination.Identifier = Colony.Star.Identifier
            then
               Available := Available + 1;
            end if;
         end loop;

         if Available < Required then
            Log ("defend", For_Empire,
                 "colony on "
                 & Colony.Star.Name
                 & ": assigned defenders" & Available'Image
                 & "; required" & Required'Image);

            Athena.Orders.Build_Ships
              (Empire   => For_Empire,
               Design   => Athena.Empires.Defender_Design (For_Empire),
               Fleet    => Athena.Handles.Fleet.Empty_Handle,
               Manager  => Manager,
               Send_To  => Colony.Star,
               Count    => Required - Available,
               Priority => Manager.Priority);
         end if;

      end Check_Defenders;

   begin
      Athena.Colonies.For_All_Colonies (For_Empire, Check_Defenders'Access);
   end Create_Orders;

end Athena.Managers.Defend;
