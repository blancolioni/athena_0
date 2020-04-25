with Athena.Colonies;
with Athena.Empires;

with Athena.Orders;

with Athena.Handles.Colony;
with Athena.Handles.Ship.Selections;

package body Athena.Managers.Defend is

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class; Priority : Positive)
   is

      procedure Check_Defender (Colony : Athena.Handles.Colony.Colony_Class);

      --------------------
      -- Check_Defender --
      --------------------

      procedure Check_Defender
        (Colony : Athena.Handles.Colony.Colony_Class)
      is
         use Athena.Handles.Ship.Selections;
      begin
         for Ship of
           Select_Where (Fleet = Athena.Empires.Defender_Fleet (For_Empire))
         loop
            if (Ship.Star.Identifier = Colony.Star.Identifier
                and then not Ship.Destination.Has_Element)
              or else Ship.Destination.Identifier = Colony.Star.Identifier
            then
               return;
            end if;
         end loop;

         Athena.Orders.Build_Ships
           (Empire   => For_Empire,
            Design   => Athena.Empires.Defender_Design (For_Empire),
            Fleet    => Athena.Empires.Defender_Fleet (For_Empire),
            Send_To  => Colony.Star,
            Count    => 1,
            Priority => Priority);
      end Check_Defender;

   begin
      Athena.Colonies.For_All_Colonies (For_Empire, Check_Defender'Access);
   end Create_Orders;

end Athena.Managers.Defend;
