with Athena.Colonies;
with Athena.Empires;
with Athena.Ships;
with Athena.Turns;

with Athena.Handles.Ship;
with Athena.Handles.Ship_Component.Selections;
with Athena.Handles.Upgrade_Order;

package body Athena.Managers.Upgrade is

   procedure Check_Component_Upgrade
     (Component : Athena.Handles.Ship_Component.Ship_Component_Class;
      Priority  : Positive);

   -----------------------------
   -- Check_Component_Upgrade --
   -----------------------------

   procedure Check_Component_Upgrade
     (Component : Athena.Handles.Ship_Component.Ship_Component_Class;
      Priority  : Positive)
   is
      Component_Tec : constant Non_Negative_Real := Component.Tec_Level;
      Condition     : constant Unit_Real := Component.Condition;
      Empire_Tec    : constant Non_Negative_Real :=
                        Athena.Empires.Current_Tec_Level
                          (Component.Ship.Empire,
                           Component.Design_Component.Component.Technology);
   begin
      if Component_Tec < Empire_Tec - 2.0
        or else (Component_Tec < Empire_Tec - 1.0 and then Condition <= 0.5)
        or else (Component_Tec < Empire_Tec and then Condition <= 0.25)
      then
         Athena.Handles.Upgrade_Order.Create
           (Turn           => Athena.Turns.Current_Turn,
            Empire         => Component.Ship.Empire,
            Priority       => Priority,
            Ship_Component => Component);
      end if;
   end Check_Component_Upgrade;

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Priority   : Positive)
   is

      procedure Check_Components
        (On_Ship : Athena.Handles.Ship.Ship_Class);

      ----------------------
      -- Check_Components --
      ----------------------

      procedure Check_Components
        (On_Ship : Athena.Handles.Ship.Ship_Class)
      is
         use Athena.Handles.Ship_Component.Selections;
      begin
         if On_Ship.Star.Has_Element
           and then not On_Ship.Destination.Has_Element
           and then Athena.Colonies.Get_Colony (On_Ship.Star).Has_Element
           and then On_Ship.Star.Owner.Identifier = On_Ship.Empire.Identifier
         then
            for Component of Select_Where (Ship = On_Ship) loop
               Check_Component_Upgrade (Component, Priority);
            end loop;
         end if;
      end Check_Components;

   begin
      Athena.Ships.For_All_Ships
        (Owned_By => For_Empire,
         Process  => Check_Components'Access);
   end Create_Orders;

end Athena.Managers.Upgrade;
