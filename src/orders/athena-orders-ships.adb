with Athena.Real_Images;

with Athena.Colonies;
with Athena.Empires;
with Athena.Ships;

package body Athena.Orders.Ships is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   -------------------------
   -- Apply_Upgrade_Order --
   -------------------------

   procedure Apply_Upgrade_Order
     (Order : Athena.Handles.Upgrade_Order.Upgrade_Order_Class)
   is
      Owner  : constant Athena.Handles.Empire.Empire_Class :=
                 Order.Ship_Component.Ship.Empire;
      Star   : constant Athena.Handles.Star.Star_Class :=
                 Order.Ship_Component.Ship.Star;
      Colony : constant Athena.Handles.Colony.Colony_Class :=
                 Athena.Colonies.Get_Colony (Star);
      Mass   : constant Non_Negative_Real :=
                 Order.Ship_Component.Design_Component.Mass;
      New_Tec : constant Non_Negative_Real :=
                  Athena.Empires.Current_Tec_Level
                    (Owner, Order.Ship_Component
                     .Design_Component.Component.Technology);
   begin
      if Colony.Has_Element
        and then Colony.Empire.Identifier = Owner.Identifier
      then
         if Colony.Construct >= Mass * 10.0 then
            Athena.Colonies.Use_Assets
              (Colony      => Colony,
               Construct   => Mass * 10.0,
               Description =>
                 "upgrade "
               & Order.Ship_Component.Design_Component.Component.Tag
               & " on " & Owner.Name & " ship "
               & Order.Ship_Component.Ship.Name
               & " to " & Image (New_Tec));
            Athena.Ships.Upgrade_Component
              (Order.Ship_Component, New_Tec);
         end if;
      end if;
   end Apply_Upgrade_Order;

end Athena.Orders.Ships;
