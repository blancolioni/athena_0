with Athena.Handles.Research_Order;
with Athena.Handles.Ship_Build_Order;

package Athena.Orders.Empires is

   procedure Apply_Research_Order
     (Order : Athena.Handles.Research_Order.Research_Order_Class);

   procedure Apply_Ship_Build_Order
     (Order : Athena.Handles.Ship_Build_Order.Ship_Build_Order_Class);

end Athena.Orders.Empires;
