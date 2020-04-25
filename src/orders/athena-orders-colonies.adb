with Athena.Logging;
with Athena.Money;
with Athena.Real_Images;

with Athena.Colonies;
with Athena.Empires;

with Athena.Db;

package body Athena.Orders.Colonies is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Produce_Industry
     (Colony   : Athena.Handles.Colony.Colony_Class;
      Quantity : Non_Negative_Real);

   ------------------------
   -- Apply_Colony_Order --
   ------------------------

   procedure Apply_Colony_Order
     (Order : Athena.Handles.Colony_Order.Colony_Order_Class)
   is
      use all type Athena.Db.Colony_Order_Category;
   begin
      case Order.Category is
         when Build_Industry =>
            Produce_Industry (Order.Colony, Order.Value);
         when Produce_Material =>
            Athena.Colonies.Produce_Material (Order.Colony, Order.Value);
      end case;
   end Apply_Colony_Order;

   ----------------------
   -- Produce_Industry --
   ----------------------

   procedure Produce_Industry
     (Colony   : Athena.Handles.Colony.Colony_Class;
      Quantity : Non_Negative_Real)
   is
      use Athena.Money;
      Max : Non_Negative_Real := Quantity;
   begin
      Max := Real'Min (Max, Colony.Construct / 4.0);
      Max := Real'Min (Max, To_Real (Colony.Empire.Cash));

      if Colony.Material < Max then
         Athena.Colonies.Produce_Material (Colony, Max - Colony.Material);
         Max := Real'Min (Max, Colony.Construct / 4.0);
         Max := Real'Min (Max, To_Real (Colony.Empire.Cash));
      end if;

      Max := Real'Min (Max, Colony.Material);

      declare
         Produced      : constant Non_Negative_Real := Max;
         New_Construct : constant Non_Negative_Real :=
                           Colony.Construct - Produced * 4.0;
         New_Material  : constant Non_Negative_Real :=
                           Colony.Material - Produced;
         New_Industry  : constant Non_Negative_Real :=
                           Colony.Industry + Produced;
      begin
         Athena.Logging.Log
           (Colony.Empire.Name
            & ": colony on "
            & Colony.Star.Name
            & ": ordered "
            & Image (Quantity)
            & " industry"
            & "; available construct "
            & Image (Colony.Construct)
            & " cash "
            & Athena.Money.Show (Colony.Empire.Cash)
            & " material "
            & Image (Colony.Material)
            & "; produced "
            & Image (Produced)
            & "; new industry level "
            & Image (New_Industry));

         Colony.Update_Colony
           .Set_Industry (New_Industry)
           .Set_Construct (New_Construct)
           .Set_Material (New_Material)
           .Done;

         Athena.Empires.Pay (Colony.Empire, To_Money (Produced),
                             "industry construction");
      end;
   end Produce_Industry;

end Athena.Orders.Colonies;
