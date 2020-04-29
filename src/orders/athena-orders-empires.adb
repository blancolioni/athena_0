with Athena.Logging;
with Athena.Money;

with Athena.Colonies;
with Athena.Empires;
with Athena.Ships.Create;

package body Athena.Orders.Empires is

   --------------------------
   -- Apply_Research_Order --
   --------------------------

   procedure Apply_Research_Order
     (Order : Athena.Handles.Research_Order.Research_Order_Class)
   is
      Required   : constant Non_Negative_Real := Order.Construct;
      Available  : Non_Negative_Real := 0.0;
      Investment : Non_Negative_Real := 0.0;

      procedure Add_Construct (Colony : Athena.Handles.Colony.Colony_Class);

      procedure Spend_Construct (Colony : Athena.Handles.Colony.Colony_Class);

      -------------------
      -- Add_Construct --
      -------------------

      procedure Add_Construct (Colony : Athena.Handles.Colony.Colony_Class) is
      begin
         Available := Available + Colony.Construct;
      end Add_Construct;

      ---------------------
      -- Spend_Construct --
      ---------------------

      procedure Spend_Construct
        (Colony : Athena.Handles.Colony.Colony_Class)
      is
      begin
         Athena.Colonies.Use_Assets
           (Colony      => Colony,
            Construct   => Colony.Construct * Investment / Available,
            Material    => 0.0,
            Description => Order.Technology.Tag & " research");
      end Spend_Construct;

   begin

      Athena.Colonies.For_All_Colonies (Order.Empire, Add_Construct'Access);

      if Available > 0.0 then
         Investment := Real'Min (Required, Available);

         declare

         begin
            Athena.Colonies.For_All_Colonies
              (Order.Empire, Spend_Construct'Access);

            Athena.Empires.Add_Investment
              (Order.Empire, Order.Technology, Investment);

         end;
      end if;

   end Apply_Research_Order;

   ----------------------------
   -- Apply_Ship_Build_Order --
   ----------------------------

   procedure Apply_Ship_Build_Order
     (Order : Athena.Handles.Ship_Build_Order.Ship_Build_Order_Class)
   is
      Mass           : constant Non_Negative_Real :=
                         Athena.Ships.Design_Mass (Order.Ship_Design);
      Material_Cost  : constant Non_Negative_Real := Mass;
      Construct_Cost : constant Non_Negative_Real := Mass * 10.0;

      function Immediately_Available
        (Colony : Athena.Handles.Colony.Colony_Class)
         return Boolean
      is (Colony.Material >= Material_Cost
          and then Colony.Construct >= Construct_Cost);

      function Potentially_Available
        (Colony : Athena.Handles.Colony.Colony_Class)
         return Boolean
      is (Athena.Colonies.Can_Provide
          (Colony    => Colony,
           Construct => Construct_Cost,
           Material  => Material_Cost));

      procedure Execute (Colony : Athena.Handles.Colony.Colony_Class);

      -------------
      -- Execute --
      -------------

      procedure Execute (Colony : Athena.Handles.Colony.Colony_Class) is
      begin
         if Colony.Material < Material_Cost then
            Athena.Colonies.Produce_Material
              (Colony, Material_Cost - Colony.Material);
         end if;

         pragma Assert (Colony.Construct >= Construct_Cost);
         pragma Assert (Colony.Material >= Material_Cost);

         Athena.Ships.Create.Create_Ship
           (Empire => Order.Empire,
            Star   => Colony.Star,
            Design => Order.Ship_Design,
            Fleet  => Order.Fleet,
            Manager => Order.Manager,
            Name   =>
              Athena.Ships.Name_Ship (Order.Empire, Order.Ship_Design.Name),
            Destination => Order.Send_To);

         Athena.Colonies.Use_Assets
           (Colony      => Colony,
            Construct   => Construct_Cost,
            Material    => Material_Cost,
            Description =>
              "building class " & Order.Ship_Design.Name & " ship");

         Athena.Empires.Pay
           (Order.Empire,
            Athena.Money.To_Money (Construct_Cost),
            "building class " & Order.Ship_Design.Name & " ship");

      end Execute;

      Immediate_Colony   : constant Athena.Handles.Colony.Colony_Class :=
                             Athena.Colonies.Find_Colony
                               (Order.Empire, Immediately_Available'Access);
   begin

      if Immediate_Colony.Has_Element then
         Execute (Immediate_Colony);
      else
         declare
            Potential_Colony : constant Athena.Handles.Colony.Colony_Class :=
                                 Athena.Colonies.Find_Colony
                                   (Order.Empire,
                                    Potentially_Available'Access);
         begin
            if Potential_Colony.Has_Element then
               Execute (Potential_Colony);
            else
               Athena.Logging.Log
                 (Order.Empire.Name & ": skipping " & Order.Ship_Design.Name
                  & " build due to insufficient resources");
            end if;
         end;
      end if;

   end Apply_Ship_Build_Order;

end Athena.Orders.Empires;
