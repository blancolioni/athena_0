with Athena.Logging;
with Athena.Money;
with Athena.Turns;

with Athena.Managers;
with Athena.Orders.Colonies;
with Athena.Orders.Empires;
with Athena.Orders.Ships;

with Athena.Empires;
with Athena.Ships.Updates;

with Athena.Db.Colony_Order;
with Athena.Db.Research_Order;
with Athena.Db.Ship_Build_Order;
with Athena.Db.Upgrade_Order;

with Athena.Handles.Colony.Selections;
with Athena.Handles.Colony_Order;
with Athena.Handles.Empire.Selections;
with Athena.Handles.Research_Order;
with Athena.Handles.Ship.Selections;
with Athena.Handles.Ship_Build_Order;
with Athena.Handles.Upgrade_Order;

package body Athena.Updates is

   procedure Debt_Service
     (Empire : Athena.Handles.Empire.Empire_Class);

   procedure Fleet_Cost
     (Fleet_Owner : Athena.Handles.Empire.Empire_Class);

   procedure Execute_Colony_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class);

   procedure Execute_Upgrade_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class);

   procedure Execute_Build_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class);

   procedure Execute_Research_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class);

   procedure For_All_Empires
     (Process : not null access
        procedure (Empire : Athena.Handles.Empire.Empire_Class));

   procedure Before_Production
     (Colony : Athena.Handles.Colony.Colony_Class);

   procedure After_Production
     (Colony : Athena.Handles.Colony.Colony_Class);

   procedure For_All_Colonies
     (Process : not null access
        procedure (Colony : Athena.Handles.Colony.Colony_Class));

   ----------------------
   -- After_Production --
   ----------------------

   procedure After_Production
     (Colony : Athena.Handles.Colony.Colony_Class)
   is
   begin
      Athena.Empires.Earn
        (Colony.Empire, Athena.Money.To_Money (Colony.Construct),
         "unused production on " & Colony.Star.Name);
      Colony.Update_Colony.Set_Construct (0.0).Done;
   end After_Production;

   -----------------------
   -- Before_Production --
   -----------------------

   procedure Before_Production
     (Colony : Athena.Handles.Colony.Colony_Class)
   is
      Construct : constant Non_Negative_Real :=
                    Real'Min (Colony.Industry, Colony.Pop)
                    + (Real'Max (Colony.Industry, Colony.Pop)
                       - Colony.Industry)
                    / 4.0;
      New_Pop   : constant Non_Negative_Real :=
                    Colony.Pop + 0.1 * Colony.Pop * Colony.Star.Habitability;
      Max_Pop   : constant Non_Negative_Real :=
                    Non_Negative_Real (Colony.Star.Space);
   begin
      Colony.Update_Colony
        .Set_Construct (Construct)
        .Set_Pop (Real'Min (New_Pop, Max_Pop))
        .Done;
   end Before_Production;

   ------------------
   -- Debt_Service --
   ------------------

   procedure Debt_Service
     (Empire : Athena.Handles.Empire.Empire_Class)
   is
      use Athena.Money;
      Repay : constant Money_Type :=
                Min (Empire.Cash, Empire.Debt);
      New_Cash : constant Money_Type :=
                   Empire.Cash - Repay;
      New_Debt : constant Money_Type :=
                   Empire.Debt - Repay;
      Interest : constant Money_Type :=
                   Adjust (New_Debt, 1.1);
   begin
      if Repay > Zero then
         Athena.Logging.Log
           (Empire.Name & ": repaid " & Athena.Money.Show (Repay)
            & " debt"
            & ": remaining cash " & Athena.Money.Show (New_Cash)
            & "; remaining debt " & Athena.Money.Show (New_Debt));
      end if;
      if New_Debt > Zero then
         Athena.Logging.Log
           (Empire.Name & ": debt " & Athena.Money.Show (Empire.Debt)
            & "; interest " & Athena.Money.Show (Interest)
            & "; new debt " & Athena.Money.Show (New_Debt + Interest));
      end if;

      Empire.Update_Empire
        .Set_Cash (New_Cash)
        .Set_Debt (New_Debt + Interest)
        .Done;

   end Debt_Service;

   --------------------------
   -- Execute_Build_Orders --
   --------------------------

   procedure Execute_Build_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class)
   is
   begin
      for Order of
        Athena.Db.Ship_Build_Order.Select_Priority_Order_Bounded_By_Priority
          (Athena.Turns.Current_Turn.Reference_Turn,
           For_Empire.Reference_Empire,
           1, Natural'Last)
      loop
         Athena.Orders.Empires.Apply_Ship_Build_Order
           (Athena.Handles.Ship_Build_Order.Get
              (Order.Get_Ship_Build_Order_Reference));
      end loop;
   end Execute_Build_Orders;

   ---------------------------
   -- Execute_Colony_Orders --
   ---------------------------

   procedure Execute_Colony_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class)
   is
   begin
      for Order of
        Athena.Db.Colony_Order.Select_Priority_Order_Bounded_By_Priority
          (Athena.Turns.Current_Turn.Reference_Turn,
           For_Empire.Reference_Empire,
           1, Natural'Last)
      loop
         Athena.Logging.Log
           (For_Empire.Name & ": processing "
            & Order.Category'Image);
         Athena.Orders.Colonies.Apply_Colony_Order
           (Athena.Handles.Colony_Order.Get
              (Order.Get_Colony_Order_Reference));
      end loop;
   end Execute_Colony_Orders;

   -----------------------------
   -- Execute_Research_Orders --
   -----------------------------

   procedure Execute_Research_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class)
   is
   begin
      for Order of
        Athena.Db.Research_Order.Select_Priority_Order_Bounded_By_Priority
          (Athena.Turns.Current_Turn.Reference_Turn,
           For_Empire.Reference_Empire,
           1, Natural'Last)
      loop
         Athena.Orders.Empires.Apply_Research_Order
           (Athena.Handles.Research_Order.Get
              (Order.Get_Research_Order_Reference));
      end loop;
   end Execute_Research_Orders;

   ----------------------------
   -- Execute_Upgrade_Orders --
   ----------------------------

   procedure Execute_Upgrade_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class)
   is
   begin
      for Order of
        Athena.Db.Upgrade_Order.Select_Priority_Order_Bounded_By_Priority
          (Athena.Turns.Current_Turn.Reference_Turn,
           For_Empire.Reference_Empire,
           1, Natural'Last)
      loop
         Athena.Orders.Ships.Apply_Upgrade_Order
           (Athena.Handles.Upgrade_Order.Get
              (Order.Get_Upgrade_Order_Reference));
      end loop;
   end Execute_Upgrade_Orders;

   ----------------
   -- Fleet_Cost --
   ----------------

   procedure Fleet_Cost
     (Fleet_Owner : Athena.Handles.Empire.Empire_Class)
   is
      use Athena.Handles.Ship.Selections;
      Total_Ships : Natural := 0;
      Total_Mass  : Non_Negative_Real := 0.0;
   begin
      for Ship of Select_Where (Empire = Fleet_Owner) loop
         Total_Ships := Total_Ships + 1;
         Total_Mass  := Total_Mass + Athena.Ships.Mass (Ship);
      end loop;

      declare
         Total : constant Athena.Money.Money_Type :=
                   Athena.Money.To_Money
                     (Total_Mass / 10.0 + Real (Total_Ships));
      begin
         Athena.Empires.Pay (Fleet_Owner, Total, "fleet maintenance");
      end;
   end Fleet_Cost;

   ----------------------
   -- For_All_Colonies --
   ----------------------

   procedure For_All_Colonies
     (Process : not null access
        procedure (Colony : Athena.Handles.Colony.Colony_Class))
   is
      use Athena.Handles.Colony.Selections;
   begin
      for Colony of Select_All loop
         Process (Colony);
      end loop;
   end For_All_Colonies;

   ---------------------
   -- For_All_Empires --
   ---------------------

   procedure For_All_Empires
     (Process : not null access
        procedure (Empire : Athena.Handles.Empire.Empire_Class))
   is
      use Athena.Handles.Empire.Selections;
   begin
      for Empire of Select_All loop
         Process (Empire);
      end loop;
   end For_All_Empires;

   ----------------
   -- Run_Update --
   ----------------

   procedure Run_Update is
   begin
      Athena.Logging.Start_Logging
        ("update-"
         & Athena.Turns.Current_Turn_Image);

      For_All_Empires (Fleet_Cost'Access);

      Athena.Managers.Run_Managers;

      For_All_Colonies (Before_Production'Access);

      For_All_Empires (Execute_Upgrade_Orders'Access);

      For_All_Empires (Execute_Colony_Orders'Access);

      For_All_Empires (Execute_Build_Orders'Access);

      For_All_Empires (Execute_Research_Orders'Access);

      Athena.Ships.For_All_Ships (Athena.Ships.Updates.Update'Access);

      For_All_Colonies (After_Production'Access);

      For_All_Empires (Debt_Service'Access);

      Athena.Turns.Next_Turn;

      Athena.Logging.Stop_Logging;
   end Run_Update;

end Athena.Updates;
