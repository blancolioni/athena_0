with WL.String_Maps;
with WL.String_Sets;

with Athena.Logging;
with Athena.Money;
with Athena.Turns;

with Athena.Managers;
with Athena.Orders.Colonies;
with Athena.Orders.Empires;
with Athena.Orders.Ships;

with Athena.Colonies;
with Athena.Empires;
with Athena.Ships.Updates;

with Athena.Encounters.Manager;
with Athena.Treaties;

with Athena.Db.Colony_Order;
with Athena.Db.Research_Order;
with Athena.Db.Ship_Build_Order;
with Athena.Db.Upgrade_Order;

with Athena.Handles.Colony.Selections;
with Athena.Handles.Colony_Order;
with Athena.Handles.Empire.Selections;
with Athena.Handles.Fleet_Order.Selections;
with Athena.Handles.Research_Order;
with Athena.Handles.Ship.Selections;
with Athena.Handles.Ship_Build_Order;
with Athena.Handles.Star;
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

   procedure Check_Colony_Owner
     (Colony : Athena.Handles.Colony.Colony_Class);

   procedure For_All_Colonies
     (Process : not null access
        procedure (Colony : Athena.Handles.Colony.Colony_Class));

   procedure Run_Encounters;

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

   ------------------------
   -- Check_Colony_Owner --
   ------------------------

   procedure Check_Colony_Owner
     (Colony : Athena.Handles.Colony.Colony_Class)
   is
      Current_Owner : constant Athena.Handles.Empire.Empire_Class :=
                        Colony.Empire;
      New_Owner     : Athena.Handles.Empire.Empire_Handle :=
                        Athena.Handles.Empire.Empty_Handle;
      Star          : constant Athena.Handles.Star.Star_Class :=
                        Colony.Star;
      Present       : WL.String_Sets.Set;
      Count         : Natural := 0;

      procedure Record_Empire
        (For_Ship : Athena.Handles.Ship.Ship_Class);

      -------------------
      -- Record_Empire --
      -------------------

      procedure Record_Empire
        (For_Ship : Athena.Handles.Ship.Ship_Class)
      is
      begin
         if For_Ship.Alive
           and then Athena.Ships.Is_Armed (For_Ship)
           and then not Present.Contains (For_Ship.Empire.Identifier)
           and then (For_Ship.Empire.Identifier = Colony.Empire.Identifier
                     or else Athena.Treaties.At_War
                       (Colony.Empire, For_Ship.Empire))
         then
            Count := Count + 1;
            Present.Include (For_Ship.Empire.Identifier);
            if For_Ship.Empire.Identifier /= Current_Owner.Identifier then
               New_Owner :=
                 Athena.Handles.Empire.Get (For_Ship.Empire.Reference_Empire);
            end if;
         end if;
      end Record_Empire;

   begin
      Athena.Ships.For_All_Ships
        (Star, Record_Empire'Access);
      if not Present.Contains (Current_Owner.Identifier)
        and then Count = 1
      then
         Athena.Colonies.Capture_Colony (Colony, New_Owner);
         Athena.Logging.Log
           (New_Owner.Name & " captures colony on "
            & Star.Name & " from " & Current_Owner.Name);
      end if;
   end Check_Colony_Owner;

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
                   Adjust (New_Debt, 0.05);
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
           (Empire.Name & ": debt " & Athena.Money.Show (New_Debt)
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
         if Ship.Alive then
            Total_Ships := Total_Ships + 1;
            Total_Mass  := Total_Mass + Athena.Ships.Mass (Ship);
         end if;
      end loop;

      declare
         Total : constant Athena.Money.Money_Type :=
                   Athena.Money.To_Money
                     (Total_Mass / 2.0 + 5.0 * Real (Total_Ships));
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

   --------------------
   -- Run_Encounters --
   --------------------

   procedure Run_Encounters is

      type Star_Record is
         record
            Star    : Athena.Handles.Star.Star_Handle;
            Ships   : Athena.Ships.Ship_Lists.List;
            Hostile : Boolean;
         end record;

      package Star_Record_Maps is
        new WL.String_Maps (Star_Record);

      Star_Map : Star_Record_Maps.Map;

      procedure Record_Ship (Ship : Athena.Handles.Ship.Ship_Class);

      -----------------
      -- Record_Ship --
      -----------------

      procedure Record_Ship (Ship : Athena.Handles.Ship.Ship_Class) is
         Key : constant String := Ship.Star.Identifier;
      begin

         if not Ship.Alive then
            return;
         end if;

         if not Star_Map.Contains (Key) then
            Star_Map.Insert
              (Key,
               Star_Record'
                 (Star    =>
                      Athena.Handles.Star.Get (Ship.Star.Reference_Star),
                  Ships   => Athena.Ships.Ship_Lists.Empty_List,
                  Hostile => False));
         end if;

         if Athena.Ships.Is_Armed (Ship)
           and then not Star_Map (Key).Hostile
         then
            for Other_Ship of Star_Map (Key).Ships loop
               if Athena.Ships.Is_Armed (Other_Ship)
                 and then Other_Ship.Empire.Identifier
                   /= Ship.Empire.Identifier
               then
                  Athena.Logging.Log
                    ("encounter: checking treaty between "
                     & Other_Ship.Empire.Name
                     & " and " & Ship.Empire.Name);
               end if;

               if Athena.Treaties.At_War (Other_Ship.Empire, Ship.Empire) then
                  Star_Map (Key).Hostile := True;
               end if;
            end loop;
         end if;

         Star_Map (Key).Ships.Append
           (Athena.Handles.Ship.Get (Ship.Reference_Ship));

      end Record_Ship;

   begin
      Athena.Ships.For_All_Ships (Record_Ship'Access);

      for Element of Star_Map loop
         if Element.Hostile then
            Athena.Encounters.Manager.Resolve_Encounter
              (Element.Star, Element.Ships);
         end if;
      end loop;

   end Run_Encounters;

   ----------------
   -- Run_Update --
   ----------------

   procedure Run_Update is
   begin
      Athena.Logging.Start_Logging
        ("update-"
         & Athena.Turns.Current_Turn_Image);

      For_All_Empires (Fleet_Cost'Access);

      Athena.Ships.For_All_Ships (Athena.Ships.Updates.Repair'Access);

      Athena.Managers.Run_Managers;

      For_All_Colonies (Before_Production'Access);

      For_All_Empires (Execute_Upgrade_Orders'Access);

      For_All_Empires (Execute_Colony_Orders'Access);

      For_All_Empires (Execute_Build_Orders'Access);

      For_All_Empires (Execute_Research_Orders'Access);

      declare
         use Athena.Handles.Fleet_Order.Selections;
      begin
         for Order of
           Select_Where (Turn = Athena.Turns.Current_Turn)
         loop
            Order.Fleet.Update_Fleet
              .Set_Destination (Order.Destination.Reference_Star)
              .Set_Progress (0.0)
              .Done;
         end loop;
      end;

      Athena.Ships.For_All_Fleets (Athena.Ships.Updates.Update'Access);
      Athena.Ships.For_All_Ships (Athena.Ships.Updates.Update'Access);

      For_All_Colonies (After_Production'Access);

      Athena.Ships.For_All_Ships (Athena.Ships.Updates.Visit'Access);

      Run_Encounters;

      For_All_Colonies (Check_Colony_Owner'Access);

      For_All_Empires (Debt_Service'Access);

      Athena.Turns.Next_Turn;

      Athena.Logging.Stop_Logging;
   end Run_Update;

end Athena.Updates;
