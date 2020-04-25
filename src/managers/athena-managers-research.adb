with Athena.Money;

with Athena.Handles.Colony;

with Athena.Colonies;
with Athena.Orders;
with Athena.Technology;

package body Athena.Managers.Research is

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Priority   : Positive)
   is
      use Athena.Money;

      Allocation : constant Unit_Real :=
                     (if For_Empire.Cash < To_Money (1_000.0)
                      then 0.1
                      elsif For_Empire.Cash < To_Money (10_000.0)
                      then 0.2
                      else 0.4);

      Total_Investment : Non_Negative_Real := 0.0;

      procedure Add_Investment (Colony : Athena.Handles.Colony.Colony_Class);

      --------------------
      -- Add_Investment --
      --------------------

      procedure Add_Investment
        (Colony : Athena.Handles.Colony.Colony_Class)
      is
      begin
         Total_Investment := Total_Investment + Colony.Industry * Allocation;
      end Add_Investment;

   begin
      Athena.Colonies.For_All_Colonies
        (For_Empire, Add_Investment'Access);

      declare
         Drive_Investment : constant Non_Negative_Real :=
                              Total_Investment / 2.0;
      begin
         Athena.Orders.Research_Technology
           (For_Empire, Athena.Technology.Drive, Drive_Investment, Priority);
         Total_Investment := Total_Investment - Drive_Investment;
      end;

      declare
         Weapon_Investment : constant Non_Negative_Real :=
                               Total_Investment / 2.0;
      begin
         Athena.Orders.Research_Technology
           (For_Empire, Athena.Technology.Weapon, Weapon_Investment, Priority);
         Total_Investment := Total_Investment - Weapon_Investment;
      end;

      declare
         Shield_Investment : constant Non_Negative_Real :=
                              Total_Investment / 1.5;
      begin
         Athena.Orders.Research_Technology
           (For_Empire, Athena.Technology.Shield, Shield_Investment, Priority);
         Total_Investment := Total_Investment - Shield_Investment;
      end;

      Athena.Orders.Research_Technology
        (For_Empire, Athena.Technology.Cargo, Total_Investment, Priority);

   end Create_Orders;

end Athena.Managers.Research;
