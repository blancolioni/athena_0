with Athena.Orders;

with Athena.Handles.Colony.Selections;

package body Athena.Managers.Development is

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Priority   : Positive)
   is
      use Athena.Handles.Colony.Selections;
   begin
      for Colony of Select_Where (Empire = For_Empire) loop
         if Colony.Industry < Colony.Pop then
            Athena.Orders.Order_Industry
              (Colony, Colony.Pop - Colony.Industry, Priority);
         end if;
      end loop;
   end Create_Orders;

end Athena.Managers.Development;
