with Athena.Orders;

with Athena.Handles.Colony.Selections;

package body Athena.Managers.Development is

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
   is
      use Athena.Handles.Colony.Selections;
   begin
      for Colony of Select_Where (Empire = For_Empire) loop
         if Colony.Industry < Colony.Pop then
            Athena.Orders.Order_Industry
              (Colony, Colony.Pop - Colony.Industry, Manager.Priority);
         end if;
      end loop;
   end Create_Orders;

end Athena.Managers.Development;
