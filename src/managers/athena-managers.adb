with WL.String_Maps;

with Athena.Logging;

with Athena.Handles.Empire_Manager.Selections;

with Athena.Managers.Attack;
with Athena.Managers.Colonization;
with Athena.Managers.Defend;
with Athena.Managers.Development;
with Athena.Managers.Exploration;
with Athena.Managers.Research;
with Athena.Managers.Transportation;
with Athena.Managers.Upgrade;

package body Athena.Managers is

   type Create_Orders_Handler is access
     procedure (Empire   : Athena.Handles.Empire.Empire_Class;
                Priority : Positive);

   package Manager_Orders_Maps is
     new WL.String_Maps (Create_Orders_Handler);

   Manager_Orders : Manager_Orders_Maps.Map;

   -------------------
   -- Load_Managers --
   -------------------

   procedure Load_Managers is
   begin
      Manager_Orders.Insert
        ("attack", Athena.Managers.Attack.Create_Orders'Access);
      Manager_Orders.Insert
        ("colonize", Athena.Managers.Colonization.Create_Orders'Access);
      Manager_Orders.Insert
        ("defend", Athena.Managers.Defend.Create_Orders'Access);
      Manager_Orders.Insert
        ("develop", Athena.Managers.Development.Create_Orders'Access);
      Manager_Orders.Insert
        ("explore", Athena.Managers.Exploration.Create_Orders'Access);
      Manager_Orders.Insert
        ("research", Athena.Managers.Research.Create_Orders'Access);
      Manager_Orders.Insert
        ("transport", Athena.Managers.Transportation.Create_Orders'Access);
      Manager_Orders.Insert
        ("upgrade", Athena.Managers.Upgrade.Create_Orders'Access);
   end Load_Managers;

   ---------
   -- Log --
   ---------

   procedure Log
     (Manager_Tag : String;
      Empire      : Athena.Handles.Empire.Empire_Class;
      Message     : String)
   is
   begin
      Athena.Logging.Log
        (Empire.Name & "/" & Manager_Tag & ": "
         & Message);
   end Log;

   ------------------
   -- Run_Managers --
   ------------------

   procedure Run_Managers is
      use Athena.Handles.Empire_Manager.Selections;
   begin

      for EM of Select_Where (Enabled = True) loop
         if Manager_Orders.Contains (EM.Manager.Tag) then
            Athena.Logging.Log
              ("running manager: " & EM.Empire.Name & "/" & EM.Manager.Tag);
            Manager_Orders.Element (EM.Manager.Tag)
              (EM.Empire, EM.Manager.Priority);
         else
            Athena.Logging.Log
              ("no handler for manager: " & EM.Manager.Tag);
         end if;
      end loop;

   end Run_Managers;

end Athena.Managers;
