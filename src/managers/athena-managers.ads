private with Athena.Handles.Empire;

package Athena.Managers is

   procedure Load_Managers;

   procedure Run_Managers;

private

   procedure Log
     (Manager_Tag : String;
      Empire      : Athena.Handles.Empire.Empire_Class;
      Message     : String);

end Athena.Managers;
