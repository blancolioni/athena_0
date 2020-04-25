with Athena.UI.Nazar_UI;

package body Athena.UI.Launch is

   ------------
   -- Get_UI --
   ------------

   function Get_UI
     (Empire : Athena.Handles.Empire.Empire_Class)
      return Athena_User_Interface'Class
   is
   begin
      return Athena.UI.Nazar_UI.Get_UI (Empire);
   end Get_UI;

end Athena.UI.Launch;
