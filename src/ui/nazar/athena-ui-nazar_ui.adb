with Ada.Containers.Doubly_Linked_Lists;

with Nazar.Builder.Gtk_Creator;
with Nazar.Controllers.Draw;

with Nazar.Models.Draw;
with Nazar.Models.Text;

with Nazar.Views.Button;
with Nazar.Views.Draw;

with Nazar.Main;
with Nazar.Gtk_Main;
with Nazar.Signals;

with Athena.UI.Models.Galaxy;

with Athena.Updates;

with Athena.Options;
with Athena.Paths;

package body Athena.UI.Nazar_UI is

   package Model_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Nazar.Models.Nazar_Model, Nazar.Models."=");

   type Athena_Nazar_UI is
     new Athena_User_Interface
     and Nazar.Signals.User_Data_Interface with
      record
         Top            : Nazar.Views.Nazar_View;
         Models         : Model_Lists.List;
         Galaxy_Model   : Nazar.Models.Draw.Nazar_Draw_Model;
         Galaxy_Control : Nazar.Controllers.Draw.Nazar_Draw_Controller_Record;
      end record;

   overriding procedure Start
     (UI : in out Athena_Nazar_UI);

   procedure On_Update_Clicked
     (User_Data : Nazar.Signals.User_Data_Interface'Class);

   ------------
   -- Get_UI --
   ------------

   function Get_UI
     (Empire : Athena.Handles.Empire.Empire_Class)
      return Athena_User_Interface'Class
   is
      Builder : constant Nazar.Builder.Nazar_Builder :=
        Nazar.Builder.Nazar_Builder_New
          (Creator     => Nazar.Builder.Gtk_Creator.Get_Gtk_Creator,
           Config_Path => Athena.Paths.Config_File ("ui/athena.nazar"));
   begin
      Nazar.Main.Init;
      return Result : Athena_Nazar_UI do
         Result.Top := Builder.Get_View ("Athena");
         Result.Galaxy_Model := Athena.UI.Models.Galaxy.Galaxy_Model (Empire);
         Result.Models.Append (Nazar.Models.Nazar_Model (Result.Galaxy_Model));

         Result.Galaxy_Control.Start_Draw
           (Model => Result.Galaxy_Model,
            View  =>
              Nazar.Views.Draw.Nazar_Draw_View
                (Builder.Get_View ("galaxy")));

         Builder.Get_View ("empire-label").Set_Property ("text", Empire.Name);

         declare
            Model : constant Nazar.Models.Text.Nazar_Text_Model :=
                      Athena.UI.Models.Current_Turn_Model;
         begin
            Builder.Get_View ("turn-label").Set_Model (Model);
            Result.Models.Append (Nazar.Models.Nazar_Model (Model));
         end;

         declare
            Model : constant Nazar.Models.Text.Nazar_Text_Model :=
                      Athena.UI.Models.Current_Cash_Model (Empire);
         begin
            Builder.Get_View ("cash-label").Set_Model (Model);
            Result.Models.Append (Nazar.Models.Nazar_Model (Model));
         end;

         declare
            Model : constant Nazar.Models.Text.Nazar_Text_Model :=
                      Athena.UI.Models.Current_Debt_Model (Empire);
         begin
            Builder.Get_View ("debt-label").Set_Model (Model);
            Result.Models.Append (Nazar.Models.Nazar_Model (Model));
         end;

         Nazar.Views.Button.Nazar_Button_View
           (Builder.Get_View ("update"))
             .On_Activate (On_Update_Clicked'Access, Result);

         if Athena.Options.Auto_Update then
            Nazar.Gtk_Main.Start_Timer
              (Timeout   => Duration (Athena.Options.Update_Interval),
               User_Data => Result,
               Callback  => On_Update_Clicked'Access);
         end if;

      end return;
   end Get_UI;

   -----------------------
   -- On_Update_Clicked --
   -----------------------

   procedure On_Update_Clicked
     (User_Data : Nazar.Signals.User_Data_Interface'Class)
   is
      UI : Athena_Nazar_UI'Class renames Athena_Nazar_UI'Class (User_Data);
   begin
      Athena.Updates.Run_Update;
      for Model of UI.Models loop
         Model.Reload;
      end loop;
   end On_Update_Clicked;

   -----------
   -- Start --
   -----------

   overriding procedure Start
     (UI : in out Athena_Nazar_UI)
   is
   begin
      UI.Top.Show;
   end Start;

end Athena.UI.Nazar_UI;
