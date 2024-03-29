with Ada.Calendar;
with Ada.Text_IO;

with WL.Processes;

with Athena.Options;

with Athena.Logging;
with Athena.Logs;
with Athena.Real_Images;

with Athena.Server;

with Athena.Encounters;
with Athena.Updates;

with Athena.Handles.Empire;
with Athena.Handles.Encounter;
with Athena.UI.Launch;

with Athena.Db.Database;
with Athena.Db.Empire;

procedure Athena.Driver is

   Database_Open : Boolean := False;

begin

   Athena.Server.Initialize;

   if Athena.Options.Create then
      Athena.Logging.Start_Logging ("create");
      Athena.Server.Create_Scenario;
      Athena.Logging.Stop_Logging;
      return;
   end if;

   if Athena.Options.Add_Empire then
      declare
         Name : constant String := Athena.Options.Name;
      begin
         if Name = "" then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "missing argument: name");
            return;
         end if;

         Athena.Logging.Start_Logging ("add-empire");

         Athena.Db.Database.Open;
         Database_Open := True;

         Athena.Server.Add_Empire
           (Name      => Name,
            Plural    => Athena.Options.Plural,
            Adjective => Athena.Options.Adjective,
            Capital   => Athena.Options.Capital,
            Color     => Athena.Options.Color);

         Athena.Db.Database.Close;
         Database_Open := False;

      end;
      return;
   end if;

   if Athena.Options.Update then
      Athena.Db.Database.Open;
      Database_Open := True;

      Athena.Server.Load;

      declare
         Count : constant Positive :=
                   Natural'Max (Athena.Options.Update_Count, 1);
         Process : WL.Processes.Process_Type;
         Start   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      begin
         Process.Start_Bar ("updating", Count);
         for I in 1 .. Count  loop
            Athena.Updates.Run_Update;
            Process.Tick;
         end loop;
         Process.Finish;

         declare
            use Ada.Calendar;
         begin
            Ada.Text_IO.Put_Line
              ("executed" & Count'Image & " update"
               & (if Count = 1 then "" else "s")
               & " in "
               & Athena.Real_Images.Approximate_Image
                 (Real (Clock - Start))
               & "s");
         end;
      end;

      Athena.Server.Save;

      Athena.Db.Database.Close;
      Database_Open := False;

      return;
   end if;

   if Athena.Options.View_Encounter then

      if Athena.Options.Star_Name = "" then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "missing option: --star-name");
         return;
      end if;

      if Athena.Options.Turn = 0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "missing option: --turn");
         return;
      end if;

      Athena.Db.Database.Open;
      Database_Open := True;

--        declare
--           Star : constant Athena.Handles.Star.Star_Handle :=
--                    Athena.Stars.Find_Star (Athena.Options.Star_Name);
--           Turn : constant Athena.Handles.Turn.Turn_Class :=
--                    Athena.Turns.Get_Turn (Athena.Options.Turn);
--
--        begin
--           if not Star.Has_Element then
--              Ada.Text_IO.Put_Line
--                (Ada.Text_IO.Standard_Error,
--                 "cannot find star: " & Athena.Options.Star_Name);
--           elsif not Turn.Has_Element then
--              Ada.Text_IO.Put_Line
--                (Ada.Text_IO.Standard_Error,
--                 "no such turn: " & Athena.Options.Turn'Image);
--           else

      declare
         Encounter : constant Athena.Handles.Encounter.Encounter_Class :=
                       Athena.Encounters.Find
                         (Athena.Options.Star_Name,
                          Athena.Options.Turn);
      begin
         if Encounter.Has_Element then
            declare
               UI : Athena.UI.Athena_User_Interface'Class :=
                      Athena.UI.Launch.Get_Encounter_UI
                        (Encounter);
            begin
               UI.Start;
            end;
         end if;
      end;

      Athena.Db.Database.Close;
      Database_Open := False;

      return;
   end if;

   Athena.Db.Database.Open;
   Database_Open := True;

   Athena.Server.Load;

   declare
      UI : Athena.UI.Athena_User_Interface'Class :=
             Athena.UI.Launch.Get_UI
               (Athena.Handles.Empire.Get
                  (Athena.Db.Empire.First_Reference_By_Top_Record
                     (Athena.Db.R_Empire)));
   begin
      UI.Start;
   end;

   Athena.Server.Save;

   Athena.Db.Database.Close;
   Database_Open := False;

exception

   when others =>
      if Database_Open then
         Athena.Db.Database.Close;
      end if;

      Athena.Logs.Flush_Logs (True);
      Athena.Logging.Stop_Logging;
      raise;

end Athena.Driver;
