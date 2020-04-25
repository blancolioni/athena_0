with Ada.Text_IO;
with Ada.Directories;

with WL.Command_Line;
with WL.Localisation;
with WL.Random.Names;

with Athena.Color;
with Athena.Configure;
with Athena.Options;
with Athena.Paths;
with Athena.Random_Names;

with Athena.Empires.Create;

with Athena.Db.Database;

with Athena.Handles.Star;
with Athena.Db.Star;

with Athena.Handles.Empire;
with Athena.Handles.Star_Distance.Selections;

package body Athena.Server is

   Name_Generator    : WL.Random.Names.Name_Generator;

   ----------------
   -- Add_Empire --
   ----------------

   procedure Add_Empire
     (Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : String)
   is
      use Athena.Handles.Star;
      use Athena.Handles.Star_Distance.Selections;
      Found : Boolean := False;
   begin

      for Star_Rec of
        Athena.Db.Star.Scan_By_Core_Distance
      loop
         declare
            Star : constant Star_Handle :=
                     Get (Star_Rec.Get_Star_Reference);
            OK : Boolean := True;
         begin
            if Star.Owner.Has_Element
              or else Star.Space not in 1000 .. 2500
              or else Star.Resource not in 0.5 .. 1.0
              or else Star.Habitability not in 0.5 .. 1.0
            then
               OK := False;
            else
               for Distance of Select_Where (From = Star) loop
                  exit when Distance.Distance > 20.0;
                  if Distance.To.Owner.Has_Element then
                     OK := False;
                     exit;
                  end if;
               end loop;
            end if;

            if OK then
               Athena.Empires.Create.New_Empire
                 (Star      => Star,
                  Name      => Name,
                  Plural    => (if Plural = "" then Name else Plural),
                  Adjective => (if Adjective = "" then Name else Adjective),
                  Capital   => (if Capital = "" then Name else Capital),
                  Color     => Athena.Color.From_String (Color));

               Ada.Text_IO.Put_Line
                 (Name & " founded at " & Star.Name);
               Found := True;
               exit;
            end if;
         end;
      end loop;

      if not Found then
         Ada.Text_IO.Put_Line
           (Name & ": unable to find home world");
      end if;

   end Add_Empire;

   ---------------------
   -- Create_Scenario --
   ---------------------

   procedure Create_Scenario is
      Database_Open : Boolean := False;
      Radius_X      : constant Natural :=
                        Natural'Max
                          (Athena.Options.Galaxy_Radius_X,
                           Athena.Options.Galaxy_Radius);
      Radius_Y      : constant Natural :=
                        Natural'Max (Athena.Options.Galaxy_Radius_Y,
                                     Athena.Options.Galaxy_Radius);

   begin
      Athena.Db.Database.Create;
      Database_Open := True;

      Athena.Configure.Initialize_Database;

      Athena.Configure.Create_Galaxy
        (Radius_X       => Non_Negative_Real (Radius_X),
         Radius_Y       => Non_Negative_Real (Radius_Y),
         Star_Count     => Athena.Options.Star_Count,
         Name_Generator => Name_Generator);

      Athena.Db.Database.Close;
      Database_Open := False;

   exception

      when others =>
         if Database_Open then
            Athena.Db.Database.Close;
         end if;
         raise;

   end Create_Scenario;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin

      if not Ada.Directories.Exists (".athena-options") then
         Ada.Directories.Copy_File
           (Source_Name => Athena.Paths.Config_File ("default-options.txt"),
            Target_Name => ".athena-options");
      end if;

      WL.Command_Line.Load_Defaults (".athena-options");

      WL.Localisation.Read_Localisation
        (Athena.Paths.Config_File
           ("localisation/" & Athena.Options.Language & ".txt"));

      WL.Random.Names.Load_Lexicon
        (Name_Generator,
         Athena.Paths.Config_File ("totro-vowels.txt"),
         Athena.Paths.Config_File ("totro-consonants.txt"));

      if Athena.Options.Randomise then
         WL.Random.Randomise;
      end if;

      Athena.Random_Names.Load_Names;

   end Initialize;

end Athena.Server;
