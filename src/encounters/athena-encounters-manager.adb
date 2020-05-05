with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Streams.Stream_IO;

with WL.String_Sets;

with Athena.Options;
with Athena.Paths;

with Athena.Identifiers;

with Athena.Turns;

with Athena.Handles.Antagonist;
with Athena.Handles.Participant;

package body Athena.Encounters.Manager is

   function Load_Frames
     (Path : String)
      return Frame_Vectors.Vector;

   procedure Save_Frames
     (Frames : Frame_Vectors.Vector;
      Path   : String);

   function Encounter_Path
     (Encounter : Athena.Handles.Encounter.Encounter_Class)
      return String;

   ---------------
   -- Add_Actor --
   ---------------

   overriding procedure Add_Actor
     (Manager : in out Encounter_Manager_Type;
      Actor   : Athena.Encounters.Actors.Actor_Type)
   is
   begin
      Manager.Actors.Append (Actor);
   end Add_Actor;

   ---------------
   -- Add_Frame --
   ---------------

   overriding procedure Add_Frame
     (Manager : in out Encounter_Manager_Type;
      Frame   : Athena.Encounters.Frames.Encounter_Frame)
   is
   begin
      Manager.Frames.Append (Frame);
   end Add_Frame;

   --------------------
   -- Encounter_Path --
   --------------------

   function Encounter_Path
     (Encounter : Athena.Handles.Encounter.Encounter_Class)
      return String
   is
      Games_Path   : constant String :=
                     Athena.Paths.Config_File ("games");
      Game_Id_Path : constant String :=
                       Games_Path & "/" & Athena.Options.Game_Id;
      File_Name    : constant String :=
                       Ada.Characters.Handling.To_Lower
                         (Encounter.Identifier)
                       & ".encounter";

   begin
      if not Ada.Directories.Exists (Games_Path) then
         Ada.Directories.Create_Directory (Games_Path);
      end if;
      if not Ada.Directories.Exists (Game_Id_Path) then
         Ada.Directories.Create_Directory (Game_Id_Path);
      end if;
      return Game_Id_Path & "/" & File_Name;
   end Encounter_Path;

   -------------------
   -- Iterate_Frame --
   -------------------

   procedure Iterate_Frame
     (Manager : Encounter_Manager_Type'Class;
      Tick    : Encounter_Tick;
      Process : not null access procedure
        (Sprite : Athena.Encounters.Sprites.Sprite_Type))
   is
   begin
      Athena.Encounters.Frames.Iterate_Frame
        (Manager.Frames (Tick), Process);
   end Iterate_Frame;

   --------------------
   -- Load_Encounter --
   --------------------

   function Load_Encounter
     (Encounter : Athena.Handles.Encounter.Encounter_Class)
      return Encounter_Manager_Type
   is
   begin
      return Manager : constant Encounter_Manager_Type :=
        Encounter_Manager_Type'
          (Encounter =>
             Athena.Handles.Encounter.Get
               (Encounter.Reference_Encounter),
           Frames    => Load_Frames (Encounter_Path (Encounter)),
           Actors    => <>);
   end Load_Encounter;

   -----------------
   -- Load_Frames --
   -----------------

   function Load_Frames
     (Path : String)
      return Frame_Vectors.Vector
   is
      use Ada.Streams.Stream_IO;
      File   : File_Type;
   begin
      return Frames : Frame_Vectors.Vector do
         Open (File, In_File, Path);
         Frame_Vectors.Vector'Read (Stream (File), Frames);
         Close (File);
      end return;
   end Load_Frames;

   -----------------------
   -- Resolve_Encounter --
   -----------------------

   procedure Resolve_Encounter
     (Star  : Athena.Handles.Star.Star_Handle;
      Ships : Athena.Ships.Ship_Lists.List)
   is
      Encounter : constant Athena.Handles.Encounter.Encounter_Handle :=
                    Athena.Handles.Encounter.Create
                      (Identifier => Athena.Identifiers.Next_Identifier,
                       Turn       => Athena.Turns.Current_Turn,
                       Star       => Star);
      Manager : Encounter_Manager_Type :=
                  Encounter_Manager_Type'
                    (Encounter => Encounter,
                     Frames    => <>,
                     Actors    => <>);
      Antagonists : WL.String_Sets.Set;

   begin
      for Ship of Ships loop
         Athena.Handles.Participant.Create
           (Encounter => Encounter,
            Ship      => Ship);
         if not Antagonists.Contains (Ship.Empire.Identifier) then
            Athena.Handles.Antagonist.Create
              (Encounter => Encounter,
               Empire    => Ship.Empire);
            Antagonists.Include (Ship.Empire.Identifier);
         end if;
      end loop;

      Athena.Encounters.Execution.Run_Encounter
        (Encounter => Encounter,
         Manager   => Manager);

      Save_Frames (Manager.Frames, Encounter_Path (Encounter));

   end Resolve_Encounter;

   -----------------
   -- Save_Frames --
   -----------------

   procedure Save_Frames
     (Frames : Frame_Vectors.Vector;
      Path   : String)
   is
      use Ada.Streams.Stream_IO;
      File : File_Type;
   begin
      Create (File, Out_File, Path);
      Frame_Vectors.Vector'Write (Stream (File), Frames);
      Close (File);
   end Save_Frames;

end Athena.Encounters.Manager;
