private with Ada.Containers.Vectors;
private with Athena.Encounters.Frames;
private with Athena.Encounters.Actors;

with Athena.Ships;

with Athena.Encounters.Execution;
with Athena.Encounters.Sprites;

with Athena.Handles.Star;

package Athena.Encounters.Manager is

   type Encounter_Manager_Type is
     new Athena.Encounters.Execution.Manager_Interface
   with private;

   procedure Resolve_Encounter
     (Star  : Athena.Handles.Star.Star_Handle;
      Ships : Athena.Ships.Ship_Lists.List);

   function Load_Encounter
     (Encounter : Athena.Handles.Encounter.Encounter_Class)
      return Encounter_Manager_Type;

   procedure Iterate_Frame
     (Manager : Encounter_Manager_Type'Class;
      Tick    : Encounter_Tick;
      Process : not null access
        procedure (Sprite : Athena.Encounters.Sprites.Sprite_Type));

private

   package Frame_Vectors is
     new Ada.Containers.Vectors
       (Encounter_Tick,  Athena.Encounters.Frames.Encounter_Frame,
        Athena.Encounters.Frames."=");

   package Actor_Vectors is
     new Ada.Containers.Vectors
       (Positive, Athena.Encounters.Actors.Actor_Type,
        Athena.Encounters.Actors."=");

   type Encounter_Manager_Type is
     new Athena.Encounters.Execution.Manager_Interface with
      record
         Encounter : Athena.Handles.Encounter.Encounter_Handle;
         Frames    : Frame_Vectors.Vector;
         Actors    : Actor_Vectors.Vector;
      end record;

   overriding procedure Add_Actor
     (Manager : in out Encounter_Manager_Type;
      Actor   : Athena.Encounters.Actors.Actor_Type);

   overriding procedure Add_Frame
     (Manager : in out Encounter_Manager_Type;
      Frame   : Athena.Encounters.Frames.Encounter_Frame);

end Athena.Encounters.Manager;
