with Athena.Encounters.Actors;
with Athena.Encounters.Frames;

package Athena.Encounters.Execution is

   type Manager_Interface is interface;

   procedure Add_Actor
     (Manager : in out Manager_Interface;
      Actor   : Athena.Encounters.Actors.Actor_Type)
   is abstract;

   procedure Add_Frame
     (Manager : in out Manager_Interface;
      Frame   : Athena.Encounters.Frames.Encounter_Frame)
   is abstract;

   procedure Run_Encounter
     (Encounter : Athena.Handles.Encounter.Encounter_Class;
      Manager   : in out Manager_Interface'Class);

end Athena.Encounters.Execution;
