package Athena.Encounters.Actors.Ships is

   function Create_Actor
     (Index   : Encounter_Actor_Reference;
      Tick    : Encounter_Tick;
      Ship    : Athena.Handles.Ship.Ship_Class;
      X, Y    : Real;
      Heading : Athena.Trigonometry.Angle)
      return Actor_Type;

end Athena.Encounters.Actors.Ships;
