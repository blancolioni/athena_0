package Athena.Encounters.Actors.Weapons is

   function Create_Beam_Actor
     (Index   : Encounter_Actor_Reference;
      Tick    : Encounter_Tick;
      Ship    : Athena.Handles.Ship.Ship_Class;
      Source  : Encounter_Point;
      Target  : Encounter_Point)
      return Actor_Type;

end Athena.Encounters.Actors.Weapons;
