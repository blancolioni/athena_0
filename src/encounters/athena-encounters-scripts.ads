with Athena.Encounters.Actors;
with Athena.Encounters.Situation;

package Athena.Encounters.Scripts is

   type Encounter_Script_Interface is interface;

   procedure Update
     (Script    : Encounter_Script_Interface;
      Actor     : Athena.Encounters.Actors.Actor_Type;
      Situation : in out Athena.Encounters.Situation.Situation_Interface'Class)
   is abstract;

   function Beam_Script return Encounter_Script_Interface'Class;

end Athena.Encounters.Scripts;
