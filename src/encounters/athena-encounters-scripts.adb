package body Athena.Encounters.Scripts is

   type Beam_Script_Record is new Encounter_Script_Interface with null record;

   overriding procedure Update
     (Script    : Beam_Script_Record;
      Actor     : Athena.Encounters.Actors.Actor_Type;
      Situation : in out Athena.Encounters.Situation.Situation_Interface'Class)
   is null;

   -----------------
   -- Beam_Script --
   -----------------

   function Beam_Script return Encounter_Script_Interface'Class is
   begin
      return Script : Beam_Script_Record;
   end Beam_Script;

end Athena.Encounters.Scripts;
