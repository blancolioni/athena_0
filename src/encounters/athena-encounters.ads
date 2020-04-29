with Athena.Ships;

with Athena.Handles.Encounter;
with Athena.Handles.Star;

package Athena.Encounters is

   function Create
     (Star  : Athena.Handles.Star.Star_Class;
      Ships : Athena.Ships.Ship_Lists.List)
      return Athena.Handles.Encounter.Encounter_Class;

   procedure Execute
     (E : Athena.Handles.Encounter.Encounter_Class);

end Athena.Encounters;
