with Athena.Ships;

with Athena.Handles.Encounter;
with Athena.Handles.Star;

package Athena.Encounters is

   function Create
     (Star  : Athena.Handles.Star.Star_Class;
      Ships : Athena.Ships.Ship_Lists.List)
      return Athena.Handles.Encounter.Encounter_Class;

   procedure Execute
     (Encounter : Athena.Handles.Encounter.Encounter_Class);

   function Find
     (Star_Name   : String;
      Turn_Number : Positive)
      return Athena.Handles.Encounter.Encounter_Class;

end Athena.Encounters;
