with Athena.Encounters.Scripts;

package Athena.Ships.Scripts is

   function Get_Script
     (Name : String)
      return Athena.Encounters.Scripts.Encounter_Script_Interface'Class;

   procedure Register
     (Script_Name : String;
      Script      : Athena.Encounters.Scripts
      .Encounter_Script_Interface'Class);

   procedure Load_Standard_Scripts;

end Athena.Ships.Scripts;
