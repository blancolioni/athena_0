with WL.String_Maps;

with Athena.Ships.Attack;
with Athena.Ships.Escape;

package body Athena.Ships.Scripts is

   package Script_Maps is
     new WL.String_Maps
       (Athena.Encounters.Scripts.Encounter_Script_Interface'Class,
        Athena.Encounters.Scripts."=");

   Script_Map : Script_Maps.Map;

   ----------------
   -- Get_Script --
   ----------------

   function Get_Script
     (Name : String)
      return Athena.Encounters.Scripts.Encounter_Script_Interface'Class
   is
   begin
      if Script_Map.Contains (Name) then
         return Script_Map (Name);
      else
         return Athena.Ships.Escape.Escape_Script;
      end if;
   end Get_Script;

   ---------------------------
   -- Load_Standard_Scripts --
   ---------------------------

   procedure Load_Standard_Scripts is
   begin
      Script_Map.Insert ("escape", Athena.Ships.Escape.Escape_Script);
      Script_Map.Insert ("attack", Athena.Ships.Attack.Attack_Script);
      Script_Map.Insert ("defend", Athena.Ships.Attack.Defend_Script);
   end Load_Standard_Scripts;

   --------------
   -- Register --
   --------------

   procedure Register
     (Script_Name : String;
      Script      : Athena.Encounters.Scripts.Encounter_Script_Interface'Class)
   is
   begin
      Script_Map.Insert (Script_Name, Script);
   end Register;

end Athena.Ships.Scripts;
