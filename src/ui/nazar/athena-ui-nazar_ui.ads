with Athena.Handles.Empire;
with Athena.Handles.Encounter;

private package Athena.UI.Nazar_UI is

   function Get_UI
     (Empire : Athena.Handles.Empire.Empire_Class)
      return Athena_User_Interface'Class;

   function Get_Encounter_UI
     (Encounter : Athena.Handles.Encounter.Encounter_Class)
      return Athena_User_Interface'Class;

end Athena.UI.Nazar_UI;
