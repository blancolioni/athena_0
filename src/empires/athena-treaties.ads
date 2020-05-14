with Athena.Handles.Empire;
with Athena.Handles.War;

package Athena.Treaties is

   function At_War
     (E1, E2 : Athena.Handles.Empire.Empire_Class)
      return Boolean;

   function Get_War
     (E1, E2 : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.War.War_Class;

   procedure Declare_War
     (E1, E2 : Athena.Handles.Empire.Empire_Class);

end Athena.Treaties;
