with Athena.Handles.Empire;

package Athena.Treaties is

   function At_War
     (E1, E2 : Athena.Handles.Empire.Empire_Class)
      return Boolean;

   procedure Declare_War
     (E1, E2 : Athena.Handles.Empire.Empire_Class);

end Athena.Treaties;
