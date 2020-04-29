private with Athena.Handles.Empire;
private with Athena.Real_Images;

package Athena.Managers is

   procedure Load_Managers;

   procedure Run_Managers;

private

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Log
     (Manager_Tag : String;
      Empire      : Athena.Handles.Empire.Empire_Class;
      Message     : String);

end Athena.Managers;
