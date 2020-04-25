with Athena.Handles.Turn;

package Athena.Turns is

   function Current_Turn return Positive;
   function Current_Turn return Athena.Handles.Turn.Turn_Class;
   function Previous_Turn return Athena.Handles.Turn.Turn_Class;

   function Current_Turn_Image return String;

   procedure Next_Turn;

end Athena.Turns;
