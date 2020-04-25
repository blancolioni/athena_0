with Athena.Handles.Colony;
with Athena.Handles.Star;

package Athena.Stars is

   function Distance
     (From, To : Athena.Handles.Star.Star_Class)
      return Non_Negative_Real;

   function Get_Colony
     (Of_Star : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class;

end Athena.Stars;
