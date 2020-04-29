with Athena.Handles.Colony;
with Athena.Handles.Star;

package Athena.Stars is

   function Distance
     (From, To : Athena.Handles.Star.Star_Class)
      return Non_Negative_Real;

   function Get_Colony
     (Of_Star : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class;

   procedure Set_Colony
     (Star   : Athena.Handles.Star.Star_Class;
      Colony : Athena.Handles.Colony.Colony_Class);

   procedure Iterate_Nearest
     (To_Star   : Athena.Handles.Star.Star_Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Star : Athena.Handles.Star.Star_Class;
                   Distance : Non_Negative_Real));

   procedure Load_Stars;

end Athena.Stars;
