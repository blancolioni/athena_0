with Athena.Elementary_Functions;

with Athena.Handles.Colony.Selections;

package body Athena.Stars is

   --------------
   -- Distance --
   --------------

   function Distance
     (From, To : Athena.Handles.Star.Star_Class)
      return Non_Negative_Real
   is
      use Athena.Elementary_Functions;
   begin
      return Sqrt ((From.X - To.X) ** 2 + (From.Y - To.Y) ** 2);
   end Distance;

   ----------------
   -- Get_Colony --
   ----------------

   function Get_Colony
     (Of_Star : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class
   is
      use Athena.Handles.Colony.Selections;
   begin
      return First_Where (Star = Of_Star);
   end Get_Colony;

end Athena.Stars;
