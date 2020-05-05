with Athena.Handles.Encounter.Selections;
with Athena.Handles.Star.Selections;
with Athena.Handles.Turn.Selections;

package body Athena.Encounters is

   ----------
   -- Find --
   ----------

   function Find
     (Star_Name   : String;
      Turn_Number : Positive)
      return Athena.Handles.Encounter.Encounter_Class
   is
      use type Athena.Handles.Star.Selections.Selection_Condition;
      use type Athena.Handles.Turn.Selections.Selection_Condition;
      Encounter_Star : constant Athena.Handles.Star.Star_Class :=
                         Athena.Handles.Star.Selections.First_Where
                           (Athena.Handles.Star.Selections.Name = Star_Name);
      Encounter_Turn : constant Athena.Handles.Turn.Turn_Class :=
                         Athena.Handles.Turn.Selections.First_Where
                           (Athena.Handles.Turn.Selections.Turn_Number
                            = Turn_Number);
   begin
      if Encounter_Star.Has_Element
        and then Encounter_Turn.Has_Element
      then
         declare
            use Athena.Handles.Encounter.Selections;
         begin
            return First_Where
              (Star = Encounter_Star and Turn = Encounter_Turn);
         end;
      else
         return Athena.Handles.Encounter.Empty_Handle;
      end if;
   end Find;

   ------------
   -- Rotate --
   ------------

   function Rotate
     (Pt     : Encounter_Point;
      Around : Encounter_Point;
      Angle  : Athena.Trigonometry.Angle)
      return Encounter_Point
   is
      Cos : constant Signed_Unit_Real :=
              Athena.Trigonometry.Cos (Angle);
      Sin : constant Signed_Unit_Real :=
              Athena.Trigonometry.Sin (Angle);
      X   : constant Real := Pt.X - Around.X;
      Y   : constant Real := Pt.Y - Around.Y;
   begin
      return Encounter_Point'
        (X => Around.X + X * Cos - Y * Sin,
         Y => Around.Y + X * Sin + Y * Cos);
   end Rotate;

end Athena.Encounters;
