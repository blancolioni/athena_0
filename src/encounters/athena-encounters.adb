with Athena.Elementary_Functions;

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

   ----------------
   -- Hit_Chance --
   ----------------

   function Hit_Chance
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target_Size  : Non_Negative_Real;
      Target_Range : Non_Negative_Real)
      return Unit_Real
   is
      Max_Range : constant Non_Negative_Real :=
                    Maximum_Range (Weapon);
      P_Hit     : constant Non_Negative_Real :=
                    (if Target_Range > Max_Range then 0.0
                     else (1.0 + Weapon.Ship.Experience)
                     * Target_Size / 10.0
                     * Weapon.Condition
                     * (1.0 - Target_Range / Max_Range));
   begin
      return Unit_Clamp (P_Hit);
   end Hit_Chance;

   ---------------
   -- Hit_Power --
   ---------------

   function Hit_Power
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target_Range : Non_Negative_Real)
      return Non_Negative_Real
   is
      Mass      : constant Non_Negative_Real :=
                    Weapon.Design_Component.Mass;
      Tec_Level : constant Non_Negative_Real :=
                    Weapon.Tec_Level;
      Condition : constant Unit_Real :=
                    Weapon.Condition;
      Max_Range : constant Non_Negative_Real :=
                    Maximum_Range (Weapon);
      Power     : constant Non_Negative_Real :=
                    (if Target_Range > Max_Range then 0.0
                     else Athena.Elementary_Functions.Sqrt (Mass)
                     * Condition * Tec_Level
                     * (1.0 - Target_Range / Max_Range));
   begin
      return Power;
   end Hit_Power;

   -------------------
   -- Maximum_Range --
   -------------------

   function Maximum_Range
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class)
      return Non_Negative_Real
   is
      Mass      : constant Non_Negative_Real :=
                    Weapon.Design_Component.Mass;
      Tec_Level : constant Non_Negative_Real :=
                    Weapon.Tec_Level;
      Condition : constant Unit_Real :=
                    Weapon.Condition;
   begin
      return 100.0 * Mass * Tec_Level * Condition;
   end Maximum_Range;

   -------------------------
   -- Range_At_Hit_Chance --
   -------------------------

   function Range_At_Hit_Chance
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target_Size  : Non_Negative_Real;
      Hit_Chance   : Unit_Real)
      return Non_Negative_Real
   is
      Max_Range : constant Non_Negative_Real :=
                    Maximum_Range (Weapon);
      Result     : constant Real :=
                     (1.0 - Hit_Chance / (1.0 + Weapon.Ship.Experience)
                      * (10.0 / Target_Size) / Weapon.Condition)
                     * Max_Range;
   begin
      return Clamp (Result, 10.0, 1.0e6);
   end Range_At_Hit_Chance;

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
