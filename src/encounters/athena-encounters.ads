private with Athena.Real_Images;

with Athena.Trigonometry;

with Athena.Handles.Encounter;
with Athena.Handles.Ship_Component;

package Athena.Encounters is

   Max_Ticks        : constant := 2_000;

   type Encounter_Actor_Reference is private;

   type Encounter_Actor_Class is
     (Ship_Actor, Missile_Actor, Fighter_Actor,
      Beam_Actor, Kinetic_Actor);

   type Encounter_Point is
      record
         X, Y : Real;
      end record;

   function Translate
     (Pt   : Encounter_Point;
      X, Y : Real := 0.0)
      return Encounter_Point
   is (Pt.X + X, Pt.Y + Y);

   function Rotate
     (Pt     : Encounter_Point;
      Around : Encounter_Point;
      Angle  : Athena.Trigonometry.Angle)
      return Encounter_Point;

   type Encounter_Tick is new Natural;

   function Find
     (Star_Name   : String;
      Turn_Number : Positive)
      return Athena.Handles.Encounter.Encounter_Class;

   function Maximum_Range
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class)
      return Non_Negative_Real;

   function Hit_Chance
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target_Size  : Non_Negative_Real;
      Target_Range : Non_Negative_Real)
      return Unit_Real;

   function Range_At_Hit_Chance
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target_Size  : Non_Negative_Real;
      Hit_Chance   : Unit_Real)
      return Non_Negative_Real
     with Pre => Weapon.Condition > 0.0;

   function Hit_Power
     (Weapon       : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target_Range : Non_Negative_Real)
      return Non_Negative_Real;

private

   type Encounter_Actor_Reference is new Positive;

   function Image (X : Real) return String
                      renames Athena.Real_Images.Approximate_Image;

end Athena.Encounters;
