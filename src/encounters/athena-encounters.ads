private with Athena.Real_Images;

with Athena.Trigonometry;

with Athena.Handles.Encounter;

package Athena.Encounters is

   Max_Ticks        : constant := 1_000;
   Encounter_Radius : constant := 1_000.0;

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

private

   type Encounter_Actor_Reference is new Positive;

   function Image (X : Real) return String
                      renames Athena.Real_Images.Approximate_Image;

end Athena.Encounters;
