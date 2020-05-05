with Athena.Handles.Ship_Component;

package Athena.Encounters.Situation is

   type Situation_Actor is
      record
         Index   : Encounter_Actor_Reference;
         Class   : Encounter_Actor_Class;
         Mass    : Non_Negative_Real;
         DX, DY  : Real;
         Heading : Athena.Trigonometry.Angle;
         Speed   : Real;
      end record;

   type Situation_Interface is interface;

   function Origin
     (Situation : Situation_Interface)
      return Encounter_Point
      is abstract;

   function Heading
     (Situation : Situation_Interface)
      return Athena.Trigonometry.Angle
      is abstract;

   function Get
     (Situation : Situation_Interface;
      Index     : Encounter_Actor_Reference)
      return Situation_Actor
   is abstract;

   procedure Iterate_Hostiles
     (Situation : Situation_Interface;
      Process   : not null access
        procedure (Hostile : Situation_Actor))
   is abstract;

   procedure Iterate_Allies
     (Situation : Situation_Interface;
      Process   : not null access
        procedure (Hostile : Situation_Actor))
   is abstract;

   procedure Fire_Weapon
     (Situation : in out Situation_Interface;
      Weapon    : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target    : Positive)
   is abstract;

   function Hostile_Centre
     (Situation : Situation_Interface'Class)
      return Encounter_Point;

   function Allied_Centre
     (Situation : Situation_Interface'Class)
      return Encounter_Point;

end Athena.Encounters.Situation;