with Athena.Handles.Ship_Component;
with Athena.Handles.Star;

package Athena.Encounters.Situation is

   type Situation_Actor is
      record
         Index   : Encounter_Actor_Reference;
         Class   : Encounter_Actor_Class;
         Dead    : Boolean;
         Mass    : Non_Negative_Real;
         Size    : Non_Negative_Real;
         DX, DY  : Real;
         Heading : Athena.Trigonometry.Angle;
         Speed   : Real;
      end record;

   type Situation_Interface is interface;

   function Star
     (Situation : Situation_Interface)
      return Athena.Handles.Star.Star_Class
      is abstract;

   function Origin
     (Situation : Situation_Interface)
      return Encounter_Point
      is abstract;

   function Current_Tick
     (Situation : Situation_Interface)
      return Encounter_Tick
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
      Target    : Encounter_Actor_Reference)
   is abstract;

   function Get_Range
     (Situation : Situation_Interface'Class;
      To_Actor  : Encounter_Actor_Reference)
      return Non_Negative_Real;

   function Hostile_Centre
     (Situation : Situation_Interface'Class)
      return Encounter_Point;

   function Allied_Centre
     (Situation : Situation_Interface'Class)
      return Encounter_Point;

end Athena.Encounters.Situation;
