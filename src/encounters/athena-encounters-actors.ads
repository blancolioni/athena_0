with Athena.Trigonometry;

with Athena.Encounters.Situation;
with Athena.Encounters.Sprites;

with Athena.Handles.Empire;
with Athena.Handles.Ship;

package Athena.Encounters.Actors is

   type Root_Actor_Type is abstract tagged private;

   type Actor_Type is access all Root_Actor_Type'Class;

   function Image (Actor : Root_Actor_Type) return String is abstract;

   function Current_Sprite
     (Actor : Root_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type
      is abstract;

   function Current_Situation
     (Actor     : Root_Actor_Type;
      Situation : Athena.Encounters.Situation.Situation_Interface'Class)
      return Athena.Encounters.Situation.Situation_Actor;

   function Location
     (Actor : Root_Actor_Type)
      return Encounter_Point;

   function Heading
     (Actor : Root_Actor_Type)
      return Athena.Trigonometry.Angle;

   function Owner
     (Actor : Root_Actor_Type)
     return Athena.Handles.Empire.Empire_Class;

   procedure Update
     (Actor     : in out Root_Actor_Type);

   procedure Set_Destination
     (Actor  : in out Root_Actor_Type;
      DX, DY : Real);

   function Is_Following
     (Actor : Root_Actor_Type)
      return Boolean;

   function Following_Actor
     (Follower : Root_Actor_Type)
      return Encounter_Actor_Reference
     with Pre => Is_Following (Follower);

   procedure Follow
     (Actor     : in out Root_Actor_Type;
      Target    : Athena.Encounters.Situation.Situation_Actor;
      Bearing   : Athena.Trigonometry.Angle;
      Min_Range : Non_Negative_Real);

   procedure Update_Follow_Destination
     (Actor     : in out Root_Actor_Type;
      Following : Athena.Encounters.Situation.Situation_Actor);

   function Create_Ship_Actor
     (Index   : Encounter_Actor_Reference;
      Ship    : Athena.Handles.Ship.Ship_Class;
      X, Y    : Real;
      Heading : Athena.Trigonometry.Angle)
      return Actor_Type;

private

   type Root_Actor_Type is abstract tagged
      record
         Index               : Encounter_Actor_Reference;
         Class               : Encounter_Actor_Class;
         Mass                : Non_Negative_Real;
         Location            : Encounter_Point;
         Heading             : Athena.Trigonometry.Angle;
         Have_Destination    : Boolean;
         Destination         : Encounter_Point;
         Target_Heading      : Athena.Trigonometry.Angle;
         Have_Target_Heading : Boolean;
         Follow              : Encounter_Actor_Reference;
         Follow_Range        : Non_Negative_Real;
         Follow_Bearing      : Athena.Trigonometry.Angle;
         Is_Following        : Boolean;
         Speed               : Non_Negative_Real;
         Owner               : Athena.Handles.Empire.Empire_Handle;
         Ship                : Athena.Handles.Ship.Ship_Handle;
         Start_Tick          : Encounter_Tick;

--           case Class is
--              when Ship_Actor =>
--                 Target_Speed   : Non_Negative_Real;
--                 Target_Heading : Athena.Trigonometry.Angle;
--
--              when Missile_Actor =>
--           Missile_Launcher : Handles.Ship_Component.Ship_Component_Handle;
--                 Missile_Target   : Actor_Type;
--
--              when Fighter_Actor =>
--                 Fighter_Target   : Actor_Type;
--
--              when Beam_Actor =>
--                 Beam_Target    : Actor_Type;
--                 Beam_Hit       : Boolean;
--
--              when Kinetic_Actor =>
--                 Kinetic_Target : Actor_Type;
--                 Kinetic_Hit    : Boolean;
--
--           end case;
      end record;

   function Location
     (Actor : Root_Actor_Type)
      return Encounter_Point
   is (Actor.Location);

   function Heading
     (Actor : Root_Actor_Type)
      return Athena.Trigonometry.Angle
   is (Actor.Heading);

   function Is_Following
     (Actor : Root_Actor_Type)
      return Boolean
   is (Actor.Is_Following);

   function Following_Actor
     (Follower : Root_Actor_Type)
      return Encounter_Actor_Reference
   is (Follower.Follow);

end Athena.Encounters.Actors;
