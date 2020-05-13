private with Ada.Containers.Doubly_Linked_Lists;

with Athena.Trigonometry;

with Athena.Encounters.Situation;
with Athena.Encounters.Sprites;

with Athena.Handles.Empire;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Component;
with Athena.Handles.Star;

package Athena.Encounters.Actors is

   type Root_Actor_Type is abstract tagged private;

   type Actor_Type is access all Root_Actor_Type'Class;

   function Image (Actor : Root_Actor_Type) return String is abstract;

   function Current_Sprite
     (Actor : Root_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type
      is abstract;

   procedure Iterate_Beam_Weapons
     (Actor : Root_Actor_Type;
      Process : not null access
        procedure (Component : Athena.Handles.Ship_Component
                   .Ship_Component_Class;
                   Charge    : Unit_Real))
   is null;

   procedure Hit
     (Actor  : in out Root_Actor_Type;
      Damage : Non_Negative_Real);

   procedure Weapon_Fired
     (Actor : in out Root_Actor_Type;
      Weapon : Athena.Handles.Ship_Component.Ship_Component_Class)
   is null;

   function Is_Active
     (Actor : Root_Actor_Type;
      Tick  : Encounter_Tick)
      return Boolean;

   function Is_Dead
     (Actor : Root_Actor_Type)
      return Boolean;

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

   function Size
     (Actor : Root_Actor_Type)
      return Non_Negative_Real;

   function Owner
     (Actor : Root_Actor_Type)
     return Athena.Handles.Empire.Empire_Class;

   procedure Update
     (Actor     : in out Root_Actor_Type);

   procedure Set_Speed_Limit
     (Actor : in out Root_Actor_Type'Class;
      Speed : Non_Negative_Real);

   procedure Set_Destination
     (Actor  : in out Root_Actor_Type;
      DX, DY : Real);

   procedure Clear_Destination
     (Actor  : in out Root_Actor_Type);

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

   function Is_Jumping
     (Actor : Root_Actor_Type)
      return Boolean;

   function Jump_Tick
     (Actor : Root_Actor_Type)
      return Encounter_Tick;

   function Jump_Destination
     (Actor : Root_Actor_Type)
      return Athena.Handles.Star.Star_Class;

   procedure Start_Jump
     (Actor       : in out Root_Actor_Type;
      Jump_Tick   : Encounter_Tick;
      Destination : Athena.Handles.Star.Star_Class);

   procedure Execute_Jump
     (Actor : in out Root_Actor_Type)
   is null;

   procedure Destroy_Actor
     (Actor : in out Root_Actor_Type;
      Tick  : Encounter_Tick);

   function Create_Ship_Actor
     (Index   : Encounter_Actor_Reference;
      Tick    : Encounter_Tick;
      Ship    : Athena.Handles.Ship.Ship_Class;
      X, Y    : Real;
      Heading : Athena.Trigonometry.Angle)
      return Actor_Type;

   function Create_Beam_Actor
     (Index   : Encounter_Actor_Reference;
      Tick    : Encounter_Tick;
      Ship    : Athena.Handles.Ship.Ship_Class;
      Source  : Encounter_Point;
      Target  : Encounter_Point)
      return Actor_Type;

private

   type Hit_Record is
      record
         Damage : Non_Negative_Real;
      end record;

   package Hit_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Hit_Record);

   type Root_Actor_Type is abstract tagged
      record
         Index               : Encounter_Actor_Reference;
         Class               : Encounter_Actor_Class;
         Dead                : Boolean;
         Mass                : Non_Negative_Real;
         Size                : Non_Negative_Real;
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
         Speed_Limit         : Non_Negative_Real;
         Speed               : Non_Negative_Real;
         Owner               : Athena.Handles.Empire.Empire_Handle;
         Ship                : Athena.Handles.Ship.Ship_Handle;
         First_Tick          : Encounter_Tick;
         Last_Tick           : Encounter_Tick;
         Hits                : Hit_Lists.List;
         Jumping             : Boolean := False;
         Jump_Tick           : Encounter_Tick;
         Jump_Destination    : Athena.Handles.Star.Star_Handle;
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

   procedure Apply_Hit
     (Actor : in out Root_Actor_Type;
      Hit   : Hit_Record)
   is null;

   function Is_Active
     (Actor : Root_Actor_Type;
      Tick  : Encounter_Tick)
      return Boolean
   is (Tick >= Actor.First_Tick and then Tick <= Actor.Last_Tick);

   function Is_Dead
     (Actor : Root_Actor_Type)
      return Boolean
   is (Actor.Dead);

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

   function Size
     (Actor : Root_Actor_Type)
      return Non_Negative_Real
   is (Actor.Size);

   function Is_Jumping
     (Actor : Root_Actor_Type)
      return Boolean
   is (Actor.Jumping);

   function Jump_Tick
     (Actor : Root_Actor_Type)
      return Encounter_Tick
   is (Actor.Jump_Tick);

   function Jump_Destination
     (Actor : Root_Actor_Type)
      return Athena.Handles.Star.Star_Class
   is (Actor.Jump_Destination);

end Athena.Encounters.Actors;
