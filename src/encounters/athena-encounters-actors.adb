with Athena.Elementary_Functions;
--  with Athena.Logging;

with Athena.Encounters.Actors.Ships;
with Athena.Encounters.Actors.Weapons;

with Athena.Ships;

package body Athena.Encounters.Actors is

   -----------------------
   -- Clear_Destination --
   -----------------------

   procedure Clear_Destination
     (Actor  : in out Root_Actor_Type)
   is
   begin
      Actor.Have_Destination := False;
      Actor.Have_Target_Heading := False;
      Actor.Is_Following := False;
   end Clear_Destination;

   -----------------------
   -- Create_Beam_Actor --
   -----------------------

   function Create_Beam_Actor
     (Index   : Encounter_Actor_Reference;
      Tick    : Encounter_Tick;
      Ship    : Athena.Handles.Ship.Ship_Class;
      Source  : Encounter_Point;
      Target  : Encounter_Point)
      return Actor_Type
   is
   begin
      return Athena.Encounters.Actors.Weapons.Create_Beam_Actor
        (Index, Tick, Ship, Source, Target);
   end Create_Beam_Actor;

   -----------------------
   -- Create_Ship_Actor --
   -----------------------

   function Create_Ship_Actor
     (Index   : Encounter_Actor_Reference;
      Tick    : Encounter_Tick;
      Ship    : Athena.Handles.Ship.Ship_Class;
      X, Y    : Real;
      Heading : Athena.Trigonometry.Angle)
      return Actor_Type
   is
   begin
      return Athena.Encounters.Actors.Ships.Create_Actor
        (Index   => Index,
         Tick    => Tick,
         Ship    => Ship,
         X       => X,
         Y       => Y,
         Heading => Heading);
   end Create_Ship_Actor;

   -----------------------
   -- Current_Situation --
   -----------------------

   function Current_Situation
     (Actor     : Root_Actor_Type;
      Situation : Athena.Encounters.Situation.Situation_Interface'Class)
      return Athena.Encounters.Situation.Situation_Actor
   is
      use type Athena.Trigonometry.Angle;
   begin
      return Athena.Encounters.Situation.Situation_Actor'
        (Index   => Actor.Index,
         Class   => Actor.Class,
         Dead    => Actor.Dead,
         Mass    => Actor.Mass,
         Size    => Actor.Size,
         DX      => Actor.Location.X - Situation.Origin.X,
         DY      => Actor.Location.Y - Situation.Origin.Y,
         Heading => Actor.Heading - Situation.Heading,
         Speed   => Actor.Speed);
   end Current_Situation;

   -------------------
   -- Destroy_Actor --
   -------------------

   procedure Destroy_Actor
     (Actor : in out Root_Actor_Type;
      Tick  : Encounter_Tick)
   is
   begin
      Actor.Last_Tick := Tick;
   end Destroy_Actor;

   ------------
   -- Follow --
   ------------

   procedure Follow
     (Actor     : in out Root_Actor_Type;
      Target    : Athena.Encounters.Situation.Situation_Actor;
      Bearing   : Athena.Trigonometry.Angle;
      Min_Range : Non_Negative_Real)
   is
   begin
      Actor.Is_Following := True;
      Actor.Follow := Target.Index;
      Actor.Follow_Bearing := Bearing;
      Actor.Follow_Range := Min_Range;
   end Follow;

   ---------
   -- Hit --
   ---------

   procedure Hit
     (Actor  : in out Root_Actor_Type;
      Damage : Non_Negative_Real)
   is
   begin
      Actor.Hits.Append
        (Hit_Record'
           (Damage => Damage));
   end Hit;

   -----------
   -- Owner --
   -----------

   function Owner
     (Actor : Root_Actor_Type)
      return Athena.Handles.Empire.Empire_Class
   is
   begin
      return Actor.Owner;
   end Owner;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Actor  : in out Root_Actor_Type;
      DX, DY : Real)
   is
   begin
      Actor.Destination := (Actor.Location.X + DX, Actor.Location.Y + DY);
      Actor.Have_Destination := True;
   end Set_Destination;

   ------------
   -- Update --
   ------------

   procedure Update
     (Actor     : in out Root_Actor_Type)
   is
      use Athena.Trigonometry;
      Speed     : constant Non_Negative_Real :=
                    Athena.Ships.Speed (Actor.Ship);
   begin

      for Hit of Actor.Hits loop
         Root_Actor_Type'Class (Actor).Apply_Hit (Hit);
      end loop;

      Actor.Hits.Clear;

      if not Actor.Dead
        and then Speed > 0.0
        and then Actor.Have_Destination
      then
         declare
            DX        : constant Real :=
                          Actor.Destination.X - Actor.Location.X;
            DY        : constant Real :=
                          Actor.Destination.Y - Actor.Location.Y;
            Distance  : constant Non_Negative_Real :=
                          Athena.Elementary_Functions.Sqrt
                            (DX ** 2 + DY ** 2);
            Target_Heading : constant Athena.Trigonometry.Angle :=
                               Athena.Trigonometry.Arctan (DY, DX);
            Bearing   : constant Athena.Trigonometry.Angle :=
                            Target_Heading - Actor.Heading;
            Turn      : constant Non_Negative_Real := Speed / 5.0;
            Accel     : constant Non_Negative_Real := Speed / 10.0;
            Stop_Time : constant Non_Negative_Real :=
                          (if Actor.Speed = 0.0
                           then 0.0
                           else Actor.Speed / Accel);
            Stop_Dist : constant Non_Negative_Real :=
                          Actor.Speed * Stop_Time
                            + 0.5 * Accel * Stop_Time ** 2;
         begin
            if Bearing /= From_Degrees (0.0) then

--                 Athena.Logging.Log
--                   (Root_Actor_Type'Class (Actor).Image & "; bearing "
--                    & Image (To_Degrees (Bearing))
--                    & "; turn " & Image (Turn));

               if abs Bearing < From_Degrees (Turn) then
                  Actor.Heading := Target_Heading;
               else
                  Actor.Heading := Actor.Heading
                    + Bearing *
                    (Turn / To_Degrees (abs Bearing));
               end if;
            end if;

            if Stop_Dist > Distance then
               Actor.Speed := Real'Max (Actor.Speed - Accel, 0.0);
            else
               Actor.Speed := Actor.Speed
                 + (1.0 - Actor.Speed / Speed) * Accel;
            end if;
         end;

      elsif Actor.Speed > 0.0 then
         Actor.Speed := Actor.Speed * 0.95;
      end if;

      Actor.Location.X := Actor.Location.X
        + Actor.Speed * Cos (Actor.Heading);
      Actor.Location.Y := Actor.Location.Y
        + Actor.Speed * Sin (Actor.Heading);

   end Update;

   -------------------------------
   -- Update_Follow_Destination --
   -------------------------------

   procedure Update_Follow_Destination
     (Actor     : in out Root_Actor_Type;
      Following : Athena.Encounters.Situation.Situation_Actor)
   is
      use Athena.Trigonometry;
   begin
      Actor.Set_Destination
        (Following.DX + Actor.Follow_Range * Cos (Actor.Follow_Bearing),
         Following.DY + Actor.Follow_Range * Sin (Actor.Follow_Bearing));
   end Update_Follow_Destination;

end Athena.Encounters.Actors;
