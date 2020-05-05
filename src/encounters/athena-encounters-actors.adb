with Athena.Elementary_Functions;

with Athena.Encounters.Actors.Ships;

with Athena.Ships;

package body Athena.Encounters.Actors is

   -----------------------
   -- Create_Ship_Actor --
   -----------------------

   function Create_Ship_Actor
     (Index   : Encounter_Actor_Reference;
      Ship    : Athena.Handles.Ship.Ship_Class;
      X, Y    : Real;
      Heading : Athena.Trigonometry.Angle)
      return Actor_Type
   is
   begin
      return Athena.Encounters.Actors.Ships.Create_Actor
        (Index   => Index,
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
         Mass    => Actor.Mass,
         DX      => Actor.Location.X - Situation.Origin.X,
         DY      => Actor.Location.Y - Situation.Origin.Y,
         Heading => Actor.Heading - Situation.Heading,
         Speed   => Actor.Speed);
   end Current_Situation;

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
      if Actor.Have_Destination then
         declare
            DX        : constant Real :=
                          Actor.Destination.X - Actor.Location.X;
            DY        : constant Real :=
                          Actor.Destination.Y - Actor.Location.Y;
            Distance  : constant Non_Negative_Real :=
                          Athena.Elementary_Functions.Sqrt
                            (DX ** 2 + DY ** 2);
            Bearing   : constant Athena.Trigonometry.Angle :=
                          Athena.Trigonometry.Arctan
                            (DY, DX)
                            - Actor.Heading;
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

               if abs Bearing < From_Degrees (Turn) then
                  Actor.Heading := Bearing;
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

            Actor.Location.X := Actor.Location.X
              + Actor.Speed * Cos (Actor.Heading);
            Actor.Location.Y := Actor.Location.Y
              + Actor.Speed * Sin (Actor.Heading);

         end;
      end if;
   end Update;

end Athena.Encounters.Actors;
