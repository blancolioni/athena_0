with Nazar;

package body Athena.Encounters.Actors.Weapons is

   type Beam_Actor_Type is
     new Root_Actor_Type with
      record
         Progress : Unit_Real;
      end record;

   overriding procedure Update
     (Beam : in out Beam_Actor_Type);

   overriding function Image
     (Beam : Beam_Actor_Type)
      return String;

   overriding function Current_Sprite
     (Beam : Beam_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type;

   -----------------------
   -- Create_Beam_Actor --
   -----------------------

   function Create_Beam_Actor
     (Index : Encounter_Actor_Reference;
      Tick  : Encounter_Tick;
      Ship   : Athena.Handles.Ship.Ship_Class;
      Source  : Encounter_Point;
      Target  : Encounter_Point)
      return Actor_Type
   is
   begin
      return new Beam_Actor_Type'
        (Index               => Index,
         Class               => Beam_Actor,
         Dead                => False,
         Mass                => 1.0,
         Size                => 1.0,
         Location            => Source,
         Heading             => Athena.Trigonometry.From_Degrees (0.0),
         Destination         => Target,
         Have_Destination    => True,
         Target_Heading      => Athena.Trigonometry.From_Degrees (0.0),
         Have_Target_Heading => False,
         Follow              => 1,
         Follow_Range        => 0.0,
         Follow_Bearing      => Athena.Trigonometry.From_Degrees (0.0),
         Is_Following        => False,
         Speed               => 0.0,
         Speed_Limit         => Real'Last,
         Owner               =>
           Athena.Handles.Empire.Get (Ship.Empire.Reference_Empire),
         Ship                =>
           Athena.Handles.Ship.Get (Ship.Reference_Ship),
         First_Tick          => Tick,
         Last_Tick           => Tick + 3,
         Hits                => <>,
         Progress            => 0.0,
         Jumping             => False,
         Jump_Tick           => Encounter_Tick'First,
         Jump_Destination    => Athena.Handles.Star.Empty_Handle);
   end Create_Beam_Actor;

   --------------------
   -- Current_Sprite --
   --------------------

   overriding function Current_Sprite
     (Beam : Beam_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type
   is
      DX : constant Real :=
             Beam.Destination.X - Beam.Location.X;
      DY : constant Real :=
             Beam.Destination.Y - Beam.Location.Y;
      X1 : constant Real :=
             Beam.Location.X + DX * 0.04;
      Y1 : constant Real :=
             Beam.Location.Y + DY * 0.04;
      X2 : constant Real :=
             Beam.Location.X + DX * 0.98;
      Y2 : constant Real :=
             Beam.Location.Y + DY * 0.98;

   begin
      return Athena.Encounters.Sprites.Make_Sprite
        (Class    => Beam_Actor,
         Size     => 1.0,
         Location => (X1, Y1),
         Heading  => Beam.Heading,
         Target   => (X2, Y2),
         Color    => (0.0, Nazar.Nazar_Unit_Float (1.0 - Beam.Progress),
                      0.0, 1.0),
         Shield   => 0.0);
   end Current_Sprite;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Beam : Beam_Actor_Type)
      return String
   is
   begin
      return Beam.Ship.Identifier & " " & Beam.Ship.Name
        & " fires beam weapon to "
        & "(" & Image (Beam.Destination.X)
        & "," & Image (Beam.Destination.Y)
        & ")";
   end Image;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Beam : in out Beam_Actor_Type)
   is
   begin
      Root_Actor_Type (Beam).Update;
      Beam.Progress := Beam.Progress + 0.2;
   end Update;

end Athena.Encounters.Actors.Weapons;
