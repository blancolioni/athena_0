package body Athena.Encounters.Actors.Weapons is

   type Beam_Actor_Type is
     new Root_Actor_Type with
      record
         null;
      end record;

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
         Mass                => 1.0,
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
         Owner               =>
           Athena.Handles.Empire.Get (Ship.Empire.Reference_Empire),
         Ship                =>
           Athena.Handles.Ship.Get (Ship.Reference_Ship),
         First_Tick          => Tick,
         Last_Tick           => Tick + 4);
   end Create_Beam_Actor;

   --------------------
   -- Current_Sprite --
   --------------------

   overriding function Current_Sprite
     (Beam : Beam_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type
   is
   begin
      return Athena.Encounters.Sprites.Make_Sprite
        (Class    => Beam_Actor,
         Size     => 1.0,
         Location => Beam.Location,
         Heading  => Beam.Heading,
         Target   => Beam.Destination,
         Color    => (1.0, 1.0, 1.0, 1.0),
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

end Athena.Encounters.Actors.Weapons;
