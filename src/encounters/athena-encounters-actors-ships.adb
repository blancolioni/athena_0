with Nazar.Colors;

with Athena.Ships;

with Athena.Handles.Ship_Component;

package body Athena.Encounters.Actors.Ships is

   type Ship_Actor_Type is
     new Root_Actor_Type with
      record
         Max_Shield     : Non_Negative_Real;
         Current_Shield : Non_Negative_Real;
      end record;

   overriding function Current_Sprite
     (Ship : Ship_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type;

   overriding function Image
     (Ship : Ship_Actor_Type)
      return String;

   function To_Nazar_Color
     (Value : Natural)
      return Nazar.Colors.Nazar_Color
   is (Nazar.Colors.Nazar_Color'
         (Red   =>
             Nazar.Nazar_Unit_Float (Real (Value / 65536 mod 256) / 255.0),
          Green =>
             Nazar.Nazar_Unit_Float (Real (Value / 256 mod 256) / 255.0),
          Blue  =>
             Nazar.Nazar_Unit_Float (Real (Value mod 256) / 255.0),
          Alpha => 1.0));

   ------------------
   -- Create_Actor --
   ------------------

   function Create_Actor
     (Index : Encounter_Actor_Reference;
      Ship  : Athena.Handles.Ship.Ship_Class;
      X, Y    : Real;
      Heading : Athena.Trigonometry.Angle)
      return Actor_Type
   is
      function Initial_Shield return Non_Negative_Real;

      function Initial_Shield return Non_Negative_Real is
         use Athena.Handles.Ship_Component;
         Shield : constant Ship_Component_Class :=
                    Athena.Ships.Get_Shield (Ship);
      begin
         if Shield.Has_Element then
            return Shield.Design_Component.Mass * Shield.Condition
              * Shield.Tec_Level;
         else
            return 0.0;
         end if;
      end Initial_Shield;

      Shield : constant Non_Negative_Real := Initial_Shield;

   begin
      return new Ship_Actor_Type'
        (Index               => Index,
         Class               => Ship_Actor,
         Mass                => Athena.Ships.Mass (Ship),
         Location            => (X, Y),
         Heading             => Heading,
         Destination         => (X, Y),
         Have_Destination    => False,
         Target_Heading      => Heading,
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
         Start_Tick          => 0,
         Max_Shield          => Shield,
         Current_Shield      => Shield);
   end Create_Actor;

   --------------------
   -- Current_Sprite --
   --------------------

   overriding function Current_Sprite
     (Ship : Ship_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type
   is
   begin
      return Athena.Encounters.Sprites.Make_Sprite
        (Class    => Ship_Actor,
         Size     => Ship.Mass,
         Location => Ship.Location,
         Heading  => Ship.Heading,
         Target   => Ship.Location,
         Color    => To_Nazar_Color (Ship.Owner.Rgb),
         Shield   =>
           (if Ship.Max_Shield > 0.0
            then Ship.Current_Shield / Ship.Max_Shield
            else 0.0));
   end Current_Sprite;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Ship : Ship_Actor_Type)
      return String
   is
   begin
      return Ship.Ship.Identifier
        & " " & Ship.Ship.Name
        & ": location (" & Image (Ship.Location.X)
        & "," & Image (Ship.Location.Y)
        & "); heading "
        & Image (Athena.Trigonometry.To_Degrees (Ship.Heading))
        & "; speed " & Image (Ship.Speed)
        & "/" & Image (Athena.Ships.Speed (Ship.Ship))
        & (if Ship.Have_Destination
           then "; destination (" & Image (Ship.Destination.X)
           & "," & Image (Ship.Destination.Y) & ")"
           else "");
   end Image;

end Athena.Encounters.Actors.Ships;
