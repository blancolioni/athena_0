with Nazar.Colors;

with Athena.Trigonometry;

package Athena.Encounters.Sprites is

   type Sprite_Type is private;

   function Class
     (Sprite : Sprite_Type)
      return Encounter_Actor_Class;

   function Size
     (Sprite : Sprite_Type)
      return Non_Negative_Real;

   function Location
     (Sprite : Sprite_Type)
      return Encounter_Point;

   function Heading
     (Sprite : Sprite_Type)
      return Athena.Trigonometry.Angle;

   function Target
     (Sprite : Sprite_Type)
      return Encounter_Point;

   function Color
     (Sprite : Sprite_Type)
      return Nazar.Colors.Nazar_Color;

   function Shield
     (Sprite : Sprite_Type)
      return Unit_Real;

   function Make_Sprite
     (Class    : Encounter_Actor_Class;
      Size     : Non_Negative_Real;
      Location : Encounter_Point;
      Heading  : Athena.Trigonometry.Angle;
      Target   : Encounter_Point;
      Color    : Nazar.Colors.Nazar_Color;
      Shield   : Unit_Real)
      return Sprite_Type;

private

   type Sprite_Type is
      record
         Class    : Encounter_Actor_Class;
         Size     : Non_Negative_Real;
         Location : Encounter_Point;
         Heading  : Athena.Trigonometry.Angle;
         Target   : Encounter_Point;
         Color    : Nazar.Colors.Nazar_Color;
         Shield   : Unit_Real;
      end record;

   function Class
     (Sprite : Sprite_Type)
      return Encounter_Actor_Class
   is (Sprite.Class);

   function Size
     (Sprite : Sprite_Type)
      return Non_Negative_Real
   is (Sprite.Size);

   function Location
     (Sprite : Sprite_Type)
      return Encounter_Point
   is (Sprite.Location);

   function Heading
     (Sprite : Sprite_Type)
      return Athena.Trigonometry.Angle
   is (Sprite.Heading);

   function Target
     (Sprite : Sprite_Type)
      return Encounter_Point
   is (Sprite.Target);

   function Color
     (Sprite : Sprite_Type)
      return Nazar.Colors.Nazar_Color
   is (Sprite.Color);

   function Shield
     (Sprite : Sprite_Type)
      return Unit_Real
   is (Sprite.Shield);

   function Make_Sprite
     (Class    : Encounter_Actor_Class;
      Size     : Non_Negative_Real;
      Location : Encounter_Point;
      Heading  : Athena.Trigonometry.Angle;
      Target   : Encounter_Point;
      Color    : Nazar.Colors.Nazar_Color;
      Shield   : Unit_Real)
      return Sprite_Type
   is (Sprite_Type'
         (Class    => Class,
          Size     => Size,
          Location => Location,
          Heading  => Heading,
          Target   => Target,
          Color    => Color,
          Shield   => Shield));

end Athena.Encounters.Sprites;
