private with Ada.Containers.Doubly_Linked_Lists;

with Athena.Encounters.Sprites;

package Athena.Encounters.Frames is

   type Encounter_Frame is private;

   procedure Add_Sprite
     (Frame  : in out Encounter_Frame;
      Sprite : Athena.Encounters.Sprites.Sprite_Type);

   procedure Iterate_Frame
     (Frame   : Encounter_Frame;
      Process : not null access
        procedure (Sprite : Athena.Encounters.Sprites.Sprite_Type));

private

   package Sprite_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Athena.Encounters.Sprites.Sprite_Type,
        Athena.Encounters.Sprites."=");

   type Encounter_Frame is
      record
         Sprite_List : Sprite_Lists.List;
      end record;

end Athena.Encounters.Frames;
