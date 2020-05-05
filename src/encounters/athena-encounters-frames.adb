package body Athena.Encounters.Frames is

   ----------------
   -- Add_Sprite --
   ----------------

   procedure Add_Sprite
     (Frame  : in out Encounter_Frame;
      Sprite :        Athena.Encounters.Sprites.Sprite_Type)
   is
   begin
      Frame.Sprite_List.Append (Sprite);
   end Add_Sprite;

   -------------------
   -- Iterate_Frame --
   -------------------

   procedure Iterate_Frame
     (Frame   : Encounter_Frame;
      Process : not null access procedure
        (Sprite : Athena.Encounters.Sprites.Sprite_Type))
   is
   begin
      for Sprite of Frame.Sprite_List loop
         Process (Sprite);
      end loop;
   end Iterate_Frame;

end Athena.Encounters.Frames;
