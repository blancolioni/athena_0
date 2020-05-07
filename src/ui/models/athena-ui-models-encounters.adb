with Ada.Text_IO;

with Nazar.Colors;

with Athena.Elementary_Functions;
with Athena.Options;
with Athena.Trigonometry;

with Athena.Encounters.Manager;
with Athena.Encounters.Sprites;

package body Athena.UI.Models.Encounters is

   type Root_Encounter_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Encounter    : Athena.Encounters.Manager.Encounter_Manager_Type;
         Current_Tick : Athena.Encounters.Encounter_Tick;
      end record;

   type Encounter_Model_Access is access all Root_Encounter_Model'Class;

   overriding function Background_Color
     (View : Root_Encounter_Model)
      return Nazar.Colors.Nazar_Color
   is (0.0, 0.0, 0.0, 1.0);

   overriding procedure Reload
     (Model : in out Root_Encounter_Model);

   procedure Draw_Encounter
     (Model : in out Root_Encounter_Model'Class);

   procedure Draw_Sprite
     (Model : in out Root_Encounter_Model'Class;
      Sprite : Athena.Encounters.Sprites.Sprite_Type);

   procedure Draw_Ship
     (Model       : in out Root_Encounter_Model'Class;
      Size        : Non_Negative_Real;
      Color       : Nazar.Colors.Nazar_Color;
      Location    : Athena.Encounters.Encounter_Point;
      Destination : Athena.Encounters.Encounter_Point;
      Heading     : Athena.Trigonometry.Angle;
      Shield      : Unit_Real);

   procedure Draw_Beam
     (Model    : in out Root_Encounter_Model'Class;
      Color    : Nazar.Colors.Nazar_Color;
      Location : Athena.Encounters.Encounter_Point;
      Target   : Athena.Encounters.Encounter_Point);

   ---------------
   -- Draw_Beam --
   ---------------

   procedure Draw_Beam
     (Model    : in out Root_Encounter_Model'Class;
      Color    : Nazar.Colors.Nazar_Color;
      Location : Athena.Encounters.Encounter_Point;
      Target   : Athena.Encounters.Encounter_Point)
   is
   begin
      Model.Save_State;
      Model.Set_Color (Color);
      Model.Move_To (Nazar.Nazar_Float (Location.X),
                     Nazar.Nazar_Float (Location.Y));
      Model.Line_To (Nazar.Nazar_Float (Target.X),
                     Nazar.Nazar_Float (Target.Y));
      Model.Render;
      Model.Restore_State;
   end Draw_Beam;

   --------------------
   -- Draw_Encounter --
   --------------------

   procedure Draw_Encounter
     (Model : in out Root_Encounter_Model'Class)
   is
      procedure Process_Sprite
        (Sprite : Athena.Encounters.Sprites.Sprite_Type);

      --------------------
      -- Process_Sprite --
      --------------------

      procedure Process_Sprite
        (Sprite : Athena.Encounters.Sprites.Sprite_Type)
      is
      begin
         Model.Draw_Sprite (Sprite);
      end Process_Sprite;

   begin
      Model.Clear;

      Model.Encounter.Iterate_Frame
        (Tick    => Model.Current_Tick,
         Process => Process_Sprite'Access);

      declare
         use type Nazar.Nazar_Float;
      begin
         Model.Set_Bounding_Box
           (Box => (-1200.0, -1200.0, 2400.0, 2400.0));
      end;

      declare
         use type Athena.Encounters.Encounter_Tick;
      begin
         Model.Current_Tick := Model.Current_Tick + 1;
      end;

   end Draw_Encounter;

   ---------------
   -- Draw_Ship --
   ---------------

   procedure Draw_Ship
     (Model       : in out Root_Encounter_Model'Class;
      Size        : Non_Negative_Real;
      Color       : Nazar.Colors.Nazar_Color;
      Location    : Athena.Encounters.Encounter_Point;
      Destination : Athena.Encounters.Encounter_Point;
      Heading     : Athena.Trigonometry.Angle;
      Shield      : Unit_Real)
   is
      use Athena.Encounters;
      A      : constant Encounter_Point :=
                 Rotate (Translate (Location, X => Size * 0.75),
                         Location,
                         Heading);
      B      : constant Encounter_Point :=
                 Rotate (Translate
                         (Location,
                            X => -Size / 2.0,
                            Y => Size / 2.0),
                         Location,
                         Heading);
      C      : constant Encounter_Point :=
                 Rotate (Translate
                         (Location,
                            X => -Size / 2.0,
                            Y => -Size / 2.0),
                         Location,
                         Heading);
   begin

      Model.Save_State;
      Model.Set_Color (Color);
      Model.Set_Fill (True);
      Model.Move_To (Nazar.Nazar_Float (A.X),
                     Nazar.Nazar_Float (A.Y));
      Model.Line_To (Nazar.Nazar_Float (B.X),
                     Nazar.Nazar_Float (B.Y));
      Model.Line_To (Nazar.Nazar_Float (C.X),
                     Nazar.Nazar_Float (C.Y));
      Model.Line_To (Nazar.Nazar_Float (A.X),
                     Nazar.Nazar_Float (A.Y));
      Model.Render;
      Model.Restore_State;

      if Shield > 0.0 then
         declare
            E : constant Nazar.Nazar_Unit_Float :=
                  Nazar.Nazar_Unit_Float (Shield * 0.75 + 0.25);
         begin
            Model.Save_State;
            Model.Set_Color (E, E, E, 1.0);
            Model.Move_To (Nazar.Nazar_Float (Location.X),
                           Nazar.Nazar_Float (Location.Y));
            Model.Circle (Nazar.Nazar_Float (Size * 0.52));
            Model.Render;
            Model.Restore_State;
         end;
      end if;

      if Destination /= Location
        and then Athena.Options.Show_Destination
      then
         Model.Save_State;
         Model.Set_Color (0.5, 0.5, 0.5, 1.0);
         Model.Move_To (Nazar.Nazar_Float (Location.X),
                        Nazar.Nazar_Float (Location.Y));
         Model.Line_To (Nazar.Nazar_Float (Destination.X),
                        Nazar.Nazar_Float (Destination.Y));
         Model.Render;
         Model.Restore_State;
      end if;

   end Draw_Ship;

   -----------------
   -- Draw_Sprite --
   -----------------

   procedure Draw_Sprite
     (Model  : in out Root_Encounter_Model'Class;
      Sprite : Athena.Encounters.Sprites.Sprite_Type)
   is
      use all type Athena.Encounters.Encounter_Actor_Class;
      use Athena.Encounters.Sprites;
   begin
      case Class (Sprite) is
         when Ship_Actor =>
            Model.Draw_Ship
              (Size        =>
                  5.0 *
                  Athena.Elementary_Functions.Sqrt (Size (Sprite)),
               Color       => Color (Sprite),
               Location    => Location (Sprite),
               Destination => Target (Sprite),
               Heading     => Heading (Sprite),
               Shield      => Shield (Sprite));
         when Beam_Actor =>
            Model.Draw_Beam
              (Color (Sprite), Location (Sprite), Target (Sprite));
         when others =>
            null;
      end case;
   end Draw_Sprite;

   ---------------------
   -- Encounter_Model --
   ---------------------

   function Encounter_Model
     (Encounter : Athena.Handles.Encounter.Encounter_Class)
      return Nazar.Models.Draw.Nazar_Draw_Model
   is
      Model : constant Encounter_Model_Access :=
                new Root_Encounter_Model'
                  (Nazar.Models.Draw.Root_Draw_Model with
                   Encounter =>
                     Athena.Encounters.Manager.Load_Encounter
                       (Encounter),
                   Current_Tick => 0);
   begin
      Ada.Text_IO.Put ("Loading encounter ... ");
      Ada.Text_IO.Flush;

      Model.Draw_Encounter;

      Ada.Text_IO.Put_Line ("done");

      return Nazar.Models.Draw.Nazar_Draw_Model (Model);
   end Encounter_Model;

   ------------
   -- Reload --
   ------------

   overriding procedure Reload
     (Model : in out Root_Encounter_Model)
     is
   begin
      Model.Draw_Encounter;
      Model.Notify_Observers;
   end Reload;

   ------------
   -- Rotate --
   ------------

--     function Rotate
--       (Pt     : Point;
--        Around : Point;
--        Angle  : Real)
--        return Point
--     is
--        Cos : constant Signed_Unit_Real :=
--                Athena.Elementary_Functions.Cos (Angle, 360.0);
--        Sin : constant Signed_Unit_Real :=
--                Athena.Elementary_Functions.Sin (Angle, 360.0);
--        X   : constant Real := Pt.X - Around.X;
--        Y   : constant Real := Pt.Y - Around.Y;
--     begin
--        return Point'
--          (X => Around.X + X * Cos - Y * Sin,
--           Y => Around.Y + X * Sin + Y * Cos);
--     end Rotate;

end Athena.UI.Models.Encounters;
