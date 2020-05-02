with Ada.Containers.Vectors;
with Ada.Text_IO;

with Nazar.Colors;

with Athena.Elementary_Functions;
with Athena.Real_Images;

with Athena.Ships;

with Athena.Handles.Participant.Selections;

with Athena.Db;
with Athena.Db.Encounter_Action;

package body Athena.UI.Models.Encounters is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   type Point is
      record
         X, Y : Real;
      end record;

   function Translate
     (Pt   : Point;
      X, Y : Real := 0.0)
         return Point
   is (Pt.X + X, Pt.Y + Y);

   function Rotate
     (Pt     : Point;
      Around : Point;
      Angle  : Real)
         return Point;

   type Encounter_Sprite_Shape is
     (Triangle);

   type Encounter_Sprite is
      record
         Shape    : Encounter_Sprite_Shape;
         Size     : Non_Negative_Real;
         Color    : Nazar.Colors.Nazar_Color;
         Location : Point;
         Heading  : Real;
         Shield   : Unit_Real;
         Action   : Athena.Db.Ship_Encounter_Action;
         Target   : Natural;
      end record;

   package Sprite_Vectors is
     new Ada.Containers.Vectors (Positive, Encounter_Sprite);

   package State_Vectors is
     new Ada.Containers.Vectors
       (Natural, Sprite_Vectors.Vector, Sprite_Vectors."=");

   package Participant_Vectors is
     new Ada.Containers.Vectors
       (Positive, Athena.Handles.Participant.Participant_Handle,
        Athena.Handles.Participant."=");

   type Encounter_State is
      record
         Sprites      : State_Vectors.Vector;
         Participants : Participant_Vectors.Vector;
      end record;

   procedure Load_State
     (For_Encounter : Athena.Handles.Encounter.Encounter_Class;
      State         : in out Encounter_State);

   type Root_Encounter_Model is
     new Nazar.Models.Draw.Root_Draw_Model with
      record
         Encounter    : Athena.Handles.Encounter.Encounter_Handle;
         Current_Tick : Natural;
         State        : Encounter_State;
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

   --------------------
   -- Draw_Encounter --
   --------------------

   procedure Draw_Encounter
     (Model : in out Root_Encounter_Model'Class)
   is
      Sprites : Sprite_Vectors.Vector renames
                  Model.State.Sprites (Model.Current_Tick);
--        Participants : Participant_Vectors.Vector renames
--                         Model.State.Participants;
   begin
      Model.Clear;
      Model.Set_Color (1.0, 1.0, 1.0, 1.0);

      for I in 1 .. Sprites.Last_Index loop
         declare
            Sprite : Encounter_Sprite renames Sprites (I);
            A      : constant Point :=
                       Rotate (Translate (Sprite.Location, X => Sprite.Size),
                               Sprite.Location,
                               Sprite.Heading);
            B      : constant Point :=
                       Rotate (Translate
                               (Sprite.Location,
                                  X => -Sprite.Size / 2.0,
                                  Y => Sprite.Size / 2.0),
                               Sprite.Location,
                               Sprite.Heading);
            C      : constant Point :=
                       Rotate (Translate
                               (Sprite.Location,
                                  X => -Sprite.Size / 2.0,
                                  Y => -Sprite.Size / 2.0),
                               Sprite.Location,
                               Sprite.Heading);
         begin

            Ada.Text_IO.Put_Line
              ("tick" & Model.Current_Tick'Image
               & "; sprite" & I'Image
               & ": size " & Image (Sprite.Size)
               & "; position ("
               & Image (Sprite.Location.X) & "," & Image (Sprite.Location.Y)
               & ") heading "
               & Image (Sprite.Heading));

            Model.Save_State;
            Model.Set_Color (Sprite.Color);
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
         end;
      end loop;

      declare
         use type Nazar.Nazar_Float;
      begin
         Model.Set_Bounding_Box
           (Box => (-1200.0, -1200.0, 2400.0, 2400.0));
      end;

   end Draw_Encounter;

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
                   Encounter => Athena.Handles.Encounter.Get
                     (Encounter.Reference_Encounter),
                   Current_Tick => 0,
                   State        => (others => <>));
   begin
      Ada.Text_IO.Put ("Loading encounter ... ");
      Ada.Text_IO.Flush;

      Load_State (Encounter, Model.State);
      Model.Draw_Encounter;

      Ada.Text_IO.Put_Line ("done");

      return Nazar.Models.Draw.Nazar_Draw_Model (Model);
   end Encounter_Model;

   ----------------
   -- Load_State --
   ----------------

   procedure Load_State
     (For_Encounter : Athena.Handles.Encounter.Encounter_Class;
      State         : in out Encounter_State)
   is
      Tick : Natural := 0;
   begin

      declare
         use Athena.Handles.Participant.Selections;
      begin
         for Participant of Select_Where (Encounter = For_Encounter) loop
            State.Participants.Append
              (Athena.Handles.Participant.Get
                 (Participant.Reference_Participant));
         end loop;
      end;

      loop
         declare
            Has_Actions : Boolean := False;
            Round       : Sprite_Vectors.Vector;
         begin
            for Participant of State.Participants loop
               for Action of
                 Athena.Db.Encounter_Action.Select_By_Participant_Action
                   (For_Encounter.Reference_Encounter,
                    Participant.Reference_Participant,
                    Tick)
               loop
                  Has_Actions := True;
                  declare
                     Size   : constant Non_Negative_Real :=
                                Athena.Elementary_Functions.Sqrt
                                  (Athena.Ships.Design_Mass
                                     (Participant.Ship.Ship_Design))
                                * 5.0;
                     Sprite : constant Encounter_Sprite :=
                                Encounter_Sprite'
                                  (Shape   => Triangle,
                                   Color   =>
                                     To_Nazar_Color
                                       (Participant.Ship.Empire.Rgb),
                                   Size    => Size,
                                   Location => (Action.X, Action.Y),
                                   Heading => Action.Heading,
                                   Shield  => Action.Shield,
                                   Action  => Action.Action,
                                   Target  => 0);
                  begin
                     Round.Append (Sprite);
                  end;
               end loop;
            end loop;

            State.Sprites.Append (Round);

            exit when not Has_Actions;

            Tick := Tick + 1;
         end;
      end loop;

   end Load_State;

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

   function Rotate
     (Pt     : Point;
      Around : Point;
      Angle  : Real)
      return Point
   is
      Cos : constant Signed_Unit_Real :=
              Athena.Elementary_Functions.Cos (Angle, 360.0);
      Sin : constant Signed_Unit_Real :=
              Athena.Elementary_Functions.Sin (Angle, 360.0);
      X   : constant Real := Pt.X - Around.X;
      Y   : constant Real := Pt.Y - Around.Y;
   begin
      return Point'
        (X => Around.X + X * Cos - Y * Sin,
         Y => Around.Y + X * Sin + Y * Cos);
   end Rotate;

end Athena.UI.Models.Encounters;
