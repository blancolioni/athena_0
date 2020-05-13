with Athena.Encounters.Actors;
with Athena.Encounters.Situation;

with Athena.Stars;

package body Athena.Ships.Escape is

   type Escape_Script_Type is
     new Athena.Encounters.Scripts.Encounter_Script_Interface with
       null record;

   overriding procedure Update
     (Script    : Escape_Script_Type;
      Actor     : Athena.Encounters.Actors.Actor_Type;
      Situation : in out
        Athena.Encounters.Situation.Situation_Interface'Class);

   -------------------
   -- Escape_Script --
   -------------------

   function Escape_Script
      return Athena.Encounters.Scripts.Encounter_Script_Interface'Class
   is
   begin
      return Script : Escape_Script_Type;
   end Escape_Script;

   ----------------
   -- Get_Orders --
   ----------------

   overriding procedure Update
     (Script    : Escape_Script_Type;
      Actor     : Athena.Encounters.Actors.Actor_Type;
      Situation : in out
        Athena.Encounters.Situation.Situation_Interface'Class)
   is
      pragma Unreferenced (Script);
      Total_Mass : Non_Negative_Real := 0.0;
      Sum_X      : Real := 0.0;
      Sum_Y      : Real := 0.0;

      procedure Add_Hostile
        (Hostile : Athena.Encounters.Situation.Situation_Actor);

      function Find_Friendly return Athena.Handles.Star.Star_Class;

      -----------------
      -- Add_Hostile --
      -----------------

      procedure Add_Hostile
        (Hostile : Athena.Encounters.Situation.Situation_Actor)
      is
      begin
         Total_Mass := Total_Mass + Hostile.Mass;
         Sum_X := Sum_X + Hostile.DX;
         Sum_Y := Sum_Y + Hostile.DY;
      end Add_Hostile;

      -------------------
      -- Find_Friendly --
      -------------------

      function Find_Friendly return Athena.Handles.Star.Star_Class is
         Closest : Athena.Handles.Star.Star_Handle :=
                     Athena.Handles.Star.Empty_Handle;
         Distance : Non_Negative_Real := Non_Negative_Real'Last;

         procedure Check (Star : Athena.Handles.Star.Star_Class;
                          D    : Non_Negative_Real);

         -----------
         -- Check --
         -----------

         procedure Check (Star : Athena.Handles.Star.Star_Class;
                          D    : Non_Negative_Real)
         is
         begin
            if D < Distance
              and then Star.Owner.Identifier = Actor.Owner.Identifier
            then
               Distance := D;
               Closest  := Athena.Handles.Star.Get (Star.Reference_Star);
            end if;
         end Check;

      begin
         Athena.Stars.Iterate_Nearest (Situation.Star, 99.0, Check'Access);
         return Closest;
      end Find_Friendly;

   begin
      Situation.Iterate_Hostiles (Add_Hostile'Access);

      if Total_Mass > 0.0 then
         declare
            Bad_X : constant Real := Sum_X / Total_Mass;
            Bad_Y : constant Real := Sum_Y / Total_Mass;
         begin
            Actor.Set_Destination
              (-Bad_X * 10.0, -Bad_Y * 10.0);
         end;
      else
         Actor.Clear_Destination;
      end if;

      if not Actor.Is_Jumping then
         declare
            use Athena.Encounters;
         begin
            Actor.Start_Jump
              (Jump_Tick   =>
                 Situation.Current_Tick
               + Encounter_Tick (Actor.Size * 100.0),
               Destination => Find_Friendly);
         end;
      end if;

   end Update;

end Athena.Ships.Escape;
