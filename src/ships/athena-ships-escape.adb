with Athena.Encounters.Actors;
with Athena.Encounters.Situation;

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

   begin
      Situation.Iterate_Hostiles (Add_Hostile'Access);

      declare
         Bad_X : constant Real := Sum_X / Total_Mass;
         Bad_Y : constant Real := Sum_Y / Total_Mass;
      begin
         Actor.Set_Destination
           (-Bad_X * 10.0, -Bad_Y * 10.0);
      end;
   end Update;

end Athena.Ships.Escape;
