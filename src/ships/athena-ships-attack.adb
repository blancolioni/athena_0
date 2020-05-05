with Athena.Encounters.Actors;
with Athena.Encounters.Situation;

with Athena.Trigonometry;

package body Athena.Ships.Attack is

   type Attack_Script_Type is
     new Athena.Encounters.Scripts.Encounter_Script_Interface with
       null record;

   overriding procedure Update
     (Script    : Attack_Script_Type;
      Actor     : Athena.Encounters.Actors.Actor_Type;
      Situation : in out
        Athena.Encounters.Situation.Situation_Interface'Class);

   -------------------
   -- Attack_Script --
   -------------------

   function Attack_Script
     return Athena.Encounters.Scripts.Encounter_Script_Interface'Class
   is
   begin
      return Script : Attack_Script_Type;
   end Attack_Script;

   ----------------
   -- Get_Orders --
   ----------------

   overriding procedure Update
     (Script    : Attack_Script_Type;
      Actor     : Athena.Encounters.Actors.Actor_Type;
      Situation : in out
        Athena.Encounters.Situation.Situation_Interface'Class)
   is
      pragma Unreferenced (Script);

   begin

      if not Actor.Is_Following then
         declare
            use Athena.Trigonometry;

            Allied_Centre : constant Athena.Encounters.Encounter_Point :=
                              Situation.Allied_Centre;

            Bearing       : constant Angle :=
                              Arctan (Allied_Centre.Y - Actor.Location.Y,
                                      Allied_Centre.X - Actor.Location.X);

            Have_Hostiles   : Boolean := False;
            Closest_Hostile : Athena.Encounters.Situation.Situation_Actor;
            Lowest_Distance : Non_Negative_Real := Non_Negative_Real'Last;

            procedure Check_Hostile
              (Hostile : Athena.Encounters.Situation.Situation_Actor);

            -------------------
            -- Check_Hostile --
            -------------------

            procedure Check_Hostile
              (Hostile : Athena.Encounters.Situation.Situation_Actor)
            is
               D : constant Non_Negative_Real :=
                     Hostile.DX ** 2 + Hostile.DY ** 2;
            begin
               Have_Hostiles := True;
               if D < Lowest_Distance then
                  Lowest_Distance := D;
                  Closest_Hostile := Hostile;
               end if;
            end Check_Hostile;

         begin
            Situation.Iterate_Hostiles (Check_Hostile'Access);

            if Have_Hostiles then
               Actor.Follow (Closest_Hostile, Bearing, 50.0);
            end if;
         end;
      end if;

      if Actor.Is_Following then
         Actor.Update_Follow_Destination
           (Situation.Get (Actor.Following_Actor));
      end if;

   end Update;

end Athena.Ships.Attack;
