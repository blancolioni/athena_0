with Athena.Encounters.Actors;
with Athena.Encounters.Situation;

with Athena.Elementary_Functions;
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

   function Is_Closer
     (Left, Right : Athena.Encounters.Situation.Situation_Actor)
      return Boolean
   is (Left.DX ** 2 + Left.DY ** 2 < Right.DX ** 2 + Right.DY ** 2);

   package List_Of_Actors is
     new Ada.Containers.Doubly_Linked_Lists
       (Athena.Encounters.Situation.Situation_Actor,
        Athena.Encounters.Situation."=");

   package Range_Sorting is
     new List_Of_Actors.Generic_Sorting (Is_Closer);

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

      Hostiles : List_Of_Actors.List;

      procedure Add_Hostile
        (Hostile : Athena.Encounters.Situation.Situation_Actor);

      -----------------
      -- Add_Hostile --
      -----------------

      procedure Add_Hostile
        (Hostile : Athena.Encounters.Situation.Situation_Actor)
      is
      begin
         Hostiles.Append (Hostile);
      end Add_Hostile;

   begin

      Situation.Iterate_Hostiles (Add_Hostile'Access);

      if Hostiles.Is_Empty then
         return;
      end if;

      Range_Sorting.Sort (Hostiles);

      if not Actor.Is_Following then
         declare
            use Athena.Trigonometry;

            Allied_Centre : constant Athena.Encounters.Encounter_Point :=
                              Situation.Allied_Centre;

            Bearing       : constant Angle :=
                              Arctan (Allied_Centre.Y - Actor.Location.Y,
                                      Allied_Centre.X - Actor.Location.X);

         begin
            Situation.Iterate_Hostiles (Add_Hostile'Access);

            Actor.Follow (Hostiles.First_Element, Bearing, 50.0);
         end;
      end if;

      if Actor.Is_Following then
         Actor.Update_Follow_Destination
           (Situation.Get (Actor.Following_Actor));
      end if;

      declare
         procedure Check_Beam_Weapon
           (Weapon : Athena.Handles.Ship_Component.Ship_Component_Class;
            Charge : Unit_Real);

         -----------------------
         -- Check_Beam_Weapon --
         -----------------------

         procedure Check_Beam_Weapon
           (Weapon : Athena.Handles.Ship_Component.Ship_Component_Class;
            Charge : Unit_Real)
         is
         begin
            if not Hostiles.Is_Empty
              and then Charge > 0.5
            then
               declare
                  H : constant Athena.Encounters.Situation.Situation_Actor :=
                        Hostiles.First_Element;
                  R : constant Non_Negative_Real :=
                        Athena.Elementary_Functions.Sqrt
                          (H.DX ** 2 + H.DY ** 2);
               begin

                  if Athena.Encounters.Hit_Chance (Weapon, H.Mass, R)
                    > 0.75
                  then
                     Situation.Fire_Weapon
                       (Weapon => Weapon,
                        Target => H.Index);
                     Hostiles.Delete_First;
                  end if;

               end;
            end if;
         end Check_Beam_Weapon;

      begin
         Actor.Iterate_Beam_Weapons (Check_Beam_Weapon'Access);
      end;

   end Update;

end Athena.Ships.Attack;
