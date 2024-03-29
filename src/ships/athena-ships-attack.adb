with Ada.Strings.Unbounded;

with Athena.Encounters.Actors;
with Athena.Encounters.Situation;

with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Random;
with Athena.Trigonometry;

with Athena.Ships.Escape;

package body Athena.Ships.Attack is

   type Attack_Script_Type is
     new Athena.Encounters.Scripts.Encounter_Script_Interface with
      record
         Defensive : Boolean;
      end record;

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
      return Attack_Script_Type'
        (Defensive => False);
   end Attack_Script;

   -------------------
   -- Defend_Script --
   -------------------

   function Defend_Script
     return Athena.Encounters.Scripts.Encounter_Script_Interface'Class
   is
   begin
      return Attack_Script_Type'
        (Defensive => True);
   end Defend_Script;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Script    : Attack_Script_Type;
      Actor     : Athena.Encounters.Actors.Actor_Type;
      Situation : in out
        Athena.Encounters.Situation.Situation_Interface'Class)
   is
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

      if not Script.Defensive
        and then (not Actor.Is_Following
                  or else Situation.Get (Actor.Following_Actor).Dead)
      then
         declare
            use Athena.Trigonometry;

            Bearing       : constant Angle :=
                              From_Degrees (Athena.Random.Unit_Random * 360.0);

            Total_Range_Weight : Non_Negative_Real := 0.0;
            Total_Weight       : Non_Negative_Real := 0.0;

            Speed_Limit : Non_Negative_Real := Non_Negative_Real'Last;

            function Ideal_Range return Non_Negative_Real
            is (if Total_Weight = 0.0
                then 1.0e6
                else Real'Max (Total_Range_Weight / Total_Weight, 20.0));

            procedure Check_Ally
              (Ally : Athena.Encounters.Situation.Situation_Actor);

            procedure Check_Weapon
              (Component : Athena.Handles.Ship_Component.Ship_Component_Class;
               Charge    : Unit_Real);

            ----------------
            -- Check_Ally --
            ----------------

            procedure Check_Ally
              (Ally : Athena.Encounters.Situation.Situation_Actor)
            is
               use type Ada.Strings.Unbounded.Unbounded_String;
            begin
               if Ally.Script = "attack" then
                  if Ally.Max_Speed > 1.0
                    and then Ally.Max_Speed < Speed_Limit
                  then
                     Speed_Limit := Ally.Max_Speed;
                  end if;
               end if;
            end Check_Ally;

            ------------------
            -- Check_Weapon --
            ------------------

            procedure Check_Weapon
              (Component : Athena.Handles.Ship_Component.Ship_Component_Class;
               Charge    : Unit_Real)
            is
               pragma Unreferenced (Charge);
               Weight : constant Non_Negative_Real :=
                          Component.Design_Component.Mass
                            * Component.Condition;
               R      : constant Non_Negative_Real :=
                          (if Weight = 0.0
                           then 0.0
                           else Athena.Encounters.Range_At_Hit_Chance
                            (Weapon      => Component,
                             Target_Size => Hostiles.First_Element.Size,
                             Hit_Chance  => 0.8));
            begin
               Total_Weight := Total_Weight + Weight;
               Total_Range_Weight := Total_Range_Weight + Weight * R;
            end Check_Weapon;

         begin

            Situation.Iterate_Allies (Check_Ally'Access);

            Actor.Set_Speed_Limit (Speed_Limit);

            Actor.Iterate_Beam_Weapons (Check_Weapon'Access);

            if Total_Weight = 0.0 then
               Athena.Ships.Escape.Escape_Script.Update
                 (Actor, Situation);
               return;
            end if;

            Actor.Follow (Hostiles.First_Element, Bearing, Ideal_Range);
            Athena.Logging.Log
              (Actor.Image & ": following hostile at range"
               & Natural'Image (Natural (Ideal_Range)));
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

                  if Athena.Encounters.Hit_Chance (Weapon, H.Size, R) > 0.5
                    or else R < Athena.Encounters.Maximum_Range (Weapon) / 2.0
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
