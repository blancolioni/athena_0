with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with WL.String_Maps;

with Athena.Logging;
with Athena.Random;

with Athena.Encounters.Scripts;
with Athena.Encounters.Situation;

with Athena.Ships.Scripts;
with Athena.Treaties;

with Athena.Handles.Antagonist.Selections;
with Athena.Handles.Participant.Selections;

with Athena.Handles.Empire;
with Athena.Handles.Participant;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Component;
with Athena.Handles.Star;

package body Athena.Encounters.Execution is

   procedure Log (Actor : Athena.Encounters.Actors.Actor_Type);

   type Team_Record is
      record
         Empire : Athena.Handles.Empire.Empire_Handle;
         Ships  : Athena.Ships.Ship_Lists.List;
      end record;

   package Team_Maps is
     new WL.String_Maps (Team_Record);

   package Actor_Vectors is
     new Ada.Containers.Vectors
       (Encounter_Actor_Reference, Athena.Encounters.Actors.Actor_Type,
        Athena.Encounters.Actors."=");

   package Script_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Encounter_Actor_Reference,
        Athena.Encounters.Scripts.Encounter_Script_Interface'Class,
        Athena.Encounters.Scripts."=");

   type State_Record is
      record
         Encounter : Athena.Handles.Encounter.Encounter_Handle;
         Actors    : Actor_Vectors.Vector;
         Scripts   : Script_Vectors.Vector;
         Teams     : Team_Maps.Map;
         Tick      : Encounter_Tick;
         Victor    : Athena.Handles.Empire.Empire_Handle;
         Countdown : Natural;
         Finished  : Boolean;
      end record;

   type State_Type is access State_Record;

   function New_State
     (For_Encounter : Athena.Handles.Encounter.Encounter_Class;
      Manager       : in out Manager_Interface'Class)
     return State_Type;

   procedure Step
     (State     : State_Type;
      Manager   : in out Manager_Interface'Class);

   procedure Log (State   : State_Type;
                  Message : String);

   function Have_Hostiles
     (State : State_Type)
      return Boolean;

   type Situation_Type is
     new Athena.Encounters.Situation.Situation_Interface with
      record
         State       : State_Type;
         Actor       : Athena.Encounters.Actors.Actor_Type;
         New_Actors  : Actor_Vectors.Vector;
         New_Scripts : Script_Vectors.Vector;
      end record;

   overriding function Star
     (Situation : Situation_Type)
      return Athena.Handles.Star.Star_Class
   is (Situation.State.Encounter.Star);

   overriding function Origin
     (Situation : Situation_Type)
      return Encounter_Point
   is (Situation.Actor.Location);

   overriding function Current_Tick
     (Situation : Situation_Type)
      return Encounter_Tick
   is (Situation.State.Tick);

   overriding function Heading
     (Situation : Situation_Type)
      return Athena.Trigonometry.Angle
   is (Situation.Actor.Heading);

   overriding function Get
     (Situation : Situation_Type;
      Actor     : Encounter_Actor_Reference)
      return Athena.Encounters.Situation.Situation_Actor;

   overriding procedure Iterate_Hostiles
     (Situation : Situation_Type;
      Process   : not null access
        procedure (Hostile : Athena.Encounters.Situation.Situation_Actor));

   overriding procedure Iterate_Allies
     (Situation : Situation_Type;
      Process   : not null access
        procedure (Ally : Athena.Encounters.Situation.Situation_Actor));

   overriding procedure Fire_Weapon
     (Situation : in out Situation_Type;
      Weapon    : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target    : Encounter_Actor_Reference);

   procedure Iterate_Actors
     (Situation : Situation_Type'Class;
      Test      : not null access
        function (Actor : Athena.Encounters.Actors.Actor_Type)
        return Boolean;
      Process   : not null access
        procedure (Actor : Athena.Encounters.Situation.Situation_Actor));

   -----------------
   -- Fire_Weapon --
   -----------------

   overriding procedure Fire_Weapon
     (Situation : in out Situation_Type;
      Weapon    : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target    : Encounter_Actor_Reference)
   is
      Actor : constant Athena.Encounters.Actors.Actor_Type :=
                Situation.State.Actors.Element (Target);
      Beam : constant Athena.Encounters.Actors.Actor_Type :=
               Athena.Encounters.Actors.Create_Beam_Actor
                 (Situation.State.Actors.Last_Index,
                  Situation.State.Tick,
                  Weapon.Ship, Situation.Actor.Location,
                  Actor.Location);
      Target_Size : constant Non_Negative_Real := Actor.Size;
      Target_Range : constant Non_Negative_Real :=
                       Situation.Get_Range (Target);
      P_Hit : constant Unit_Real :=
                Hit_Chance
                  (Weapon       => Weapon,
                   Target_Size  => Target_Size,
                   Target_Range => Target_Range);
      Hit          : constant Boolean := Athena.Random.Unit_Random <= P_Hit;
      Damage       : constant Non_Negative_Real :=
                       (if Hit
                        then Hit_Power (Weapon, Target_Range)
                        else 0.0);
   begin
      Situation.New_Actors.Append (Beam);
      Situation.New_Scripts.Append (Athena.Encounters.Scripts.Beam_Script);
      Situation.Actor.Weapon_Fired (Weapon);

      if Hit then
         Athena.Ships.Add_Experience (Weapon.Ship, 0.005);
         Actor.Hit (Damage);
         if Actor.Is_Dead then
            Athena.Ships.Add_Experience (Weapon.Ship, 0.01);
         end if;
      end if;

      Log
        (Situation.State,
         Weapon.Ship.Empire.Name & " ship "
         & Weapon.Ship.Identifier & " " & Weapon.Ship.Name & " fires at "
         & Actor.Image
         & ": range " & Image (Target_Range)
         & "/" & Image (Maximum_Range (Weapon))
         & "; target size " & Image (Target_Size)
         & "; tec level " & Image (Weapon.Tec_Level)
         & "; condition " & Image (Weapon.Condition * 100.0) & "%"
         & "; chance " & Image (P_Hit * 100.0) & "%"
         & "; result = " & (if Hit then "HIT" else "miss")
         & "; damage = " & Image (Damage));
   end Fire_Weapon;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Situation : Situation_Type;
      Actor     : Encounter_Actor_Reference)
      return Athena.Encounters.Situation.Situation_Actor
   is
      A : constant Athena.Encounters.Actors.Actor_Type :=
            Situation.State.Actors.Element (Actor);
   begin
      return A.Current_Situation (Situation);
   end Get;

   -------------------
   -- Have_Hostiles --
   -------------------

   function Have_Hostiles
     (State : State_Type)
      return Boolean
   is
      Active : Actor_Vectors.Vector;
      Result : Boolean := False;
   begin
      for Actor of State.Actors loop
         if Actor.Is_Active (State.Tick) and then not Actor.Is_Dead then
            for A of Active loop
               if A.Owner.Identifier /= Actor.Owner.Identifier
                 and then Athena.Treaties.At_War
                   (A.Owner, Actor.Owner)
               then
                  Result := True;
                  exit;
               end if;
            end loop;
            exit when Result;
            Active.Append (Actor);
         end if;
      end loop;

      if not Result
        and then not Active.Is_Empty
      then
         State.Victor :=
           Athena.Handles.Empire.Get
             (Active.First_Element.Owner.Reference_Empire);
      end if;

      return Result;

   end Have_Hostiles;

   --------------------
   -- Iterate_Actors --
   --------------------

   procedure Iterate_Actors
     (Situation : Situation_Type'Class;
      Test      : not null access
        function (Actor : Athena.Encounters.Actors.Actor_Type)
      return Boolean;
      Process   : not null access
        procedure (Actor : Athena.Encounters.Situation.Situation_Actor))
   is
   begin
      for Actor of Situation.State.Actors loop
         if Test (Actor) then
            Process (Actor.Current_Situation (Situation));
         end if;
      end loop;
   end Iterate_Actors;

   --------------------
   -- Iterate_Allies --
   --------------------

   overriding procedure Iterate_Allies
     (Situation : Situation_Type;
      Process   : not null access
        procedure (Ally : Athena.Encounters.Situation.Situation_Actor))
   is
      function Is_Ally
        (Actor : Athena.Encounters.Actors.Actor_Type)
         return Boolean
      is (Actor.Is_Active (Situation.State.Tick)
          and then not Actor.Is_Dead
          and then Actor.Owner.Identifier = Situation.Actor.Owner.Identifier);
   begin
      Situation.Iterate_Actors (Is_Ally'Access, Process);
   end Iterate_Allies;

   ----------------------
   -- Iterate_Hostiles --
   ----------------------

   overriding procedure Iterate_Hostiles
     (Situation : Situation_Type;
      Process   : not null access
        procedure (Hostile : Athena.Encounters.Situation.Situation_Actor))
   is
      function Is_Hostile
        (Actor : Athena.Encounters.Actors.Actor_Type)
         return Boolean
      is (Actor.Is_Active (Situation.State.Tick)
          and then not Actor.Is_Dead
          and then Actor.Owner.Identifier /= Situation.Actor.Owner.Identifier);
   begin
      Situation.Iterate_Actors (Is_Hostile'Access, Process);
   end Iterate_Hostiles;

   ---------
   -- Log --
   ---------

   procedure Log (State   : State_Type;
                  Message : String)
   is
   begin
      Athena.Logging.Log
        ("timestamp" & State.Tick'Image
         & ": " & Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Actor : Athena.Encounters.Actors.Actor_Type) is
   begin
      Athena.Logging.Log (Actor.Image);
   end Log;

   ---------------
   -- New_State --
   ---------------

   function New_State
     (For_Encounter : Athena.Handles.Encounter.Encounter_Class;
      Manager       : in out Manager_Interface'Class)
      return State_Type
   is

      State : constant State_Type :=
                new State_Record'
                  (Encounter => Athena.Handles.Encounter.Get
                     (For_Encounter.Reference_Encounter),
                   Actors    => <>,
                   Scripts   => <>,
                   Teams     => <>,
                   Tick      => 0,
                   Finished  => False,
                   Countdown => 0,
                   Victor    => Athena.Handles.Empire.Empty_Handle);

      procedure Deploy
        (Team    : Team_Record;
         Radius  : Non_Negative_Real;
         Bearing : Athena.Trigonometry.Angle);

      package Participant_Maps is
        new WL.String_Maps (Athena.Handles.Participant.Participant_Class,
                            Athena.Handles.Participant."=");

      Participants : Participant_Maps.Map;

      ------------
      -- Deploy --
      ------------

      procedure Deploy
        (Team    : Team_Record;
         Radius  : Non_Negative_Real;
         Bearing : Athena.Trigonometry.Angle)
      is
         use Athena.Trigonometry;
         Ships : constant Athena.Ships.Ship_Lists.List := Team.Ships;
         Ranks : array (Deployment_Rank) of Athena.Ships.Ship_Lists.List;

         procedure Deploy_Rank
           (Ships    : Athena.Ships.Ship_Lists.List;
            Distance : in out Non_Negative_Real);

         -----------------
         -- Deploy_Rank --
         -----------------

         procedure Deploy_Rank
           (Ships    : Athena.Ships.Ship_Lists.List;
            Distance : in out Non_Negative_Real)
         is
            Max_Width     : constant := 12;
            Separation    : constant := 1.0;
            Rank_Distance : constant := 50.0;
            List        : Athena.Ships.Ship_Lists.List := Ships;
         begin

            while not List.Is_Empty loop
               declare
                  Rank_Length : constant Positive :=
                                  Positive'Min (Positive (List.Length),
                                                Max_Width);
                  Offset      : constant Real :=
                                  (Real (Rank_Length) / 2.0 - 0.5)
                                  * Separation;
                  Theta       : Angle :=
                                  Bearing - From_Degrees (Offset);
               begin
                  for I in 1 .. Rank_Length loop
                     declare
                        Ship : constant Athena.Handles.Ship.Ship_Handle :=
                                 List.First_Element;
                        X    : constant Real := Distance * Cos (Theta);
                        Y    : constant Real := Distance * Sin (Theta);
                     begin
                        List.Delete_First;
                        State.Actors.Append
                          (Athena.Encounters.Actors.Create_Ship_Actor
                             (Index   => State.Actors.Last_Index + 1,
                              Tick    => 0,
                              Ship    => Ship,
                              X       => X,
                              Y       => Y,
                              Heading => Bearing + From_Degrees (180.0)));
                        State.Scripts.Append
                          (Athena.Ships.Scripts.Get_Script
                             (Ship.Script));
                        Manager.Add_Actor (State.Actors.Last_Element);
                        Athena.Ships.Add_Experience (Ship, 0.01);

                        Theta := Theta + From_Degrees (Separation);
                     end;
                  end loop;
               end;
               Distance := Distance + Rank_Distance;
            end loop;
         end Deploy_Rank;

      begin
         for Ship of Ships loop
            declare
               Rank : constant Deployment_Rank :=
                        Deployment_Rank (Ship.Ship_Design.Default_Rank);
            begin
               Ranks (Rank).Append (Ship);
            end;
         end loop;

         declare
            R : Non_Negative_Real := Radius;
         begin
            for Rank of Ranks loop
               if not Rank.Is_Empty then
                  Deploy_Rank (Rank, R);
               end if;
            end loop;
         end;

      end Deploy;

      Encounter_Radius : Non_Negative_Real := 0.0;

   begin

      declare
         use Athena.Handles.Antagonist.Selections;
      begin
         for Antagonist of Select_Where (Encounter = State.Encounter) loop
            State.Teams.Insert
              (Antagonist.Empire.Identifier,
               (Athena.Handles.Empire.Get
                    (Antagonist.Empire.Reference_Empire),
                Athena.Ships.Ship_Lists.Empty_List));
         end loop;
      end;

      declare
         use Athena.Handles.Participant.Selections;
      begin
         for Participant of Select_Where (Encounter = State.Encounter) loop
            declare
               Handle : constant Athena.Handles.Ship.Ship_Handle :=
                          Athena.Handles.Ship.Get
                            (Participant.Ship.Reference_Ship);
               Weapon_Range : constant Non_Negative_Real :=
                                Athena.Ships.Weapon_Range (Handle);
            begin
               State.Teams (Participant.Ship.Empire.Identifier)
                 .Ships.Append (Handle);
               Participants.Insert (Handle.Identifier, Participant);
               Encounter_Radius := Real'Max (Weapon_Range, Encounter_Radius);
            end;
         end loop;
      end;

      declare
         use Athena.Trigonometry;
         Angular_Sep   : constant Angle :=
                           From_Degrees (360.0 / Real (State.Teams.Length));
         Current_Angle : Angle := From_Degrees (0.0);
      begin
         for Team of State.Teams loop
            Athena.Logging.Log ("   team: " & Team.Empire.Name);
            for Ship of Team.Ships loop
               Athena.Logging.Log
                 ("        " & Ship.Name
                  & ": mass " & Image (Athena.Ships.Mass (Ship))
                  & "; drive mass "
                  & Image (Athena.Ships.Get_Drive (Ship).Design_Component.Mass)
                  & " tec "
                  & Image (Athena.Ships.Get_Drive (Ship).Tec_Level)
                  & "; speed " & Image (Athena.Ships.Speed (Ship))
                  & "; experience " & Image (Ship.Experience));
            end loop;
            Deploy (Team, Encounter_Radius, Current_Angle);
            Current_Angle := Current_Angle + Angular_Sep;
         end loop;
      end;

      return State;

   end New_State;

   -------------------
   -- Run_Encounter --
   -------------------

   procedure Run_Encounter
     (Encounter :        Athena.Handles.Encounter.Encounter_Class;
      Manager   : in out Manager_Interface'Class)
   is
      State : constant State_Type := New_State (Encounter, Manager);
   begin

      while not State.Finished loop
         Step (State, Manager);
      end loop;

      if State.Victor.Has_Element then
         Athena.Logging.Log
           ("Victory to " & State.Victor.Name);
      else
         Athena.Logging.Log
           ("Stalemate");
      end if;
   end Run_Encounter;

   ----------
   -- Step --
   ----------

   procedure Step
     (State     : State_Type;
      Manager   : in out Manager_Interface'Class)
   is
   begin
      for Index in 1 .. State.Scripts.Last_Index loop
         if State.Actors.Element (Index).Is_Active (State.Tick)
           and then not State.Actors.Element (Index).Is_Dead
         then
            declare
               Situation : Situation_Type :=
                             Situation_Type'
                               (State       => State,
                                Actor       => State.Actors (Index),
                                New_Actors  => <>,
                                New_Scripts => <>);
            begin
               State.Scripts.Element (Index).Update
                 (State.Actors (Index), Situation);
               for Actor of Situation.New_Actors loop
                  State.Actors.Append (Actor);
               end loop;
               for Script of Situation.New_Scripts loop
                  State.Scripts.Append (Script);
               end loop;
            end;
         end if;
      end loop;

      for Actor of State.Actors loop
         if Actor.Is_Active (State.Tick) then
            Actor.Update;

            if Actor.Is_Jumping
              and then Actor.Jump_Tick = State.Tick
            then
               Athena.Logging.Log
                 (Actor.Image
                  & " jumps to " & Actor.Jump_Destination.Name);
               Actor.Execute_Jump;
               Actor.Destroy_Actor (State.Tick);
            end if;
         end if;
      end loop;

      if False then
         Log (State.Actors.First_Element);
      end if;

      declare
         Frame : Athena.Encounters.Frames.Encounter_Frame;
      begin
         for Actor of State.Actors loop
            if Actor.Is_Active (State.Tick) then
               Athena.Encounters.Frames.Add_Sprite
                 (Frame, Actor.Current_Sprite);
            end if;
         end loop;
         Manager.Add_Frame (Frame);
      end;

      State.Tick := State.Tick + 1;

      if State.Countdown = 0
        and then not Have_Hostiles (State)
      then
         Log (State, "starting countdown");
         State.Countdown := 20;
      end if;

      if State.Countdown > 1 then
         State.Countdown := State.Countdown - 1;
      end if;

      if (State.Tick = Max_Ticks and then State.Countdown = 0)
        or else State.Countdown = 1
      then
         Log (State, "setting finished");
         State.Finished := True;
      end if;

   end Step;

end Athena.Encounters.Execution;
