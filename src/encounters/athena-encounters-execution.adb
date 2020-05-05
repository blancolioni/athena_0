with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

with WL.String_Maps;

with Athena.Logging;
with Athena.Random;

with Athena.Encounters.Scripts;
with Athena.Encounters.Situation;

with Athena.Ships.Scripts;

with Athena.Handles.Antagonist.Selections;
with Athena.Handles.Participant.Selections;

with Athena.Handles.Empire;
with Athena.Handles.Participant;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Component;

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
         Tick      : Natural;
         Victor    : Athena.Handles.Empire.Empire_Handle;
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

   type Situation_Type is
     new Athena.Encounters.Situation.Situation_Interface with
      record
         State : State_Type;
         Actor : Athena.Encounters.Actors.Actor_Type;
      end record;

   overriding function Origin
     (Situation : Situation_Type)
      return Encounter_Point
   is (Situation.Actor.Location);

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
        procedure (Hostile : Athena.Encounters.Situation.Situation_Actor));

   overriding procedure Fire_Weapon
     (Situation : in out Situation_Type;
      Weapon    : Athena.Handles.Ship_Component.Ship_Component_Class;
      Target    : Positive)
   is null;

   procedure Iterate_Actors
     (Situation : Situation_Type'Class;
      Test      : not null access
        function (Actor : Athena.Encounters.Actors.Actor_Type)
        return Boolean;
      Process   : not null access
        procedure (Actor : Athena.Encounters.Situation.Situation_Actor));

   ---------
   -- Get --
   ---------

   overriding function Get
     (Situation : Situation_Type;
      Actor     : Encounter_Actor_Reference)
      return Athena.Encounters.Situation.Situation_Actor
   is
   begin
      return Situation.State.Actors.Element (Actor)
        .Current_Situation (Situation);
   end Get;

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
        procedure (Hostile : Athena.Encounters.Situation.Situation_Actor))
   is
      function Is_Ally
        (Actor : Athena.Encounters.Actors.Actor_Type)
         return Boolean
      is (Actor.Owner.Identifier = Situation.Actor.Owner.Identifier);
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
      is (Actor.Owner.Identifier /= Situation.Actor.Owner.Identifier);
   begin
      Situation.Iterate_Actors (Is_Hostile'Access, Process);
   end Iterate_Hostiles;

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
                   Victor    => Athena.Handles.Empire.Empty_Handle);

      procedure Deploy
        (Team    : Team_Record;
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
         Bearing : Athena.Trigonometry.Angle)
      is
         use Athena.Trigonometry;
         use Athena.Random;
         Ships : constant Athena.Ships.Ship_Lists.List := Team.Ships;
         X     : constant Real := Encounter_Radius * Cos (Bearing);
         Y     : constant Real := Encounter_Radius * Sin (Bearing);
      begin
         for Ship of Ships loop
            State.Actors.Append
              (Athena.Encounters.Actors.Create_Ship_Actor
                 (Index   => State.Actors.Last_Index + 1,
                  Ship    => Ship,
                  X       => X + Normal_Random (Encounter_Radius / 10.0),
                  Y       => Y + Normal_Random (Encounter_Radius / 10.0),
                  Heading => Bearing + From_Degrees (180.0)));
            State.Scripts.Append
              (Athena.Ships.Scripts.Get_Script
                 (Ship.Script));
            Manager.Add_Actor (State.Actors.Last_Element);
         end loop;
      end Deploy;

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
            begin
               State.Teams (Participant.Ship.Empire.Identifier)
                 .Ships.Append (Handle);
               Participants.Insert (Handle.Identifier, Participant);
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
                  & "; speed " & Image (Athena.Ships.Speed (Ship)));
            end loop;
            Deploy (Team, Current_Angle);
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
         declare
            Situation : Situation_Type :=
                          Situation_Type'
                            (State => State,
                             Actor => State.Actors (Index));
         begin
            State.Scripts.Element (Index).Update
              (State.Actors (Index), Situation);
         end;
      end loop;

      for Actor of State.Actors loop
         Actor.Update;
      end loop;

      if False then
         Log (State.Actors.First_Element);
      end if;

      declare
         Frame : Athena.Encounters.Frames.Encounter_Frame;
      begin
         for Actor of State.Actors loop
            Athena.Encounters.Frames.Add_Sprite
              (Frame, Actor.Current_Sprite);
         end loop;
         Manager.Add_Frame (Frame);
      end;

      State.Tick := State.Tick + 1;
      if State.Tick = Max_Ticks then
         State.Finished := True;
      end if;
   end Step;

end Athena.Encounters.Execution;
