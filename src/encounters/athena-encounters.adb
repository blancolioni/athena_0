with Ada.Containers.Doubly_Linked_Lists;

with WL.String_Maps;
with WL.String_Sets;

with Athena.Turns;

with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Random;

with Athena.Handles.Empire;
with Athena.Handles.Ship;
with Athena.Handles.Encounter_Action;

with Athena.Handles.Antagonist.Selections;
with Athena.Handles.Participant.Selections;
with Athena.Handles.Ship_Component;

with Athena.Handles.Encounter.Selections;
with Athena.Handles.Star.Selections;
with Athena.Handles.Turn.Selections;

with Athena.Db;

package body Athena.Encounters is

   Max_Ticks        : constant := 1_000;
   Encounter_Radius : constant := 1_000.0;

   type Weapon_Class is (Beam, Missile, Fighter);

   type Launch_Record is
      record
         From     : Athena.Handles.Ship.Ship_Handle;
         Weapon   : Weapon_Class;
         Target   : Athena.Handles.Ship.Ship_Handle;
         X, Y     : Real;
         Heading  : Real;
         Speed    : Non_Negative_Real;
      end record;

   package Launch_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Launch_Record);

   type Weapon_Record is
      record
         Class       : Weapon_Class;
         Component   : Athena.Handles.Ship_Component.Ship_Component_Handle;
         Charge      : Non_Negative_Real;
         Charge_Rate : Non_Negative_Real;
         Condition   : Unit_Real;
         Remaining   : Natural;
      end record;

   package Weapon_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Weapon_Record);

   type Shield_Record is
      record
         Component : Athena.Handles.Ship_Component.Ship_Component_Handle;
         Max_Level : Non_Negative_Real;
         Level     : Non_Negative_Real;
         Restore   : Non_Negative_Real;
         Condition : Unit_Real;
      end record;

   type Ship_Record is
      record
         Handle      : Athena.Handles.Ship.Ship_Handle;
         Participant : Athena.Handles.Participant.Participant_Handle;
         X, Y        : Real;
         Heading     : Real;
         Shield      : Shield_Record;
         Weapons     : Weapon_Lists.List;
      end record;

   package Ship_Record_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Record);

   type Team_Record is
      record
         Empire : Athena.Handles.Empire.Empire_Handle;
         Ships  : Athena.Ships.Ship_Lists.List;
      end record;

   package Team_Maps is
     new WL.String_Maps (Team_Record);

   type Battle_Record is
      record
         Teams    : Team_Maps.Map;
         Ships    : Ship_Record_Lists.List;
         Launches : Launch_Lists.List;
         Tick     : Natural;
         Victor   : Athena.Handles.Empire.Empire_Handle;
      end record;

   procedure Initialize
     (Battle : in out Battle_Record;
      From   : Athena.Handles.Encounter.Encounter_Class);

   function Complete
     (Battle : Battle_Record)
      return Boolean;

   procedure Step
     (Battle : in out Battle_Record);

   --------------
   -- Complete --
   --------------

   function Complete
     (Battle : Battle_Record)
      return Boolean
   is
   begin
      return Battle.Tick > Max_Ticks
        or else (for some Team of Battle.Teams =>
                   Team.Ships.Is_Empty);
   end Complete;

   ------------
   -- Create --
   ------------

   function Create
     (Star  : Athena.Handles.Star.Star_Class;
      Ships : Athena.Ships.Ship_Lists.List)
      return Athena.Handles.Encounter.Encounter_Class
   is
      Encounter   : constant Athena.Handles.Encounter.Encounter_Handle :=
                      Athena.Handles.Encounter.Create
                        (Turn => Athena.Turns.Current_Turn,
                         Star => Star);
      Antagonists : WL.String_Sets.Set;
   begin
      for Ship of Ships loop
         Athena.Handles.Participant.Create
           (Encounter => Encounter,
            Ship      => Ship);
         if not Antagonists.Contains (Ship.Empire.Identifier) then
            Athena.Handles.Antagonist.Create
              (Encounter => Encounter,
               Empire    => Ship.Empire);
            Antagonists.Include (Ship.Empire.Identifier);
         end if;
      end loop;
      return Encounter;
   end Create;

   -------------
   -- Execute --
   -------------

   procedure Execute (Encounter : Athena.Handles.Encounter.Encounter_Class) is
      Battle : Battle_Record;
   begin
      Athena.Logging.Log
        ("Executing the Battle of " & Encounter.Star.Name);

      Initialize (Battle, Encounter);

      while not Complete (Battle) loop
         Step (Battle);
      end loop;

      if Battle.Victor.Has_Element then
         Athena.Logging.Log (Battle.Victor.Name
                             & " won the Battle of " & Encounter.Star.Name);
      else
         Athena.Logging.Log ("The battle continues ...");
      end if;

   end Execute;

   ----------
   -- Find --
   ----------

   function Find
     (Star_Name   : String;
      Turn_Number : Positive)
      return Athena.Handles.Encounter.Encounter_Class
   is
      use type Athena.Handles.Star.Selections.Selection_Condition;
      use type Athena.Handles.Turn.Selections.Selection_Condition;
      Encounter_Star : constant Athena.Handles.Star.Star_Class :=
                         Athena.Handles.Star.Selections.First_Where
                           (Athena.Handles.Star.Selections.Name = Star_Name);
      Encounter_Turn : constant Athena.Handles.Turn.Turn_Class :=
                         Athena.Handles.Turn.Selections.First_Where
                           (Athena.Handles.Turn.Selections.Turn_Number
                            = Turn_Number);
   begin
      if Encounter_Star.Has_Element
        and then Encounter_Turn.Has_Element
      then
         declare
            use Athena.Handles.Encounter.Selections;
         begin
            return First_Where
              (Star = Encounter_Star and Turn = Encounter_Turn);
         end;
      else
         return Athena.Handles.Encounter.Empty_Handle;
      end if;
   end Find;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Battle : in out Battle_Record;
      From   : Athena.Handles.Encounter.Encounter_Class)
   is

      procedure Deploy (Team : Team_Record;
                        Angle : Real);

      function Initial_Ship
        (Handle  : Athena.Handles.Ship.Ship_Handle;
         X, Y    : Real;
         Heading : Real)
         return Ship_Record;

      package Participant_Maps is
        new WL.String_Maps (Athena.Handles.Participant.Participant_Class,
                            Athena.Handles.Participant."=");

      Participants : Participant_Maps.Map;

      ------------
      -- Deploy --
      ------------

      procedure Deploy (Team  : Team_Record;
                        Angle : Real)
      is
         use Athena.Elementary_Functions;
         use Athena.Random;
         Ships : constant Athena.Ships.Ship_Lists.List := Team.Ships;
         X     : constant Real := Encounter_Radius * Cos (Angle, 360.0);
         Y     : constant Real := Encounter_Radius * Sin (Angle, 360.0);
      begin
         for Ship of Ships loop
            Battle.Ships.Append
              (Initial_Ship
                 (Handle => Athena.Handles.Ship.Get (Ship.Reference_Ship),
                  X      => X + Normal_Random (Encounter_Radius / 10.0),
                  Y      => Y + Normal_Random (Encounter_Radius / 10.0),
                  Heading => Angle + 180.0));
         end loop;
      end Deploy;

      ------------------
      -- Initial_Ship --
      ------------------

      function Initial_Ship
        (Handle  : Athena.Handles.Ship.Ship_Handle;
         X, Y    : Real;
         Heading : Real)
         return Ship_Record
      is

         function Initial_Shield return Shield_Record;
         function Initial_Weapons return Weapon_Lists.List;

         --------------------
         -- Initial_Shield --
         --------------------

         function Initial_Shield return Shield_Record is
            Shield : constant Athena.Handles.Ship_Component
              .Ship_Component_Class := Athena.Ships.Get_Shield (Handle);
         begin
            if Shield.Has_Element then
               return Shield_Record'
                 (Component =>
                    Athena.Handles.Ship_Component.Get
                      (Shield.Reference_Ship_Component),
                  Max_Level =>
                    Shield.Design_Component.Mass * Shield.Condition,
                  Level     =>
                    Shield.Design_Component.Mass * Shield.Condition,
                  Restore   =>
                    Shield.Design_Component.Mass * Shield.Condition * 0.02,
                  Condition => Shield.Condition);
            else
               return Shield_Record'
                 (Component => Athena.Handles.Ship_Component.Empty_Handle,
                  Max_Level => 0.0,
                  Level     => 0.0,
                  Restore   => 0.0,
                  Condition => 0.0);
            end if;
         end Initial_Shield;

         ---------------------
         -- Initial_Weapons --
         ---------------------

         function Initial_Weapons return Weapon_Lists.List is

            List : Weapon_Lists.List;

            procedure Add_Weapon
              (Weapon : Athena.Handles.Ship_Component.Ship_Component_Class);

            ----------------
            -- Add_Weapon --
            ----------------

            procedure Add_Weapon
              (Weapon : Athena.Handles.Ship_Component.Ship_Component_Class)
            is
               Class : constant Weapon_Class :=
                         (case Weapon.Component.Class is
                             when Athena.Db.Beam => Beam,
                             when Athena.Db.Missile => Missile,
                             when Athena.Db.Fighter => Fighter,
                             when others            =>
                                raise Constraint_Error with
                          Weapon.Component.Class'Image &
                            " is not a known weapon class");

               Condition : constant Unit_Real :=
                             Weapon.Condition;
               Rec : constant Weapon_Record :=
                       Weapon_Record'
                         (Class       => Class,
                          Component   =>
                            Athena.Handles.Ship_Component.Get
                              (Weapon.Reference_Ship_Component),
                          Charge      =>
                            Weapon.Design_Component.Mass * Condition,
                          Charge_Rate =>
                            Weapon.Design_Component.Mass * Condition * 0.02,
                          Condition   => Condition,
                          Remaining   => 100);
            begin
               List.Append (Rec);
            end Add_Weapon;

         begin
            Athena.Ships.Iterate_Weapons
              (Handle, Add_Weapon'Access);
            return List;
         end Initial_Weapons;

      begin
         return Ship : constant Ship_Record :=
           Ship_Record'
             (Handle  => Handle,
              Participant =>
                Athena.Handles.Participant.Get
                  (Participants.Element
                     (Handle.Identifier).Reference_Participant),
              X       => X,
              Y       => Y,
              Heading => Heading,
              Shield  => Initial_Shield,
              Weapons => Initial_Weapons)
         do
            Athena.Handles.Encounter_Action.Create
              (Encounter   => From,
               Participant => Ship.Participant,
               Time_Stamp  => 0,
               X           => X,
               Y           => Y,
               Heading     => Heading,
               Shield      =>
                 (if Initial_Shield.Max_Level > 0.0
                  then Initial_Shield.Level / Initial_Shield.Max_Level
                  else 0.0),
               Action      => Athena.Db.No_Action,
               Component   => Athena.Handles.Ship_Component.Empty_Handle,
               Target      => Athena.Handles.Participant.Empty_Handle);
         end return;
      end Initial_Ship;

   begin
      Battle :=
        Battle_Record'
          (Teams    => Team_Maps.Empty_Map,
           Ships    => Ship_Record_Lists.Empty_List,
           Launches => Launch_Lists.Empty_List,
           Tick     => 0,
           Victor   => Athena.Handles.Empire.Empty_Handle);

      declare
         use Athena.Handles.Antagonist.Selections;
      begin
         for Antagonist of Select_Where (Encounter = From) loop
            Battle.Teams.Insert
              (Antagonist.Empire.Identifier,
               (Athena.Handles.Empire.Get
                    (Antagonist.Empire.Reference_Empire),
                Athena.Ships.Ship_Lists.Empty_List));
         end loop;
      end;

      declare
         use Athena.Handles.Participant.Selections;
      begin
         for Participant of Select_Where (Encounter = From) loop
            declare
               Handle : constant Athena.Handles.Ship.Ship_Handle :=
                          Athena.Handles.Ship.Get
                            (Participant.Ship.Reference_Ship);
            begin
               Battle.Teams (Participant.Ship.Empire.Identifier)
                 .Ships.Append (Handle);
               Participants.Insert (Handle.Identifier, Participant);
            end;
         end loop;
      end;

      declare
         Angular_Sep : constant Real := 360.0 / Real (Battle.Teams.Length);
         Current_Angle : Real := 0.0;
      begin
         for Team of Battle.Teams loop
            Athena.Logging.Log ("   team: " & Team.Empire.Name);
            for Ship of Team.Ships loop
               Athena.Logging.Log ("        " & Ship.Name);
            end loop;
            Deploy (Team, Current_Angle);
            Current_Angle := Current_Angle + Angular_Sep;
         end loop;
      end;

   end Initialize;

   ----------
   -- Step --
   ----------

   procedure Step
     (Battle : in out Battle_Record)
   is
   begin
      Battle.Tick := Battle.Tick + 1;
   end Step;

end Athena.Encounters;
