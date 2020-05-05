with Ada.Containers.Indefinite_Holders;
with Ada.Numerics;

with WL.String_Maps;
with WL.String_Sets;

with Athena.Turns;

with Athena.Elementary_Functions;
with Athena.Trigonometry;

with Athena.Logging;
with Athena.Random;
with Athena.Real_Images;

with Athena.Ships.Scripts;

with Athena.Handles.Empire;
with Athena.Handles.Encounter_Action;

with Athena.Handles.Antagonist.Selections;
with Athena.Handles.Participant.Selections;

with Athena.Handles.Encounter.Selections;
with Athena.Handles.Star.Selections;
with Athena.Handles.Turn.Selections;

with Athena.Db;

package body Athena.Encounters is

   Max_Ticks        : constant := 1_000;
   Encounter_Radius : constant := 1_000.0;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   type Weapon_Class is (Beam, Missile, Fighter);

   type Launch_Record is
      record
         From     : Athena.Handles.Ship.Ship_Handle;
         Weapon   : Weapon_Class;
         Target   : Athena.Handles.Ship.Ship_Handle;
         X, Y     : Real;
         Heading  : Athena.Trigonometry.Angle;
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

   package Script_Holders is
     new Ada.Containers.Indefinite_Holders
       (Encounter_Script_Interface'Class);

   type Damage_Record is
      record
         Shield_Damage : Non_Negative_Real;
      end record;

   package Damage_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Damage_Record);

   type Ship_Record is
      record
         Handle      : Athena.Handles.Ship.Ship_Handle;
         Participant : Athena.Handles.Participant.Participant_Handle;
         Mass        : Non_Negative_Real;
         X, Y        : Real;
         Heading     : Athena.Trigonometry.Angle;
         Speed       : Non_Negative_Real;
         Shield      : Shield_Record;
         Condition   : Unit_Real;
         Weapons     : Weapon_Lists.List;
         Script      : Script_Holders.Holder;
         Orders      : Encounter_Order_List;
         Damage      : Damage_Lists.List;
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

   type Battle_Record is new Encounter_Interface with
      record
         Encounter : Athena.Handles.Encounter.Encounter_Handle;
         Teams     : Team_Maps.Map;
         Ships     : Ship_Record_Lists.List;
         Launches  : Launch_Lists.List;
         Tick      : Natural;
         Victor    : Athena.Handles.Empire.Empire_Handle;
         Actor     : Ship_Record_Lists.Cursor;
      end record;

   overriding procedure Get_State
     (Battle    : Battle_Record;
      Condition : out Unit_Real;
      Shields   : out Unit_Real);

   overriding procedure Iterate_Weapons
     (Battle    : Battle_Record;
      Process   : not null access
        procedure (Index : Positive;
                   Weapon : Athena.Handles.Ship_Component.Ship_Component_Class;
                   Charge : Unit_Real;
                   Remaining : Natural));

   overriding procedure Iterate_Hostiles
     (Battle    : Battle_Record;
      Process   : not null access
        procedure (Index   : Positive;
                   Mass    : Non_Negative_Real;
                   DX, DY  : Real;
                   Heading : Real;
                   Speed   : Real));

   overriding procedure Iterate_Allies
     (Battle    : Battle_Record;
      Process   : not null access
        procedure (Ally    : Athena.Handles.Ship.Ship_Class;
                   DX, DY  : Real;
                   Heading : Real;
                   Speed   : Real));

   procedure Initialize
     (Battle : in out Battle_Record'Class;
      From   : Athena.Handles.Encounter.Encounter_Class);

   function Complete
     (Battle : Battle_Record'Class)
      return Boolean;

   procedure Step
     (Battle : in out Battle_Record'Class);

   procedure Execute_Orders
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record);

   procedure Move
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record);

   procedure Write_Action
     (Battle : Battle_Record'Class;
      Ship   : Ship_Record);

   procedure Apply_Damage
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record);

   function Get_Weapon
     (Battle : Battle_Record'Class;
      Ship   : Ship_Record;
      Index  : Positive)
      return Weapon_Lists.Cursor;

   function Get_Hostile
     (Battle : Battle_Record'Class;
      Ship   : Ship_Record;
      Index  : Positive)
      return Ship_Record_Lists.Cursor;

   procedure Fire_Weapon
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record;
      Weapon : in out Weapon_Record;
      Target : in out Ship_Record);

   ---------
   -- Add --
   ---------

   procedure Add
     (To    : in out Encounter_Order_List;
      Order : Encounter_Order)
   is
   begin
      To.List.Append (Order);
   end Add;

   ------------------
   -- Apply_Damage --
   ------------------

   procedure Apply_Damage
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record)
   is
      pragma Unreferenced (Battle);
   begin
      for Damage of Ship.Damage loop
         declare
            Absorbed  : constant Non_Negative_Real :=
                          Real'Min
                            (Ship.Shield.Level,
                             Damage.Shield_Damage *
                               (if Ship.Shield.Max_Level > 0.0
                                then Ship.Shield.Level / Ship.Shield.Max_Level
                                else 0.0));
         begin
            Ship.Shield.Level := Ship.Shield.Level - Absorbed;
         end;
      end loop;
   end Apply_Damage;

   --------------
   -- Complete --
   --------------

   function Complete
     (Battle : Battle_Record'Class)
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

   --------------------
   -- Execute_Orders --
   --------------------

   procedure Execute_Orders
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record)
   is
   begin
      for Order of Ship.Orders.List loop
         case Order.Class is
            when Set_Destination =>
               declare
                  use Athena.Trigonometry;
                  Speed : constant Non_Negative_Real :=
                            Athena.Ships.Speed
                              (Ship.Handle);
                  DX       : constant Real := Ship.X - Order.Target_X;
                  DY       : constant Real := Ship.Y - Order.Target_Y;
                  Distance : constant Non_Negative_Real :=
                               Athena.Elementary_Functions.Sqrt
                                 (DX ** 2 + DY ** 2);
                  Bearing  : constant Athena.Trigonometry.Angle :=
                               Athena.Trigonometry.Arctan
                                 (DY, DX)
                                 - Ship.Heading;
                  Turn     : constant Non_Negative_Real := Speed / 10.0;
                  Accel    : constant Non_Negative_Real := Speed / 20.0;
                  Stop_Time : constant Non_Negative_Real :=
                                (if Ship.Speed = 0.0
                                 then 0.0
                                 else Ship.Speed / Accel);
                  Stop_Dist : constant Non_Negative_Real :=
                                Ship.Speed * Stop_Time
                                  + 0.5 * Accel * Stop_Time ** 2;
               begin
                  if Bearing /= From_Degrees (0.0) then
                     if abs Bearing < From_Degrees (Turn) then
                        Ship.Heading := Bearing;
                     else
                        Ship.Heading := Ship.Heading
                          + Bearing *
                          (Turn / To_Degrees (abs Bearing));
                     end if;
                  end if;

                  if Stop_Dist >= Distance then
                     Ship.Speed := Real'Max (Ship.Speed - Accel, 0.0);
                  else
                     Ship.Speed := Ship.Speed + Accel;
                  end if;
               end;

            when Fire_Weapon =>
               declare
                  Weapon : Weapon_Record renames
                             Ship.Weapons
                               (Battle.Get_Weapon (Ship, Order.Weapon));
                  Target : Ship_Record renames
                             Battle.Ships
                               (Battle.Get_Hostile (Ship, Order.Target));
               begin
                  Battle.Fire_Weapon (Ship, Weapon, Target);
               end;

            when Jump =>
               null;
         end case;
      end loop;
   end Execute_Orders;

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

   -----------------
   -- Fire_Weapon --
   -----------------

   procedure Fire_Weapon
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record;
      Weapon : in out Weapon_Record;
      Target : in out Ship_Record)
   is
      pragma Unreferenced (Battle);
      use Athena.Elementary_Functions;
      Pi : constant := Ada.Numerics.Pi;
      DX : constant Real := Ship.X - Target.X;
      DY : constant Real := Ship.Y - Target.Y;
      Distance : constant Non_Negative_Real :=
                   Sqrt (DX ** 2 + DY ** 2);
   begin
      case Weapon.Class is
         when Beam =>
            declare
               Mass      : constant Non_Negative_Real :=
                             Weapon.Component.Design_Component.Mass;
               Tec_Level : constant Non_Negative_Real :=
                             Weapon.Component.Tec_Level;
               Condition : constant Unit_Real :=
                             Weapon.Condition;
               Max_Range : constant Non_Negative_Real :=
                             Athena.Elementary_Functions.Sqrt (Mass)
                             * 10.0 * Tec_Level;
               Power     : constant Non_Negative_Real :=
                             Mass * Condition * Tec_Level
                               * (1.0 - Distance / Max_Range);
               Target_R  : constant Non_Negative_Real :=
                             (3.0 * Mass / 4.0 / Pi) ** (1.0 / 3.0);
               Target_A  : constant Non_Negative_Real :=
                             Pi * Target_R ** 2;
               P_Hit     : constant Non_Negative_Real :=
                             (1.0 + Ship.Handle.Experience)
                             * Target_A / Distance
                             / (1.0 + Target.Handle.Experience);
               Hit       : constant Boolean :=
                             Athena.Random.Unit_Random <= P_Hit;
            begin
               Athena.Logging.Log
                 (Ship.Handle.Name
                  & " fires at "
                  & Target.Handle.Name
                  & ": mass " & Image (Mass)
                  & "; tec " & Image (Tec_Level)
                  & "; exp " & Image (Ship.Handle.Experience)
                  & "; condition " & Image (Condition)
                  & "; max range " & Image (Max_Range)
                  & "; range " & Image (Distance)
                  & "; power " & Image (Power)
                  & "; target mass " & Image (Target.Mass)
                  & "; target area " & Image (Target_A)
                  & "; target exp " & Image (Target.Handle.Experience)
                  & "; chance " & Image (P_Hit * 100.0) & "%"
                  & "; result " & (if Hit then "HIT" else "miss"));

               Weapon.Charge := 0.0;
               if Hit then
                  Target.Damage.Append
                    (Damage_Record'
                       (Shield_Damage => Power));
               end if;
            end;

         when Missile =>
            null;

         when Fighter =>
            null;
      end case;

   end Fire_Weapon;

   -----------------
   -- Get_Hostile --
   -----------------

   function Get_Hostile
     (Battle : Battle_Record'Class;
      Ship   : Ship_Record;
      Index  : Positive)
      return Ship_Record_Lists.Cursor
   is
      Hostile_Index : Natural := 0;
   begin
      for Position in Battle.Ships.Iterate loop
         if Battle.Ships (Position).Handle.Empire.Identifier
           /= Ship.Handle.Empire.Identifier
         then
            Hostile_Index := Hostile_Index + 1;
            if Hostile_Index = Index then
               return Position;
            end if;
         end if;
      end loop;
      return Ship_Record_Lists.No_Element;
   end Get_Hostile;

   ---------------
   -- Get_State --
   ---------------

   overriding procedure Get_State
     (Battle    : Battle_Record;
      Condition : out Unit_Real;
      Shields   : out Unit_Real)
   is
      Shield : constant Shield_Record :=
                 Ship_Record_Lists.Element (Battle.Actor).Shield;
   begin
      Shields := (if Shield.Max_Level = 0.0 then 0.0
                  else Shield.Level / Shield.Max_Level);
      Condition := Ship_Record_Lists.Element (Battle.Actor).Condition;
   end Get_State;

   ----------------
   -- Get_Weapon --
   ----------------

   function Get_Weapon
     (Battle : Battle_Record'Class;
      Ship   : Ship_Record;
      Index  : Positive)
      return Weapon_Lists.Cursor
   is
      pragma Unreferenced (Battle);
      Weapon_Index : Natural := 0;
   begin
      for Position in
        Ship.Weapons.Iterate
      loop
         Weapon_Index := Weapon_Index + 1;
         if Weapon_Index = Index then
            return Position;
         end if;
      end loop;
      return Weapon_Lists.No_Element;
   end Get_Weapon;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Battle : in out Battle_Record'Class;
      From   : Athena.Handles.Encounter.Encounter_Class)
   is

      procedure Deploy
        (Team    : Team_Record;
         Bearing : Athena.Trigonometry.Angle);

      function Initial_Ship
        (Handle  : Athena.Handles.Ship.Ship_Handle;
         X, Y    : Real;
         Heading : Athena.Trigonometry.Angle)
         return Ship_Record;

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
            Battle.Ships.Append
              (Initial_Ship
                 (Handle => Athena.Handles.Ship.Get (Ship.Reference_Ship),
                  X      => X + Normal_Random (Encounter_Radius / 10.0),
                  Y      => Y + Normal_Random (Encounter_Radius / 10.0),
                  Heading => Bearing + From_Degrees (180.0)));
         end loop;
      end Deploy;

      ------------------
      -- Initial_Ship --
      ------------------

      function Initial_Ship
        (Handle  : Athena.Handles.Ship.Ship_Handle;
         X, Y    : Real;
         Heading : Athena.Trigonometry.Angle)
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
             (Handle      => Handle,
              Participant =>
                Athena.Handles.Participant.Get
                  (Participants.Element
                     (Handle.Identifier).Reference_Participant),
              Mass        => Athena.Ships.Mass (Handle),
              X           => X,
              Y           => Y,
              Heading     => Heading,
              Speed       => 0.0,
              Shield      => Initial_Shield,
              Condition   => 1.0,
              Weapons     => Initial_Weapons,
              Script      =>
                Script_Holders.To_Holder
                  (Athena.Ships.Scripts.Get_Script
                     (Handle.Script)),
              Orders      => <>,
              Damage      => <>)
         do
            Athena.Handles.Encounter_Action.Create
              (Encounter   => From,
               Participant => Ship.Participant,
               Time_Stamp  => 0,
               X           => X,
               Y           => Y,
               Heading     => Athena.Trigonometry.To_Degrees (Heading),
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
      Battle_Record (Battle) :=
        Battle_Record'
          (Encounter =>
             Athena.Handles.Encounter.Get (From.Reference_Encounter),
           Teams     => Team_Maps.Empty_Map,
           Ships     => Ship_Record_Lists.Empty_List,
           Launches  => Launch_Lists.Empty_List,
           Tick      => 0,
           Victor    => Athena.Handles.Empire.Empty_Handle,
           Actor     => Ship_Record_Lists.No_Element);

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
         use Athena.Trigonometry;
         Angular_Sep : constant Angle :=
                         From_Degrees (360.0 / Real (Battle.Teams.Length));
         Current_Angle : Angle := From_Degrees (0.0);
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

   --------------------
   -- Iterate_Allies --
   --------------------

   overriding procedure Iterate_Allies
     (Battle    : Battle_Record;
      Process   : not null access
        procedure (Ally    : Athena.Handles.Ship.Ship_Class;
                   DX, DY  : Real;
                   Heading : Real;
                   Speed   : Real))
   is
      Index : Natural := 0;
      Actor : constant Ship_Record :=
                Ship_Record_Lists.Element (Battle.Actor);
   begin
      for Ship of Battle.Ships loop
         if Ship.Handle.Identifier /= Actor.Handle.Identifier
           and then Ship.Handle.Empire.Identifier
             = Actor.Handle.Empire.Identifier
         then
            Index := Index + 1;
            Process (Ship.Handle,
                     Ship.X - Actor.X,
                     Ship.Y - Actor.Y,
                     Athena.Trigonometry.To_Degrees (Ship.Heading),
                     Ship.Speed);
         end if;
      end loop;
   end Iterate_Allies;

   ----------------------
   -- Iterate_Hostiles --
   ----------------------

   overriding procedure Iterate_Hostiles
     (Battle    : Battle_Record;
      Process   : not null access
        procedure (Index   : Positive;
                   Mass    : Non_Negative_Real;
                   DX, DY  : Real;
                   Heading : Real;
                   Speed   : Real))
   is
      Index : Natural := 0;
      Actor : constant Ship_Record :=
                Ship_Record_Lists.Element (Battle.Actor);
   begin
      for Ship of Battle.Ships loop
         if Ship.Handle.Empire.Identifier
           /= Actor.Handle.Empire.Identifier
         then
            Index := Index + 1;
            Process (Index, Ship.Mass,
                     Ship.X - Actor.X,
                     Ship.Y - Actor.Y,
                     Athena.Trigonometry.To_Degrees (Ship.Heading),
                     Ship.Speed);
         end if;
      end loop;
   end Iterate_Hostiles;

   ---------------------
   -- Iterate_Weapons --
   ---------------------

   overriding procedure Iterate_Weapons
     (Battle    : Battle_Record;
      Process   : not null access
        procedure (Index : Positive;
                   Weapon : Athena.Handles.Ship_Component.Ship_Component_Class;
                   Charge : Unit_Real;
                   Remaining : Natural))
   is
      Index : Natural := 0;
   begin
      for Weapon of
        Battle.Ships (Battle.Actor).Weapons
      loop
         Index := Index + 1;
         Process (Index, Weapon.Component, Weapon.Charge, Weapon.Remaining);
      end loop;
   end Iterate_Weapons;

   ----------
   -- Move --
   ----------

   procedure Move
     (Battle : in out Battle_Record'Class;
      Ship   : in out Ship_Record)
   is
      pragma Unreferenced (Battle);
   begin
      Ship.X := Ship.X + Ship.Speed * Athena.Trigonometry.Cos (Ship.Heading);
      Ship.Y := Ship.Y + Ship.Speed * Athena.Trigonometry.Sin (Ship.Heading);
   end Move;

   ----------
   -- Step --
   ----------

   procedure Step
     (Battle : in out Battle_Record'Class)
   is
   begin
      for Position in Battle.Ships.Iterate loop
         Battle.Actor := Position;

         declare
            Orders : Encounter_Order_List;
         begin
            Ship_Record_Lists.Element (Position)
              .Script.Element.Get_Orders
                (Battle, Orders);
            Battle.Ships.Reference (Position).Orders := Orders;
         end;
      end loop;

      for Item of Battle.Ships loop
         Battle.Execute_Orders (Item);
      end loop;

      for Item of Battle.Ships loop
         Battle.Move (Item);
      end loop;

      for Item of Battle.Ships loop
         Battle.Apply_Damage (Item);
      end loop;

      for Item of Battle.Ships loop
         Battle.Write_Action (Item);
      end loop;

      Battle.Tick := Battle.Tick + 1;
   end Step;

   ------------------
   -- Write_Action --
   ------------------

   procedure Write_Action (Battle : Battle_Record'Class;
                           Ship   : Ship_Record) is
   begin
      Athena.Handles.Encounter_Action.Create
        (Encounter   => Battle.Encounter,
         Participant => Ship.Participant,
         Time_Stamp  => Battle.Tick,
         X           => Ship.X,
         Y           => Ship.Y,
         Heading     => Athena.Trigonometry.To_Degrees (Ship.Heading),
         Shield      =>
           (if Ship.Shield.Max_Level > 0.0
            then Ship.Shield.Level / Ship.Shield.Max_Level
            else 0.0),
         Action      => Athena.Db.No_Action,
         Component   => Athena.Handles.Ship_Component.Empty_Handle,
         Target      => Athena.Handles.Participant.Empty_Handle);
   end Write_Action;

end Athena.Encounters;
