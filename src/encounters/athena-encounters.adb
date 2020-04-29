with WL.String_Maps;
with WL.String_Sets;

with Athena.Turns;

with Athena.Logging;

with Athena.Handles.Empire;
with Athena.Handles.Ship;

with Athena.Handles.Antagonist.Selections;
with Athena.Handles.Participant.Selections;

package body Athena.Encounters is

   type Team_Record is
      record
         Empire : Athena.Handles.Empire.Empire_Handle;
         Ships  : Athena.Ships.Ship_Lists.List;
      end record;

   package Team_Maps is
     new WL.String_Maps (Team_Record);

   type Battle_Record is
      record
         Teams : Team_Maps.Map;
         Ships : Athena.Ships.Ship_Lists.List;
      end record;

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

   procedure Execute (E : Athena.Handles.Encounter.Encounter_Class) is
      Battle : Battle_Record :=
                 Battle_Record'
                   (Teams => Team_Maps.Empty_Map,
                    Ships => Athena.Ships.Ship_Lists.Empty_List);
   begin
      Athena.Logging.Log
        ("Executing the Battle of " & E.Star.Name);

      declare
         use Athena.Handles.Antagonist.Selections;
      begin
         for Antagonist of Select_Where (Encounter = E) loop
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
         for Participant of Select_Where (Encounter = E) loop
            declare
               Handle : constant Athena.Handles.Ship.Ship_Handle :=
                          Athena.Handles.Ship.Get
                            (Participant.Ship.Reference_Ship);
            begin
               Battle.Ships.Append (Handle);
               Battle.Teams (Participant.Ship.Empire.Identifier)
                 .Ships.Append (Handle);
            end;
         end loop;
      end;

      for Team of Battle.Teams loop
         Athena.Logging.Log ("   team: " & Team.Empire.Name);
         for Ship of Team.Ships loop
            Athena.Logging.Log ("        " & Ship.Name);
         end loop;
      end loop;

   end Execute;

end Athena.Encounters;
