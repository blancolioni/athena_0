with Athena.Identifiers;
with Athena.Money;

with Athena.Configure.Designs;

with Athena.Ships.Create;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.System_Fleets;
with Athena.Handles.System_Designs;

with Athena.Handles.Empire_Capital;
with Athena.Handles.Empire_Tec;
with Athena.Handles.Technology.Selections;

with Athena.Handles.Manager.Selections;
with Athena.Handles.Empire_Manager;

with Athena.Handles.Ship_Design;

with Athena.Handles.Fleet;

with Athena.Db;

package body Athena.Empires.Create is

   ----------------
   -- New_Empire --
   ----------------

   procedure New_Empire
     (Star      : Athena.Handles.Star.Star_Handle;
      Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : Athena.Color.Athena_Color)
   is
      Id             : constant Athena.Identifiers.Object_Identifier :=
                         Athena.Identifiers.Next_Identifier;
      RGB            : constant Natural :=
                         65536 * Natural (Color.Red * 255.0)
                         + 256 * Natural (Color.Green * 255.0)
                         + Natural (Color.Blue * 255.0);
      Empire         : constant Handles.Empire.Empire_Handle :=
                         Athena.Handles.Empire.Create
                           (Identifier => Id,
                            Name       => Name,
                            Plural     => Plural,
                            Adjective  => Adjective,
                            Cash       =>
                              Athena.Money.To_Money (100.0),
                            Debt       => Athena.Money.Zero,
                            Rgb        => RGB);

      Scouts         : constant Handles.Fleet.Fleet_Handle :=
                         Handles.Fleet.Create
                           (Identifier => Identifiers.Next_Identifier,
                            Name       => Adjective & " Scouts",
                            Empire     => Empire);
      Transports     : constant Handles.Fleet.Fleet_Handle :=
                         Handles.Fleet.Create
                           (Identifier => Identifiers.Next_Identifier,
                            Name       => Adjective & " Transports",
                            Empire     => Empire);
      Defenders     : constant Handles.Fleet.Fleet_Handle :=
                         Handles.Fleet.Create
                           (Identifier => Identifiers.Next_Identifier,
                            Name       => Adjective & " Defenders",
                            Empire     => Empire);
      Attackers     : constant Handles.Fleet.Fleet_Handle :=
                         Handles.Fleet.Create
                           (Identifier => Identifiers.Next_Identifier,
                            Name       => Adjective & " Attackers",
                            Empire     => Empire);

   begin

      Star.Update
        .Set_Owner (Empire.Reference_Empire)
        .Set_Name (Capital)
        .Done;

      Athena.Handles.Empire_Capital.Create
        (Empire => Empire,
         Star   => Star);

      Athena.Handles.Colony.Create
        (Star      => Star,
         Empire    => Empire,
         Construct => 0.0,
         Pop       => 1000.0,
         Colonists => 0.0,
         Industry  => 100.0,
         Material  => 100.0);

      Athena.Handles.System_Fleets.Create
        (Empire     => Empire,
         Scouts     => Scouts,
         Transports => Transports,
         Defenders  => Defenders,
         Attackers  => Attackers);

      for Tec of
        Athena.Handles.Technology.Selections.Select_All
      loop
         Athena.Handles.Empire_Tec.Create
           (Empire     => Empire,
            Technology => Tec,
            Investment => 0.0,
            Level      => 1.0);
      end loop;

      for Manager of
        Athena.Handles.Manager.Selections.Select_All
      loop
         Athena.Handles.Empire_Manager.Create
           (Empire  => Empire,
            Manager => Manager,
            Enabled => True,
            Fleet   => (if Manager.Tag = "explore" then Scouts
                        elsif Manager.Tag = "transport" then Transports
                        else Handles.Fleet.Get
                          (Athena.Db.Null_Fleet_Reference)));
      end loop;

      declare
         use Athena.Handles.Ship_Design;

         Scout_Design     : constant Ship_Design_Class :=
                              Athena.Configure.Designs.Load_Design
                                (Empire, "scout");
         Transport_Design : constant Ship_Design_Class :=
                              Athena.Configure.Designs.Load_Design
                                (Empire, "transport");
         Defender_Design  : constant Ship_Design_Class :=
                              Athena.Configure.Designs.Load_Design
                                (Empire, "defender");
         Destroyer_Design : constant Ship_Design_Class :=
                              Athena.Configure.Designs.Load_Design
                                (Empire, "destroyer");
      begin

         Athena.Ships.Create.Create_Ship
           (Empire => Empire,
            Star   => Star,
            Fleet  => Scouts,
            Design => Scout_Design,
            Name   => "Scout I");

         Athena.Ships.Create.Create_Ship
           (Empire => Empire,
            Star   => Star,
            Fleet  => Scouts,
            Design => Scout_Design,
            Name   => "Scout II");

         Athena.Ships.Create.Create_Ship
           (Empire => Empire,
            Star   => Star,
            Fleet  => Defenders,
            Design => Defender_Design,
            Name   => "Defender I");

         Athena.Ships.Create.Create_Ship
           (Empire => Empire,
            Star   => Star,
            Fleet  => Transports,
            Design => Transport_Design,
            Name   => "Transport I");

         Athena.Ships.Create.Create_Ship
           (Empire => Empire,
            Star   => Star,
            Fleet  => Attackers,
            Design => Destroyer_Design,
            Name   => "Destroyer I");

         Athena.Handles.System_Designs.Create
           (Empire     => Empire,
            Scout      => Scout_Design,
            Transport  => Transport_Design,
            Defender   => Defender_Design,
            Destroyer  => Destroyer_Design,
            Cruiser    =>
              Athena.Configure.Designs.Load_Design
                (Empire, "cruiser"),
            Battleship =>
              Athena.Configure.Designs.Load_Design
                (Empire, "battleship"),
            Carrier    =>
              Athena.Configure.Designs.Load_Design
                (Empire, "carrier"));

      end;

   end New_Empire;

end Athena.Empires.Create;
