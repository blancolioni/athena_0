with Athena.Identifiers;
with Athena.Money;

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

with Athena.Handles.Component.Selections;
with Athena.Handles.Ship_Design;
with Athena.Handles.Design_Component;

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
         Attackers  => Athena.Handles.Fleet.Empty_Handle);

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

         function Component
           (T : String)
                        return Athena.Handles.Component.Component_Handle;

         ---------------
         -- Component --
         ---------------

         function Component
           (T : String)
                        return Athena.Handles.Component.Component_Handle
         is
            use Athena.Handles.Component.Selections;
         begin
            return First_Where (Tag = T);
         end Component;

         Scout_Id : constant String :=
                      Athena.Identifiers.Next_Identifier;
         Scout_Design : constant Ship_Design_Handle :=
                          Athena.Handles.Ship_Design.Create
                            (Identifier => Scout_Id,
                             Empire     => Empire,
                             Name       => "Scout");

         Transport_Id     : constant String :=
                              Athena.Identifiers.Next_Identifier;
         Transport_Design : constant Ship_Design_Handle :=
                              Athena.Handles.Ship_Design.Create
                                (Identifier => Transport_Id,
                                 Empire     => Empire,
                                 Name       => "Transport");

         Defender_Id     : constant String :=
                             Athena.Identifiers.Next_Identifier;
         Defender_Design : constant Ship_Design_Handle :=
                             Athena.Handles.Ship_Design.Create
                               (Identifier => Defender_Id,
                                Empire     => Empire,
                                Name       => "Defender");

      begin

         Athena.Handles.Design_Component.Create
           (Ship_Design => Scout_Design,
            Component   => Component ("drive"),
            Power       => 1.0,
            Count       => 0);

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

         Athena.Handles.Design_Component.Create
           (Ship_Design => Defender_Design,
            Component   => Component ("drive"),
            Power       => 1.0,
            Count       => 0);

         Athena.Handles.Design_Component.Create
           (Ship_Design => Defender_Design,
            Component   => Component ("shield"),
            Power       => 10.0,
            Count       => 0);

         Athena.Handles.Design_Component.Create
           (Ship_Design => Defender_Design,
            Component   => Component ("beam"),
            Power       => 10.0,
            Count       => 4);

         Athena.Handles.Design_Component.Create
           (Ship_Design => Defender_Design,
            Component   => Component ("missile"),
            Power       => 10.0,
            Count       => 4);

         Athena.Handles.Design_Component.Create
           (Ship_Design => Defender_Design,
            Component   => Component ("fighter"),
            Power       => 10.0,
            Count       => 4);

         Athena.Ships.Create.Create_Ship
           (Empire => Empire,
            Star   => Star,
            Fleet  => Defenders,
            Design => Defender_Design,
            Name   => "Defender I");

         Athena.Handles.Design_Component.Create
           (Ship_Design => Transport_Design,
            Component   => Component ("drive"),
            Power       => 10.0,
            Count       => 0);

         Athena.Handles.Design_Component.Create
           (Ship_Design => Transport_Design,
            Component   => Component ("cargo"),
            Power       => 10.0,
            Count       => 0);

         Athena.Ships.Create.Create_Ship
           (Empire => Empire,
            Star   => Star,
            Fleet  => Transports,
            Design => Transport_Design,
            Name   => "Transport I");

         Athena.Handles.System_Designs.Create
           (Empire     => Empire,
            Scout      => Scout_Design,
            Transport  => Transport_Design,
            Defender   => Defender_Design,
            Destroyer  => Athena.Handles.Ship_Design.Empty_Handle,
            Cruiser    => Athena.Handles.Ship_Design.Empty_Handle,
            Battleship => Athena.Handles.Ship_Design.Empty_Handle,
            Carrier    => Athena.Handles.Ship_Design.Empty_Handle);

      end;

   end New_Empire;

end Athena.Empires.Create;
