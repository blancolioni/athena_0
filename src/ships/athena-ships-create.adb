with Athena.Identifiers;

with Athena.Empires;

with Athena.Handles.Design_Component.Selections;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Component;

package body Athena.Ships.Create is

   -----------------
   -- Create_Ship --
   -----------------

   procedure Create_Ship
     (Empire      : Athena.Handles.Empire.Empire_Class;
      Star        : Athena.Handles.Star.Star_Class;
      Design      : Athena.Handles.Ship_Design.Ship_Design_Class;
      Name        : String;
      Fleet       : Athena.Handles.Fleet.Fleet_Class;
      Manager     : Athena.Handles.Empire_Manager.Empire_Manager_Class;
      Destination : Athena.Handles.Star.Star_Class :=
        Athena.Handles.Star.Empty_Handle)
   is
      use Athena.Handles.Design_Component.Selections;
      Ship_Id : constant String :=
                  Athena.Identifiers.Next_Identifier;
      Ship : constant Athena.Handles.Ship.Ship_Handle :=
                  Athena.Handles.Ship.Create
                    (Identifier  => Ship_Id,
                     Empire      => Empire,
                     Star        => Star,
                     Ship_Design => Design,
                     Fleet       => Fleet,
                     Manager     => Manager,
                     Name        => Name,
                     Destination => Destination,
                     First_Order => 0,
                     Last_Order  => 0,
                     Progress    => 0.0,
                     Experience  => 0.0,
                     Colonists   => 0.0,
                     Industry    => 0.0,
                     Material    => 0.0);
   begin
      for Component of Select_Where (Ship_Design = Design) loop
         Athena.Handles.Ship_Component.Create
           (Identifier       => Athena.Identifiers.Next_Identifier,
            Ship             => Ship,
            Component        => Component.Component,
            Design_Component => Component,
            Condition        => 1.0,
            Tec_Level        =>
              Athena.Empires.Current_Tec_Level
                (Empire, Component.Component.Technology));
      end loop;
      Add_Ship (Ship);
   end Create_Ship;

end Athena.Ships.Create;
