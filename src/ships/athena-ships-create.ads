with Athena.Handles.Empire;
with Athena.Handles.Manager;
with Athena.Handles.Star;
with Athena.Handles.Ship_Design;

package Athena.Ships.Create is

   procedure Create_Ship
     (Empire      : Athena.Handles.Empire.Empire_Class;
      Star        : Athena.Handles.Star.Star_Class;
      Design      : Athena.Handles.Ship_Design.Ship_Design_Class;
      Name        : String;
      Fleet       : Athena.Handles.Fleet.Fleet_Class;
      Manager     : Athena.Handles.Manager.Manager_Class;
      Destination : Athena.Handles.Star.Star_Class :=
        Athena.Handles.Star.Empty_Handle);

end Athena.Ships.Create;
