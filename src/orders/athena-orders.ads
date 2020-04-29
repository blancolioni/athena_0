with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Fleet;
with Athena.Handles.Manager;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Design;
with Athena.Handles.Star;
with Athena.Handles.Technology;

with Athena.Db;

package Athena.Orders is

   procedure Order_Industry
     (Colony   : Athena.Handles.Colony.Colony_Class;
      Quantity : Non_Negative_Real;
      Priority : Positive);

   procedure Set_Destination
     (Ship        : Athena.Handles.Ship.Ship_Class;
      Destination : Athena.Handles.Star.Star_Class;
      Priority    : Positive);

   subtype Cargo_Type is Athena.Db.Cargo_Type;

   procedure Move_Cargo
     (Cargo    : Cargo_Type;
      Quantity : Non_Negative_Real;
      From     : Athena.Handles.Colony.Colony_Class;
      To       : Athena.Handles.Star.Star_Class;
      Priority : Positive);

   procedure Move_Fleet
     (Fleet       : Athena.Handles.Fleet.Fleet_Class;
      Destination : Athena.Handles.Star.Star_Class);

   procedure Build_Ships
     (Empire   : Athena.Handles.Empire.Empire_Class;
      Design   : Athena.Handles.Ship_Design.Ship_Design_Class;
      Fleet    : Athena.Handles.Fleet.Fleet_Class;
      Manager  : Athena.Handles.Manager.Manager_Class;
      Send_To  : Athena.Handles.Star.Star_Class;
      Count    : Positive;
      Priority : Positive);

   procedure Research_Technology
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Technology : Athena.Handles.Technology.Technology_Class;
      Construct  : Non_Negative_Real;
      Priority   : Positive);

end Athena.Orders;
