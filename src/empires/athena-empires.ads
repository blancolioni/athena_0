with Athena.Money;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Fleet;
with Athena.Handles.Ship_Design;
with Athena.Handles.Star;
with Athena.Handles.Technology;

package Athena.Empires is

   function Current_Tec_Level
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Technology : Athena.Handles.Technology.Technology_Class)
      return Non_Negative_Real;

   procedure Add_Investment
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Technology : Athena.Handles.Technology.Technology_Class;
      Construct  : Non_Negative_Real);

   procedure Pay
     (Empire      : Athena.Handles.Empire.Empire_Class;
      Amount      : Athena.Money.Money_Type;
      Description : String);

   procedure Earn
     (Empire      : Athena.Handles.Empire.Empire_Class;
      Amount      : Athena.Money.Money_Type;
      Description : String);

   function Scout_Fleet
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Fleet.Fleet_Class;

   function Transport_Fleet
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Fleet.Fleet_Class;

   function Defender_Fleet
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Fleet.Fleet_Class;

   function Scout_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Transport_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Defender_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Star.Star_Class;

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Colony.Colony_Class;

end Athena.Empires;
