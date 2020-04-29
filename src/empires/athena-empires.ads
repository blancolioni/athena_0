with Athena.Money;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Empire_Manager;
with Athena.Handles.Manager;
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

   function Get_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class;

   function Exploration_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class;

   function Transport_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class;

   function Defense_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class;

   function Attack_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class;

   function Scout_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Transport_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Defender_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Destroyer_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Cruiser_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Battleship_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Carrier_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Star.Star_Class;

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Colony.Colony_Class;

end Athena.Empires;
