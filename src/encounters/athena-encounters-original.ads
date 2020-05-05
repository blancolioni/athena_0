private with Ada.Containers.Doubly_Linked_Lists;

with Athena.Ships;

with Athena.Handles.Encounter;
with Athena.Handles.Ship;
with Athena.Handles.Ship_Component;
with Athena.Handles.Star;

package Athena.Encounters is

   type Encounter_Actor_Class is
     (Ship_Actor, Missile_Actor, Fighter_Actor,
      Beam_Actor, Kinetic_Actor);

   type Encounter_Point is
      record
         X, Y : Real;
      end record;

   type Encounter_Tick is new Natural;

   function Create
     (Star  : Athena.Handles.Star.Star_Class;
      Ships : Athena.Ships.Ship_Lists.List)
      return Athena.Handles.Encounter.Encounter_Class;

   procedure Execute
     (Encounter : Athena.Handles.Encounter.Encounter_Class);

   function Find
     (Star_Name   : String;
      Turn_Number : Positive)
      return Athena.Handles.Encounter.Encounter_Class;

   type Encounter_Interface is interface;

   procedure Get_State
     (Encounter : Encounter_Interface;
      Condition : out Unit_Real;
      Shields   : out Unit_Real)
   is abstract;

   procedure Iterate_Weapons
     (Encounter : Encounter_Interface;
      Process   : not null access
        procedure (Index : Positive;
                   Weapon : Athena.Handles.Ship_Component.Ship_Component_Class;
                   Charge : Unit_Real;
                   Remaining : Natural))
   is abstract;

   procedure Iterate_Hostiles
     (Encounter : Encounter_Interface;
      Process   : not null access
        procedure (Index   : Positive;
                   Mass    : Non_Negative_Real;
                   DX, DY  : Real;
                   Heading : Real;
                   Speed   : Real))
   is abstract;

   procedure Iterate_Allies
     (Encounter : Encounter_Interface;
      Process   : not null access
        procedure (Ally    : Athena.Handles.Ship.Ship_Class;
                   DX, DY  : Real;
                   Heading : Real;
                   Speed   : Real))
   is abstract;

   type Encounter_Order is private;

   function Set_Destination
     (DX, DY  : Real;
      Heading : Real)
      return Encounter_Order;

   function Fire_Weapon
     (Weapon : Positive;
      Target : Positive)
     return Encounter_Order;

   function Jump
     (Target : Athena.Handles.Star.Star_Class)
      return Encounter_Order;

   type Encounter_Order_List is private;

   procedure Add
     (To    : in out Encounter_Order_List;
      Order : Encounter_Order);

   type Encounter_Script_Interface is interface;

   procedure Get_Orders
     (Script : Encounter_Script_Interface;
      Encounter : Encounter_Interface'Class;
      Orders    : out Encounter_Order_List)
   is abstract;

private

   type Order_Class is (Set_Destination, Fire_Weapon, Jump);

   type Encounter_Order (Class : Order_Class := Set_Destination) is
      record
         case Class is
            when Set_Destination =>
               Target_X, Target_Y : Real;
               Target_Heading     : Real;
            when Fire_Weapon =>
               Weapon             : Positive;
               Target             : Positive;
            when Jump =>
               Target_Star        : Athena.Handles.Star.Star_Handle;
         end case;
      end record;

   package Encounter_Order_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Encounter_Order);

   type Encounter_Order_List is
      record
         List : Encounter_Order_Lists.List;
      end record;

   function Set_Destination
     (DX, DY  : Real;
      Heading : Real)
      return Encounter_Order
   is (Set_Destination, DX, DY, Heading);

   function Fire_Weapon
     (Weapon : Positive;
      Target : Positive)
      return Encounter_Order
   is (Fire_Weapon, Weapon, Target);

   function Jump
     (Target : Athena.Handles.Star.Star_Class)
      return Encounter_Order
   is (Jump, Athena.Handles.Star.Get (Target.Reference_Star));

end Athena.Encounters;
