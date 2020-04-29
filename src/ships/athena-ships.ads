with Ada.Containers.Doubly_Linked_Lists;

with Athena.Handles.Empire;
with Athena.Handles.Empire_Manager;
with Athena.Handles.Fleet;
with Athena.Handles.Ship.Selections;
with Athena.Handles.Ship_Component;
with Athena.Handles.Ship_Design;
with Athena.Handles.Star;

with Athena.Db;

package Athena.Ships is

   function Mass
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real;

   function Speed
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
     return Non_Negative_Real;

   function Total_Cargo_Space
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real;

   function Available_Space
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real;

   function Current_Cargo
     (Of_Ship : Athena.Handles.Ship.Ship_Class;
      Cargo   : Athena.Db.Cargo_Type)
      return Non_Negative_Real;

   function Get_Drive
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Athena.Handles.Ship_Component.Ship_Component_Class;

   function Design_Mass
     (Of_Design : Athena.Handles.Ship_Design.Ship_Design_Class)
      return Non_Negative_Real;

   function Design_Cargo
     (Design : Athena.Handles.Ship_Design.Ship_Design_Class)
      return Non_Negative_Real;

   procedure Iterate_Components
     (On_Ship : Athena.Handles.Ship.Ship_Class;
      Process : not null access
        procedure
          (Component : Athena.Handles.Ship_Component.Ship_Component_Class));

   package Ship_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Athena.Handles.Ship.Ship_Handle, Athena.Handles.Ship."=");

   procedure Get_Ships
     (In_Fleet : Athena.Handles.Fleet.Fleet_Class;
      Ships    : out Ship_Lists.List);

   procedure On_Arrival
     (Arriving_Ship : Athena.Handles.Ship.Ship_Class);

   procedure Load
     (Actor    : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real);

   procedure Unload
     (Actor    : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real);

   procedure Move_To
     (Actor : Athena.Handles.Ship.Ship_Class;
      Star  : Athena.Handles.Star.Star_Class);

   procedure For_All_Fleets
     (Process : not null access
        procedure (Fleet : Athena.Handles.Fleet.Fleet_Class));

   procedure For_All_Ships
     (Process : not null access
        procedure (Ship : Athena.Handles.Ship.Ship_Class));

   procedure Upgrade_Component
     (Component     : Athena.Handles.Ship_Component.Ship_Component_Class;
      New_Tec_Level : Non_Negative_Real);

   procedure For_All_Ships
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Process  : not null access
        procedure (Ship : Athena.Handles.Ship.Ship_Class));

   function Select_Managed_Ships
     (Managed_By : Athena.Handles.Empire_Manager.Empire_Manager_Class)
      return Athena.Handles.Ship.Selections.Selection;

   function Name_Ship
     (Owner     : Athena.Handles.Empire.Empire_Class;
      Base_Name : String)
      return String;

   function Name_Fleet
     (Owner     : Athena.Handles.Empire.Empire_Class;
      Base_Name : String)
      return String;

   procedure Load_Ships;

private

   procedure Add_Ship (Added_Ship : Athena.Handles.Ship.Ship_Class);
--     procedure Update_Ship (Updated_Ship : Athena.Handles.Ship.Ship_Class);

end Athena.Ships;
