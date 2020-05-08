private with Ada.Containers.Doubly_Linked_Lists;
private with WL.String_Maps;

with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Star;

package Athena.Knowledge.Stars is

   type Star_Knowledge is tagged private;

   procedure Load
     (Knowledge  : in out Star_Knowledge;
      For_Empire : Athena.Handles.Empire.Empire_Class);

   function Colonizing
     (Knowledge : Star_Knowledge'Class;
      Star      : Athena.Handles.Star.Star_Class)
      return Boolean;

   function Visited
     (Knowledge : Star_Knowledge'Class;
      Star      : Athena.Handles.Star.Star_Class)
      return Boolean;

   procedure Iterate_Neighbours
     (Knowledge : Star_Knowledge'Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Neighbour : Athena.Handles.Star.Star_Class;
                   Nearest   : Athena.Handles.Colony.Colony_Class;
                   Stop      : out Boolean));

   procedure Iterate_Threats
     (Knowledge : Star_Knowledge'Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Threat      : Athena.Handles.Empire.Empire_Class;
                   Threat_Star : Athena.Handles.Star.Star_Class;
                   Nearest     : Athena.Handles.Colony.Colony_Class;
                   Stop        : out Boolean));

   procedure Iterate_Uncolonized
     (Knowledge : Star_Knowledge'Class;
      Process   : not null access
        procedure (Star      : Athena.Handles.Star.Star_Class;
                   Stop      : out Boolean));

   procedure Set_Colonizing
     (Knowledge  : in out Star_Knowledge'Class;
      Star       : Athena.Handles.Star.Star_Class;
      Colonizing : Boolean);

   procedure Clear_Colonizing
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Star       : Athena.Handles.Star.Star_Class);

   procedure Clear_Cache;

private

   type Neighbour_Record is
      record
         Neighbour : Athena.Handles.Star.Star_Handle;
         Nearest   : Athena.Handles.Colony.Colony_Handle;
         Distance  : Non_Negative_Real;
      end record;

   package Neighbour_Maps is
     new WL.String_Maps (Neighbour_Record);

   package Neighbour_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Neighbour_Record);

   package Star_Maps is
     new WL.String_Maps
       (Athena.Handles.Star.Star_Class,
        Athena.Handles.Star."=");

   type Star_Knowledge is tagged
      record
         Empire         : Athena.Handles.Empire.Empire_Handle;
         Neighbour_Map  : Neighbour_Maps.Map;
         Neighbour_List : Neighbour_Lists.List;
         Threat_Map  : Neighbour_Maps.Map;
         Threat_List : Neighbour_Lists.List;
         Uncolonized    : Star_Maps.Map;
         Visited        : Star_Maps.Map;
         Colonizing     : Star_Maps.Map;
      end record;

   function Visited
     (Knowledge : Star_Knowledge'Class;
      Star      : Athena.Handles.Star.Star_Class)
      return Boolean
   is (Knowledge.Visited.Contains (Star.Identifier));

   function Colonizing
     (Knowledge : Star_Knowledge'Class;
      Star      : Athena.Handles.Star.Star_Class)
      return Boolean
   is (Knowledge.Colonizing.Contains (Star.Identifier));

end Athena.Knowledge.Stars;
