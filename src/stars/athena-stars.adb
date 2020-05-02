with Ada.Text_IO;

with Ada.Containers.Doubly_Linked_Lists;

with WL.String_Maps;

with Athena.Elementary_Functions;

with Athena.Handles.Colony.Selections;
with Athena.Handles.Star.Selections;
with Athena.Handles.Star_Distance.Selections;

package body Athena.Stars is

   type Neighbour_Record is
      record
         Neighbour : Athena.Handles.Star.Star_Handle;
         Distance  : Non_Negative_Real;
      end record;

   package Neighbour_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Neighbour_Record);

   type Star_Record is
      record
         Handle     : Athena.Handles.Star.Star_Handle;
         Colony     : Athena.Handles.Colony.Colony_Handle;
         Neighbours : Neighbour_Lists.List;
      end record;

   package Star_Maps is
     new WL.String_Maps (Star_Record);

   Star_Map : Star_Maps.Map;

   --------------
   -- Distance --
   --------------

   function Distance
     (From, To : Athena.Handles.Star.Star_Class)
      return Non_Negative_Real
   is
      use Athena.Elementary_Functions;
   begin
      return Sqrt ((From.X - To.X) ** 2 + (From.Y - To.Y) ** 2);
   end Distance;

   ---------------
   -- Find_Star --
   ---------------

   function Find_Star
     (With_Name : String)
      return Athena.Handles.Star.Star_Class
   is
      use Athena.Handles.Star.Selections;
   begin
      return First_Where (Name = With_Name);
   end Find_Star;

   ----------------
   -- Get_Colony --
   ----------------

   function Get_Colony
     (Of_Star : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class
   is
      use Athena.Handles.Colony.Selections;
   begin
      return First_Where (Star = Of_Star);
   end Get_Colony;

   procedure Iterate_Nearest
     (To_Star   : Athena.Handles.Star.Star_Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access
        procedure (Star : Athena.Handles.Star.Star_Class;
                   Distance : Non_Negative_Real))
   is
      Rec : Star_Record renames Star_Map (To_Star.Identifier);
   begin
      if Rec.Neighbours.Is_Empty then
         declare
            use Athena.Handles.Star_Distance.Selections;
         begin
            for SD of Select_Where (From = To_Star) loop
               Rec.Neighbours.Append
                 (Neighbour_Record'
                    (Neighbour =>
                         Athena.Handles.Star.Get (SD.To.Reference_Star),
                     Distance  => SD.Distance));
            end loop;
         end;
      end if;

      for Neighbour of Rec.Neighbours loop
         exit when Neighbour.Distance > Max_Range;
         Process (Neighbour.Neighbour, Neighbour.Distance);
      end loop;
   end Iterate_Nearest;

   ----------------
   -- Load_Stars --
   ----------------

   procedure Load_Stars is
   begin
      Ada.Text_IO.Put ("loading stars ...");
      Ada.Text_IO.Flush;

      for Star of Athena.Handles.Star.Selections.Select_All loop
         Star_Map.Insert
           (Star.Identifier,
            Star_Record'
              (Handle     => Athena.Handles.Star.Get (Star.Reference_Star),
               Colony     => Athena.Handles.Colony.Empty_Handle,
               Neighbours => <>));
      end loop;

      for Colony of Athena.Handles.Colony.Selections.Select_All loop
         Star_Map (Colony.Star.Identifier).Colony :=
           Athena.Handles.Colony.Get (Colony.Reference_Colony);
      end loop;

      Ada.Text_IO.Put_Line (" done");

   end Load_Stars;

   ----------------
   -- Set_Colony --
   ----------------

   procedure Set_Colony
     (Star   : Athena.Handles.Star.Star_Class;
      Colony : Athena.Handles.Colony.Colony_Class)
   is
   begin
      Star_Map (Star.Identifier).Colony :=
        Athena.Handles.Colony.Get (Colony.Reference_Colony);
      Star.Update_Star
        .Set_Owner (Colony.Empire.Reference_Empire)
        .Done;
   end Set_Colony;

end Athena.Stars;
