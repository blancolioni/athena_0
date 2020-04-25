with Ada.Containers.Doubly_Linked_Lists;

with Athena.Logging;
with Athena.Real_Images;

with Athena.Empires;
with Athena.Knowledge.Stars;
with Athena.Orders;
with Athena.Stars;

with Athena.Handles.Star;

with Athena.Db;

package body Athena.Managers.Colonization is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   -------------------
   -- Create_Orders --
   -------------------

   procedure Create_Orders
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Priority   : Positive)
   is

      Knowledge   : Athena.Knowledge.Stars.Star_Knowledge;

      type Target_Record is
         record
            Star  : Athena.Handles.Star.Star_Handle;
            Score : Non_Negative_Real;
         end record;

      function Better (Left, Right : Target_Record) return Boolean
      is (Left.Score > Right.Score);

      package Target_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Target_Record);

      package Target_Sorting is
        new Target_Lists.Generic_Sorting (Better);

      Targets : Target_Lists.List;

      procedure Check_Colonization_Target
        (Star : Athena.Handles.Star.Star_Class;
         Stop : out Boolean);

      -------------------------------
      -- Check_Colonization_Target --
      -------------------------------

      procedure Check_Colonization_Target
        (Star : Athena.Handles.Star.Star_Class;
         Stop : out Boolean)
      is
      begin

         Stop := False;

         if Star.Owner.Has_Element then
            return;
         end if;

         if not Knowledge.Visited (Star)
           or else Knowledge.Colonizing (Star)
         then
            return;
         end if;

         declare
            Distance : constant Non_Negative_Real :=
                         Athena.Stars.Distance
                           (Star, Athena.Empires.Capital (For_Empire));
            Rec : constant Target_Record :=
                    Target_Record'
                      (Star  => Athena.Handles.Star.Get (Star.Reference_Star),
                       Score =>
                         Real'Max (Star.Resource, Star.Habitability)
                       / Distance / Distance);
         begin
            Athena.Logging.Log
              (For_Empire.Name
               & ": colonization target " & Star.Name
               & ": resource " & Image (Star.Resource * 100.0) & "%"
               & "; habitability " & Image (Star.Habitability * 100.0) & "%"
               & "; space" & Star.Space'Image
               & "; distance " & Image (Distance)
               & "; colonization score " & Image (Rec.Score));
            Targets.Append (Rec);
         end;

      end Check_Colonization_Target;

   begin
      Knowledge.Load (For_Empire);
      Knowledge.Iterate_Uncolonized
        (Check_Colonization_Target'Access);

      if not Targets.Is_Empty then
         Target_Sorting.Sort (Targets);
         Athena.Logging.Log
           ("colonizing: " & Targets.First_Element.Star.Name
            & " score " & Image (Targets.First_Element.Score));

         Athena.Orders.Move_Cargo
           (Athena.Db.Colonists, 10.0,
            Athena.Empires.Capital (For_Empire),
            Targets.First_Element.Star,
            Priority);
         Knowledge.Set_Colonizing
           (Targets.First_Element.Star, True);
      end if;

   end Create_Orders;

end Athena.Managers.Colonization;
