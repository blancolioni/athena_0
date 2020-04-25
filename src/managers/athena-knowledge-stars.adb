with Athena.Logging;
with Athena.Turns;

with Athena.Handles.Colony.Selections;
with Athena.Handles.Star_Distance.Selections;
with Athena.Handles.Star_Knowledge;

with Athena.Db.Star_Knowledge;

package body Athena.Knowledge.Stars is

   type Cached_Knowledge is
      record
         Turn      : Positive;
         Knowledge : Star_Knowledge;
      end record;

   package Cached_Knowledge_Maps is
     new WL.String_Maps (Cached_Knowledge);

   Cached_Knowledge_Map : Cached_Knowledge_Maps.Map;

   procedure Update_Neighbours
     (Knowledge : in out Star_Knowledge'Class;
      Colony    : Athena.Handles.Colony.Colony_Class);

   ----------------------
   -- Clear_Colonizing --
   ----------------------

   procedure Clear_Colonizing
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Star       : Athena.Handles.Star.Star_Class)
   is
      use type Athena.Db.Star_Knowledge_Reference;
      K : constant Athena.Db.Star_Knowledge_Reference :=
            Athena.Db.Star_Knowledge.Get_Reference_By_Star_Knowledge
              (Star.Reference_Star, Empire.Reference_Empire);
   begin
      if K /= Athena.Db.Null_Star_Knowledge_Reference then
         Athena.Db.Star_Knowledge.Update_Star_Knowledge (K)
           .Set_Colonizing (False)
           .Done;
      end if;

   end Clear_Colonizing;

   ------------------------
   -- Iterate_Neighbours --
   ------------------------

   procedure Iterate_Neighbours
     (Knowledge : Star_Knowledge'Class;
      Max_Range : Non_Negative_Real;
      Process   : not null access procedure
        (Neighbour : Athena.Handles.Star.Star_Class;
         Nearest   : Athena.Handles.Colony.Colony_Class; Stop : out Boolean))
   is
   begin
      for Rec of Knowledge.Neighbour_List loop
         exit when Rec.Distance > Max_Range;

         declare
            Stop : Boolean;
         begin
            Process (Rec.Neighbour, Rec.Nearest, Stop);
            exit when Stop;
         end;
      end loop;
   end Iterate_Neighbours;

   -------------------------
   -- Iterate_Uncolonized --
   -------------------------

   procedure Iterate_Uncolonized
     (Knowledge : Star_Knowledge'Class;
      Process   : not null access
        procedure (Star      : Athena.Handles.Star.Star_Class;
                   Stop      : out Boolean))
   is
   begin
      for Item of Knowledge.Uncolonized loop
         declare
            Stop : Boolean;
         begin
            if not Knowledge.Colonizing.Contains (Item.Identifier) then
               Process (Item, Stop);
               exit when Stop;
            end if;
         end;
      end loop;
   end Iterate_Uncolonized;

   ----------
   -- Load --
   ----------

   procedure Load
     (Knowledge  : in out Star_Knowledge;
      For_Empire : Athena.Handles.Empire.Empire_Class)
   is
      Turn : constant Positive := Athena.Turns.Current_Turn;
   begin

      declare
         use Cached_Knowledge_Maps;
         Position : Cursor :=
                      Cached_Knowledge_Map.Find (For_Empire.Identifier);
      begin
         if Has_Element (Position) then
            if Element (Position).Turn = Turn then
               Knowledge := Element (Position).Knowledge;
               return;
            else
               Cached_Knowledge_Map.Delete (Position);
            end if;
         end if;
      end;

      Knowledge.Empire :=
        Athena.Handles.Empire.Get (For_Empire.Reference_Empire);

      Athena.Logging.Log
        (For_Empire.Name
         & "/knowledge: loading");

      declare
         use Athena.Handles.Colony.Selections;
      begin
         for Colony of Select_Where (Empire = For_Empire) loop
            Athena.Logging.Log
              (For_Empire.Name & "/knowledge: scanning colony "
               & Colony.Star.Name);
            Update_Neighbours (Knowledge, Colony);
         end loop;
      end;

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: sorting neighbours");

      declare
         function Closer (Left, Right : Neighbour_Record) return Boolean
         is (Left.Distance < Right.Distance);

         package Neighbour_Sorting is
           new Neighbour_Lists.Generic_Sorting (Closer);

      begin
         for Neighbour of Knowledge.Neighbour_Map loop
            Knowledge.Neighbour_List.Append (Neighbour);
         end loop;

         Neighbour_Sorting.Sort (Knowledge.Neighbour_List);
      end;

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: scanning star knowledge");

      for K of Athena.Db.Star_Knowledge.Select_By_Empire
        (For_Empire.Reference_Empire)
      loop
         if K.Visited then
            declare
               Handle : constant Athena.Handles.Star.Star_Handle :=
                          Athena.Handles.Star.Get (K.Star);
            begin
               Knowledge.Visited.Insert (Handle.Identifier, Handle);
               if not Handle.Owner.Has_Element then
                  Knowledge.Uncolonized.Insert (Handle.Identifier, Handle);
               end if;
               if K.Colonizing then
                  Knowledge.Colonizing.Insert (Handle.Identifier, Handle);
               end if;
            end;
         end if;
      end loop;

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: adding to cache");

      Cached_Knowledge_Map.Insert
        (For_Empire.Identifier, (Turn, Knowledge));

      Athena.Logging.Log
        (For_Empire.Name & "/knowledge: done");

   end Load;

   --------------------
   -- Set_Colonizing --
   --------------------

   procedure Set_Colonizing
     (Knowledge  : in out Star_Knowledge'Class;
      Star       : Athena.Handles.Star.Star_Class;
      Colonizing : Boolean)
   is
      use type Athena.Db.Star_Knowledge_Reference;
      K : constant Athena.Db.Star_Knowledge_Reference :=
            Athena.Db.Star_Knowledge.Get_Reference_By_Star_Knowledge
              (Star.Reference_Star, Knowledge.Empire.Reference_Empire);
   begin
      if K = Athena.Db.Null_Star_Knowledge_Reference then
         Athena.Handles.Star_Knowledge.Create
           (Star       => Star,
            Empire     => Knowledge.Empire,
            Visited    => False,
            Colonizing => Colonizing);
      else
         Athena.Db.Star_Knowledge.Update_Star_Knowledge (K)
           .Set_Colonizing (Colonizing)
           .Done;
      end if;

      if Colonizing then
         Knowledge.Colonizing.Insert (Star.Identifier, Star);
      else
         Knowledge.Colonizing.Delete (Star.Identifier);
      end if;

      Cached_Knowledge_Map.Replace
        (Knowledge.Empire.Identifier,
         (Athena.Turns.Current_Turn, Star_Knowledge (Knowledge)));

   end Set_Colonizing;

   -----------------------
   -- Update_Neighbours --
   -----------------------

   procedure Update_Neighbours
     (Knowledge : in out Star_Knowledge'Class;
      Colony    : Athena.Handles.Colony.Colony_Class)
   is
      use Athena.Handles.Star_Distance.Selections;
   begin
      for SD of Select_Where (From = Colony.Star) loop
         if not SD.To.Owner.Has_Element
           or else SD.To.Owner.Name /= Knowledge.Empire.Name
         then
            declare
               use Neighbour_Maps;
               Position : constant Neighbour_Maps.Cursor :=
                            Knowledge.Neighbour_Map.Find (SD.To.Identifier);
            begin
               if Has_Element (Position) then
                  declare
                     Item : Neighbour_Record renames
                              Knowledge.Neighbour_Map (Position);
                  begin
                     if SD.Distance < Item.Distance then
                        Item.Distance := SD.Distance;
                        Item.Nearest  :=
                          Athena.Handles.Colony.Get (Colony.Reference_Colony);
                     end if;
                  end;
               else
                  Knowledge.Neighbour_Map.Insert
                    (SD.To.Identifier,
                     Neighbour_Record'
                       (Neighbour =>
                            Athena.Handles.Star.Get (SD.To.Reference_Star),
                        Nearest   =>
                          Athena.Handles.Colony.Get (Colony.Reference_Colony),
                        Distance  => SD.Distance));
               end if;
            end;
         end if;
      end loop;
   end Update_Neighbours;

end Athena.Knowledge.Stars;
