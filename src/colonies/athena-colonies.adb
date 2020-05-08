with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.String_Maps;

with Athena.Identifiers;
with Athena.Logging;
with Athena.Money;
with Athena.Real_Images;

with Athena.Empires;
with Athena.Stars;

with Athena.Knowledge.Stars;

with Athena.Handles.Colony.Selections;

package body Athena.Colonies is

   Log_Use_Assets : constant Boolean := False;

   type Colony_Reference is range 1 .. Natural'Last;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Log
     (Colony : Athena.Handles.Colony.Colony_Class;
      Message : String);

   type Colony_Record is
      record
         Handle : Athena.Handles.Colony.Colony_Handle;
      end record;

   package Colony_Vectors is
     new Ada.Containers.Vectors (Colony_Reference, Colony_Record);

   package Colony_Maps is
     new WL.String_Maps (Colony_Reference);

   package Empire_Colony_Maps is
     new WL.String_Maps (Colony_Maps.Map, Colony_Maps."=");

   Colony_Vector : Colony_Vectors.Vector;
   Colony_Map    : Colony_Maps.Map;
   Empire_Colony : Empire_Colony_Maps.Map;
   Star_Colony   : Colony_Maps.Map;

   -----------------
   -- Best_Colony --
   -----------------

   function Best_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Score    : not null access
        function (Colony : Athena.Handles.Colony.Colony_Class) return Real)
      return Athena.Handles.Colony.Colony_Class
   is
      Best_Score : Real := Real'First;
      Result     : Athena.Handles.Colony.Colony_Handle :=
                     Athena.Handles.Colony.Empty_Handle;
   begin
      for Ref of Empire_Colony.Element (Owned_By.Identifier) loop
         declare
            Colony     : constant Athena.Handles.Colony.Colony_Handle :=
                           Colony_Vector (Ref).Handle;
            This_Score : constant Real := Score (Colony);
         begin
            if This_Score > Best_Score then
               Best_Score := This_Score;
               Result := Colony;
            end if;
         end;
      end loop;
      return Result;
   end Best_Colony;

   -----------------
   -- Can_Provide --
   -----------------

   function Can_Provide
     (Colony    : Athena.Handles.Colony.Colony_Class;
      Construct : Non_Negative_Real := 0.0;
      Material  : Non_Negative_Real := 0.0)
      return Boolean
   is
   begin
      if Colony.Construct >= Construct
        and then Colony.Material >= Material
      then
         return True;
      end if;

      if Colony.Construct < Construct then
         return False;
      end if;

      declare
         Used_Construct : constant Non_Negative_Real :=
                            Material / Colony.Star.Resource;
      begin
         return Used_Construct + Construct <= Colony.Construct;
      end;

   end Can_Provide;

   --------------------
   -- Capture_Colony --
   --------------------

   procedure Capture_Colony
     (Colony      : Athena.Handles.Colony.Colony_Class;
      Captured_By : Athena.Handles.Empire.Empire_Class)
   is
   begin
      Empire_Colony (Colony.Empire.Identifier).Delete (Colony.Identifier);
      Empire_Colony (Captured_By.Identifier).Insert
        (Colony.Identifier, Colony_Map (Colony.Identifier));
      Colony.Update_Colony
        .Set_Empire (Captured_By.Reference_Empire)
        .Done;
      Athena.Knowledge.Stars.Clear_Cache;

   end Capture_Colony;

   -----------------
   -- Find_Colony --
   -----------------

   function Find_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Test     : not null access function
        (Colony : Athena.Handles.Colony.Colony_Class) return Boolean)
      return Athena.Handles.Colony.Colony_Class
   is
   begin
      for Ref of Empire_Colony.Element (Owned_By.Identifier) loop
         declare
            Colony     : constant Athena.Handles.Colony.Colony_Handle :=
                           Colony_Vector (Ref).Handle;
         begin
            if Test (Colony) then
               return Colony;
            end if;
         end;
      end loop;
      return Athena.Handles.Colony.Empty_Handle;
   end Find_Colony;

   ----------------------
   -- For_All_Colonies --
   ----------------------

   procedure For_All_Colonies
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Process  : not null access procedure
        (Colony : Athena.Handles.Colony.Colony_Class))
   is
   begin
      for Ref of Empire_Colony.Element (Owned_By.Identifier) loop
         declare
            Colony     : constant Athena.Handles.Colony.Colony_Handle :=
                           Colony_Vector (Ref).Handle;
         begin
            Process (Colony);
         end;
      end loop;
   end For_All_Colonies;

   ----------------
   -- Get_Colony --
   ----------------

   function Get_Colony
     (At_Star : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class
   is
      use Colony_Maps;
      Position : constant Cursor := Star_Colony.Find (At_Star.Identifier);
   begin
      if Has_Element (Position) then
         return Colony_Vector (Element (Position)).Handle;
      else
         return Athena.Handles.Colony.Empty_Handle;
      end if;
   end Get_Colony;

   -------------------
   -- Load_Colonies --
   -------------------

   procedure Load_Colonies is
   begin
      Ada.Text_IO.Put ("loading colonies ...");
      Ada.Text_IO.Flush;

      for Colony of
        Athena.Handles.Colony.Selections.Select_All
      loop
         Colony_Vector.Append
           ((Handle => Athena.Handles.Colony.Get (Colony.Reference_Colony)));
         Colony_Map.Insert (Colony.Identifier, Colony_Vector.Last_Index);

         Star_Colony.Insert (Colony.Identifier, Colony_Vector.Last_Index);
         if not Empire_Colony.Contains (Colony.Empire.Identifier) then
            Empire_Colony.Insert
              (Colony.Empire.Identifier, Colony_Maps.Empty_Map);
         end if;
         Empire_Colony (Colony.Empire.Identifier)
           .Insert (Colony.Identifier, Colony_Vector.Last_Index);
      end loop;
      Ada.Text_IO.Put_Line (" done");
   end Load_Colonies;

   ---------
   -- Log --
   ---------

   procedure Log
     (Colony  : Athena.Handles.Colony.Colony_Class;
      Message : String)
   is
   begin
      Athena.Logging.Log
        (Colony.Empire.Name
         & ": colony on "
         & Colony.Star.Name
         & ": "
         & Message);
   end Log;

   --------------------
   -- Nearest_Colony --
   --------------------

   function Nearest_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      To_Star  : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class
   is
      Distance : Non_Negative_Real := Non_Negative_Real'Last;
      Result   : Athena.Handles.Colony.Colony_Handle :=
                   Athena.Handles.Colony.Empty_Handle;
   begin
      for Ref of Empire_Colony.Element (Owned_By.Identifier) loop
         declare
            Colony     : constant Athena.Handles.Colony.Colony_Handle :=
                           Colony_Vector (Ref).Handle;
            D          : constant Non_Negative_Real :=
                           Athena.Stars.Distance (Colony.Star, To_Star);
         begin
            if D < Distance then
               Distance := D;
               Result :=
                 Athena.Handles.Colony.Get
                   (Colony.Reference_Colony);
            end if;
         end;
      end loop;

      return Result;
   end Nearest_Colony;

   ----------------
   -- New_Colony --
   ----------------

   procedure New_Colony
     (At_Star : Athena.Handles.Star.Star_Class;
      Owner   : Athena.Handles.Empire.Empire_Class;
      Pop     : Non_Negative_Real;
      Ind     : Non_Negative_Real;
      Mat     : Non_Negative_Real)
   is
      Colony : constant Athena.Handles.Colony.Colony_Class :=
                 Athena.Handles.Colony.Create
                   (Identifier => Athena.Identifiers.Next_Identifier,
                    Star       => At_Star,
                    Empire     => Owner,
                    Construct  => 0.0,
                    Pop        => Pop,
                    Colonists  => 0.0,
                    Industry   => Ind,
                    Material   => Mat);
   begin
      Athena.Stars.Set_Colony (At_Star, Colony);
      Colony_Vector.Append
        ((Handle => Athena.Handles.Colony.Get (Colony.Reference_Colony)));
      Colony_Map.Insert (Colony.Identifier, Colony_Vector.Last_Index);

      Star_Colony.Insert (Colony.Identifier, Colony_Vector.Last_Index);
      if not Empire_Colony.Contains (Owner.Identifier) then
         Empire_Colony.Insert (Owner.Identifier, Colony_Maps.Empty_Map);
      end if;
      Empire_Colony (Owner.Identifier)
        .Insert (Colony.Identifier, Colony_Vector.Last_Index);
   end New_Colony;

   ----------------------
   -- Produce_Material --
   ----------------------

   procedure Produce_Material
     (Colony   : Athena.Handles.Colony.Colony_Class;
      Quantity : Non_Negative_Real)
   is
      use Athena.Money;
      Max : Non_Negative_Real := Quantity;
   begin
      Max := Real'Min (Max, Colony.Construct * Colony.Star.Resource);
      --  Max := Real'Min (Max, To_Real (Colony.Empire.Cash));

      declare
         Produced      : constant Non_Negative_Real := Max;
         New_Construct : constant Non_Negative_Real :=
                           Real'Max
                             (Colony.Construct
                              - Produced / Colony.Star.Resource,
                              0.0);
         New_Material  : constant Non_Negative_Real :=
                           Colony.Material + Produced;
      begin
         Athena.Logging.Log
           (Colony.Empire.Name
            & ": colony on "
            & Colony.Star.Name
            & ": ordered "
            & Image (Quantity)
            & " material"
            & "; available construct "
            & Image (Colony.Construct)
            & " resource "
            & Image (Colony.Star.Resource * 100.0) & "%"
            & " cash "
            & Athena.Money.Show (Colony.Empire.Cash)
            & "; produced "
            & Image (Produced)
            & "; new material stock "
            & Image (New_Material));

         Colony.Update_Colony
           .Set_Construct (New_Construct)
           .Set_Material (New_Material)
           .Done;

         Athena.Empires.Pay (Colony.Empire, To_Money (Produced),
                             "material production");
      end;
   exception
      when others =>
         Athena.Logging.Log
           (Colony.Empire.Name
            & ": colony on "
            & Colony.Star.Name
            & ": exception while producing material: max = "
            & Image (Max)
            & "; construct = "
            & Image (Colony.Construct)
            & "; cash = "
            & Athena.Money.Show (Colony.Empire.Cash)
            & "; resource = "
            & Image (Colony.Star.Resource));
         raise;

   end Produce_Material;

   ----------------
   -- Use_Assets --
   ----------------

   procedure Use_Assets
     (Colony      : Athena.Handles.Colony.Colony_Class;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0;
      Description : String)
   is
   begin

      if Log_Use_Assets then
         if Construct = 0.0 and then Material = 0.0 then
            Log (Colony, "uses no assets for " & Description);
         elsif Construct = 0.0 then
            Log (Colony,
                 "uses " & Image (Material) & " material for "
                 & Description);
         elsif Material = 0.0 then
            Log (Colony,
                 "uses " & Image (Construct) & " construct for "
                 & Description);
         else
            Log (Colony,
                 "uses " & Image (Construct) & " construct"
                 & " and " & Image (Material) & " material for "
                 & Description);
         end if;
      end if;

      declare
         New_Construct : constant Non_Negative_Real :=
                           Real'Max (Colony.Construct - Construct, 0.0);
         New_Material  : constant Non_Negative_Real :=
                           Real'Max (Colony.Material - Material, 0.0);
      begin
         Colony.Update_Colony
           .Set_Construct (New_Construct)
           .Set_Material (New_Material)
           .Done;
      end;
   end Use_Assets;

end Athena.Colonies;
