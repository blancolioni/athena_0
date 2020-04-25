with Athena.Logging;
with Athena.Money;
with Athena.Real_Images;

with Athena.Empires;

with Athena.Handles.Colony.Selections;

package body Athena.Colonies is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Log
     (Colony : Athena.Handles.Colony.Colony_Class;
      Message : String);

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

   -----------------
   -- Find_Colony --
   -----------------

   function Find_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Test     : not null access function
        (Colony : Athena.Handles.Colony.Colony_Class) return Boolean)
      return Athena.Handles.Colony.Colony_Class
   is
      use Athena.Handles.Colony.Selections;
   begin
      for Colony of Select_Where (Empire = Owned_By) loop
         if Test (Colony) then
            return Colony;
         end if;
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
      use Athena.Handles.Colony.Selections;
   begin
      for Colony of Select_Where (Empire = Owned_By) loop
         Process (Colony);
      end loop;
   end For_All_Colonies;

   ----------------
   -- Get_Colony --
   ----------------

   function Get_Colony
     (At_Star : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class
   is
      use Athena.Handles.Colony.Selections;
   begin
      return First_Where (Star = At_Star);
   end Get_Colony;

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
