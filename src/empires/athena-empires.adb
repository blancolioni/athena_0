with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Real_Images;

with Athena.Handles.Colony.Selections;
with Athena.Handles.Empire_Capital.Selections;
with Athena.Handles.System_Designs.Selections;
with Athena.Handles.System_Fleets.Selections;

with Athena.Db.Empire_Tec;

package body Athena.Empires is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   --------------------
   -- Add_Investment --
   --------------------

   procedure Add_Investment
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Technology : Athena.Handles.Technology.Technology_Class;
      Construct  : Non_Negative_Real)
   is
      use Athena.Elementary_Functions;
      Tec  : constant Athena.Db.Empire_Tec.Empire_Tec_Type :=
               Athena.Db.Empire_Tec.Get_By_Empire_Tec
                 (Empire.Reference_Empire,
                  Technology.Reference_Technology);

      Old_Level  : constant Non_Negative_Real :=
                     Athena.Empires.Current_Tec_Level
                       (Empire, Technology);
      New_Level  : constant Non_Negative_Real :=
                     Log (2.0 ** (Old_Level - 1.0) + Construct / 3000.0)
                     / Log (2.0)
                     + 1.0;
      New_Investment : constant Non_Negative_Real :=
                         Tec.Investment + Construct;
   begin

      Athena.Logging.Log
        (Empire.Name
         & " invests " & Image (Construct)
         & " construct on " & Technology.Tag & " technology "
         & " and increases from " & Image (Old_Level)
         & " to " & Image (New_Level));

      Athena.Db.Empire_Tec.Update_Empire_Tec
        (Tec.Get_Empire_Tec_Reference)
        .Set_Level (New_Level)
        .Set_Investment (New_Investment)
        .Done;
   end Add_Investment;

   -------------
   -- Capital --
   -------------

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Star.Star_Class
   is
      use Athena.Handles.Empire_Capital.Selections;
   begin
      return First_Where (Empire = Of_Empire).Star;
   end Capital;

   -------------
   -- Capital --
   -------------

   function Capital
     (Of_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Colony.Colony_Class
   is
      use Athena.Handles.Colony.Selections;
   begin
      return First_Where (Star = Capital (Of_Empire));
   end Capital;

   -----------------------
   -- Current_Tec_Level --
   -----------------------

   function Current_Tec_Level
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Technology : Athena.Handles.Technology.Technology_Class)
      return Non_Negative_Real
   is
   begin
      return Athena.Db.Empire_Tec
        .Get_By_Empire_Tec (Empire.Reference_Empire,
                            Technology.Reference_Technology)
        .Level;
   end Current_Tec_Level;

   ---------------------
   -- Defender_Design --
   ---------------------

   function Defender_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      use Athena.Handles.System_Designs.Selections;
   begin
      return First_Where (Empire = For_Empire).Defender;
   end Defender_Design;

   --------------------
   -- Defender_Fleet --
   --------------------

   function Defender_Fleet
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Fleet.Fleet_Class
   is
      use Athena.Handles.System_Fleets.Selections;
   begin
      return First_Where (Empire = For_Empire).Defenders;
   end Defender_Fleet;

   ----------
   -- Earn --
   ----------

   procedure Earn
     (Empire      : Athena.Handles.Empire.Empire_Class;
      Amount      : Athena.Money.Money_Type;
      Description : String)
   is
      use type Athena.Money.Money_Type;
      New_Cash : constant Athena.Money.Money_Type :=
                   Empire.Cash + Amount;
   begin
      Athena.Logging.Log
        (Empire.Name & ": earn " & Athena.Money.Show (Amount)
         & " for " & Description
         & ": cash now " & Athena.Money.Show (New_Cash));

      Empire.Update_Empire
        .Set_Cash (New_Cash)
        .Done;
   end Earn;

   ---------
   -- Pay --
   ---------

   procedure Pay
     (Empire      : Athena.Handles.Empire.Empire_Class;
      Amount      : Athena.Money.Money_Type;
      Description : String)
   is
      use type Athena.Money.Money_Type;
      New_Cash : Athena.Money.Money_Type := Empire.Cash;
      New_Debt : Athena.Money.Money_Type := Empire.Debt;
   begin
      if Amount >= New_Cash then
         New_Debt := New_Debt + (Amount - New_Cash);
         New_Cash := Athena.Money.Zero;
      else
         New_Cash := New_Cash - Amount;
      end if;

      Athena.Logging.Log
        (Empire.Name & ": pay " & Athena.Money.Show (Amount)
         & " for " & Description
         & ": remaining cash " & Athena.Money.Show (New_Cash)
         & "; debt " & Athena.Money.Show (New_Debt));

      Empire.Update_Empire
        .Set_Cash (New_Cash)
        .Set_Debt (New_Debt)
        .Done;
   end Pay;

   ------------------
   -- Scout_Design --
   ------------------

   function Scout_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      use Athena.Handles.System_Designs.Selections;
   begin
      return First_Where (Empire = For_Empire).Scout;
   end Scout_Design;

   -----------------
   -- Scout_Fleet --
   -----------------

   function Scout_Fleet
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Fleet.Fleet_Class
   is
      use Athena.Handles.System_Fleets.Selections;
   begin
      return First_Where (Empire = For_Empire).Scouts;
   end Scout_Fleet;

   ----------------------
   -- Transport_Design --
   ----------------------

   function Transport_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      use Athena.Handles.System_Designs.Selections;
   begin
      return First_Where (Empire = For_Empire).Transport;
   end Transport_Design;

   ---------------------
   -- Transport_Fleet --
   ---------------------

   function Transport_Fleet
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Fleet.Fleet_Class
   is
      use Athena.Handles.System_Fleets.Selections;
   begin
      return First_Where (Empire = For_Empire).Transports;
   end Transport_Fleet;

end Athena.Empires;
