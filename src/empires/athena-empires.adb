with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Real_Images;

with Athena.Handles.Colony.Selections;
with Athena.Handles.Empire_Capital.Selections;
with Athena.Handles.Empire_Manager.Selections;
with Athena.Handles.System_Designs.Selections;

with Athena.Db.Empire_Tec;

package body Athena.Empires is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   function Get_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Tag        : String)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class;

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

   --------------------
   -- Attack_Manager --
   --------------------

   function Attack_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class
   is
   begin
      return Get_Manager (For_Empire, "attack");
   end Attack_Manager;

   -----------------------
   -- Battleship_Design --
   -----------------------

   function Battleship_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      use Athena.Handles.System_Designs.Selections;
   begin
      return First_Where (Empire = For_Empire).Battleship;
   end Battleship_Design;

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

   --------------------
   -- Carrier_Design --
   --------------------

   function Carrier_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      use Athena.Handles.System_Designs.Selections;
   begin
      return First_Where (Empire = For_Empire).Carrier;
   end Carrier_Design;

   --------------------
   -- Cruiser_Design --
   --------------------

   function Cruiser_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      use Athena.Handles.System_Designs.Selections;
   begin
      return First_Where (Empire = For_Empire).Cruiser;
   end Cruiser_Design;

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

   ---------------------
   -- Defense_Manager --
   ---------------------

   function Defense_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class
   is
   begin
      return Get_Manager (For_Empire, "defend");
   end Defense_Manager;

   ----------------------
   -- Destroyer_Design --
   ----------------------

   function Destroyer_Design
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      use Athena.Handles.System_Designs.Selections;
   begin
      return First_Where (Empire = For_Empire).Destroyer;
   end Destroyer_Design;

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

   -------------------------
   -- Exploration_Manager --
   -------------------------

   function Exploration_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class
   is
   begin
      return Get_Manager (For_Empire, "explore");
   end Exploration_Manager;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Tag        : String)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class
   is
      use Athena.Handles.Empire_Manager.Selections;
   begin
      for Manager of Select_Where (Empire = For_Empire) loop
         if Manager.Manager.Tag = Tag then
            return Manager;
         end if;
      end loop;
      raise Constraint_Error with "no such manager: " & Tag;
   end Get_Manager;

   -----------------
   -- Get_Manager --
   -----------------

   function Get_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class;
      Manager    : Athena.Handles.Manager.Manager_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class
   is
   begin
      return Get_Manager (For_Empire, Manager.Tag);
   end Get_Manager;

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

   function Transport_Manager
     (For_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Empire_Manager.Empire_Manager_Class
   is
   begin
      return Get_Manager (For_Empire, "transport");
   end Transport_Manager;

end Athena.Empires;
