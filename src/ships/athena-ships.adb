with Ada.Containers.Vectors;
with Ada.Text_IO;

with WL.Numerics.Roman;
with WL.String_Maps;

with Athena.Logging;
with Athena.Real_Images;

with Athena.Empires;
with Athena.Technology;

with Athena.Handles.Design_Component.Selections;
with Athena.Handles.Fleet.Selections;
with Athena.Handles.Ship_Component.Selections;
with Athena.Handles.Ship_Order;
with Athena.Handles.Star_Knowledge.Selections;

with Athena.Db.Fleet;
with Athena.Db.Ship;
with Athena.Db.Ship_Order;

package body Athena.Ships is

   type Ship_Reference is new Natural;
   subtype Real_Ship_Reference is
     Ship_Reference range 1 .. Ship_Reference'Last;
   --  Null_Ship_Reference : constant Ship_Reference := 0;

   type Ship_Component_Record is
      record
         Component : Athena.Handles.Ship_Component.Ship_Component_Handle :=
                       Athena.Handles.Ship_Component.Empty_Handle;
         Mass      : Non_Negative_Real := 0.0;
         Tec_Level : Non_Negative_Real := 0.0;
         Condition : Unit_Real         := 0.0;
      end record;

   package Ship_Component_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Ship_Component_Record);

   type Ship_Record is
      record
         Handle  : Athena.Handles.Ship.Ship_Handle;
         Drive   : Ship_Component_Record;
         Cargo   : Ship_Component_Record;
         Shield  : Ship_Component_Record;
         Repair  : Ship_Component_Record;
         Weapons : Ship_Component_Lists.List;
         Mass    : Non_Negative_Real;
         Space   : Non_Negative_Real;
      end record;

   package Ship_Vectors is
     new Ada.Containers.Vectors (Real_Ship_Reference, Ship_Record);

   package Ship_Maps is
     new WL.String_Maps (Real_Ship_Reference);

   package Empire_Ships_Maps is
     new WL.String_Maps (Ship_Maps.Map, Ship_Maps."=");

   Ship_Vector  : Ship_Vectors.Vector;
   All_Ships    : Ship_Maps.Map;
   Empire_Ships : Empire_Ships_Maps.Map;

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Add_Order
     (To       : Athena.Handles.Ship.Ship_Class;
      Action   : Athena.Db.Ship_Action;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Long_Float;
      Star     : Athena.Handles.Star.Star_Class);

   function New_Name
     (Base_Name : String;
      Exists    : not null access
        function (Name : String) return Boolean)
      return String;

   ---------------
   -- Add_Order --
   ---------------

   procedure Add_Order
     (To       : Athena.Handles.Ship.Ship_Class;
      Action   : Athena.Db.Ship_Action;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Long_Float;
      Star     : Athena.Handles.Star.Star_Class)
   is
      New_First_Order : Natural := To.First_Order;
      New_Last_Order  : Natural := To.Last_Order;
   begin
      New_Last_Order := New_Last_Order + 1;

      Athena.Logging.Log
        (To.Empire.Name
         & " ship "
         & To.Name
         & ": order" & New_Last_Order'Image
         & ": "
         & Action'Image
         & " "
         & (if Action in Athena.Db.Load | Athena.Db.Unload
           then Cargo'Image
           & " "
           & Image (Quantity)
           else "")
         & (if Star.Has_Element then " " & Star.Name else ""));

      declare
         use Athena.Db;
         Ref : constant Ship_Order_Reference :=
                 Athena.Db.Ship_Order.Get_Reference_By_Ship_Order
                   (To.Reference_Ship, New_Last_Order);
      begin
         if Ref /= Null_Ship_Order_Reference then
            Athena.Db.Ship_Order.Update_Ship_Order (Ref)
              .Set_Action (Action)
              .Set_Cargo (Cargo)
              .Set_Quantity (Quantity)
              .Set_Star (Star.Reference_Star)
              .Done;
         else
            Athena.Handles.Ship_Order.Create
              (Action   => Action,
               Ship     => To,
               Sequence => New_Last_Order,
               Cargo    => Cargo,
               Quantity => Quantity,
               Star     => Star);
         end if;
      end;

      if New_First_Order = 0 then
         New_First_Order := 1;
      end if;

      To.Update_Ship
        .Set_First_Order (New_First_Order)
        .Set_Last_Order (New_Last_Order)
        .Done;

   end Add_Order;

   --------------
   -- Add_Ship --
   --------------

   procedure Add_Ship (Added_Ship : Athena.Handles.Ship.Ship_Class) is

      function Load_Ship
        (Handle : Athena.Handles.Ship.Ship_Class)
         return Ship_Record;

      ---------------
      -- Load_Ship --
      ---------------

      function Load_Ship
        (Handle : Athena.Handles.Ship.Ship_Class)
         return Ship_Record
      is
         Rec : Ship_Record :=
                 Ship_Record'
                   (Handle  => Athena.Handles.Ship.Get (Handle.Reference_Ship),
                    Mass    => 0.0,
                    Space   => 0.0,
                    others  => <>);

         procedure Add_Component
           (Component : Athena.Handles.Ship_Component.Ship_Component_Class);

         -------------------
         -- Add_Component --
         -------------------

         procedure Add_Component
           (Component : Athena.Handles.Ship_Component.Ship_Component_Class)
         is
            use all type Athena.Db.Component_Function_Type;
            Comp_Hndl : constant Athena.Handles.Ship_Component
              .Ship_Component_Handle :=
                Athena.Handles.Ship_Component.Get
                  (Component.Reference_Ship_Component);

            Comp_Rec : constant Ship_Component_Record :=
                         Ship_Component_Record'
                           (Component => Comp_Hndl,
                            Mass      => Component.Design_Component.Mass,
                            Tec_Level => Component.Tec_Level,
                            Condition => Component.Condition);

         begin

            Rec.Mass := Rec.Mass + Component.Design_Component.Mass;

            case Component.Design_Component.Component.Class is
               when Drive =>
                  Rec.Drive := Comp_Rec;
               when Shield =>
                  Rec.Shield := Comp_Rec;
               when Cargo =>
                  Rec.Cargo := Comp_Rec;
                  Rec.Space :=
                    Component.Tec_Level
                      * (Component.Design_Component.Mass
                         + Component.Design_Component.Mass ** 2 / 20.0);
               when Repair =>
                  Rec.Repair := Comp_Rec;
               when Beam =>
                  Rec.Weapons.Append (Comp_Rec);
               when Missile =>
                  Rec.Weapons.Append (Comp_Rec);
               when Fighter =>
                  Rec.Weapons.Append (Comp_Rec);
            end case;
         end Add_Component;

      begin
         declare
            use Athena.Handles.Ship_Component.Selections;
         begin
            for Component of Select_Where (Ship = Handle) loop
               Add_Component (Component);
            end loop;
         end;

         return Rec;
      end Load_Ship;

      Rec   : constant Ship_Record := Load_Ship (Added_Ship);
      Owner : constant String := Added_Ship.Empire.Identifier;
   begin
      Ship_Vector.Append (Rec);
      All_Ships.Insert (Added_Ship.Identifier, Ship_Vector.Last_Index);
      if not Empire_Ships.Contains (Owner) then
         Empire_Ships.Insert (Owner, Ship_Maps.Empty_Map);
      end if;
      Empire_Ships (Owner).Insert
        (Added_Ship.Identifier, Ship_Vector.Last_Index);

   end Add_Ship;

   ---------------------
   -- Available_Space --
   ---------------------

   function Available_Space
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real
   is
   begin
      return Total_Cargo_Space (Of_Ship)
        - Of_Ship.Colonists - Of_Ship.Material - Of_Ship.Industry;
   end Available_Space;

   -------------------
   -- Current_Cargo --
   -------------------

   function Current_Cargo
     (Of_Ship : Athena.Handles.Ship.Ship_Class;
      Cargo   : Athena.Db.Cargo_Type)
      return Non_Negative_Real
   is
      use all type Athena.Db.Cargo_Type;
   begin
      return (case Cargo is
                 when Colonists => Of_Ship.Colonists,
                 when Industry  => Of_Ship.Industry,
                 when Material  => Of_Ship.Material);
   end Current_Cargo;

   ------------------
   -- Design_Cargo --
   ------------------

   function Design_Cargo
     (Design : Athena.Handles.Ship_Design.Ship_Design_Class)
      return Non_Negative_Real
   is
      use Athena.Db;
      use Athena.Handles.Design_Component.Selections;
      Handle : Handles.Design_Component.Design_Component_Handle :=
                 Handles.Design_Component.Empty_Handle;
   begin
      for Component of Select_Where (Ship_Design = Design) loop
         if Component.Component.Class = Athena.Db.Cargo then
            Handle := Component;
            exit;
         end if;
      end loop;

      if not Handle.Has_Element then
         return 0.0;
      else
         return Athena.Empires.Current_Tec_Level
           (Design.Empire, Athena.Technology.Cargo)
           * (Handle.Mass + Handle.Mass ** 2 / 20.0);
      end if;
   end Design_Cargo;

   -----------------
   -- Design_Mass --
   -----------------

   function Design_Mass
     (Of_Design : Athena.Handles.Ship_Design.Ship_Design_Class)
      return Non_Negative_Real
   is
      use Athena.Handles.Design_Component.Selections;
      Mass : Non_Negative_Real := 0.0;
   begin
      for Component of Select_Where (Ship_Design = Of_Design) loop
         Mass := Mass + Component.Mass;
      end loop;
      return Mass;
   end Design_Mass;

   --------------------
   -- For_All_Fleets --
   --------------------

   procedure For_All_Fleets
     (Process : not null access
        procedure (Fleet : Athena.Handles.Fleet.Fleet_Class))
   is
   begin
      for Fleet of
        Athena.Handles.Fleet.Selections.Select_All
      loop
         Process (Fleet);
      end loop;
   end For_All_Fleets;

   -------------------
   -- For_All_Ships --
   -------------------

   procedure For_All_Ships
     (Process : not null access
        procedure (Ship : Athena.Handles.Ship.Ship_Class))
   is
   begin
      for Ship of Ship_Vector loop
         Process (Ship.Handle);
      end loop;
   end For_All_Ships;

   -------------------
   -- For_All_Ships --
   -------------------

   procedure For_All_Ships
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Process  : not null access
        procedure (Ship : Athena.Handles.Ship.Ship_Class))
   is
   begin
      for Ref of Empire_Ships (Owned_By.Identifier) loop
         Process (Ship_Vector (Ref).Handle);
      end loop;
   end For_All_Ships;

   ---------------
   -- Get_Drive --
   ---------------

   function Get_Drive
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Athena.Handles.Ship_Component.Ship_Component_Class
   is
   begin
      return Ship_Vector (All_Ships.Element (Of_Ship.Identifier))
        .Drive.Component;
   end Get_Drive;

   ---------------
   -- Get_Ships --
   ---------------

   procedure Get_Ships
     (In_Fleet : Athena.Handles.Fleet.Fleet_Class;
      Ships    : out Ship_Lists.List)
   is
      use Athena.Handles.Ship.Selections;
   begin
      Ships.Clear;
      for Ship of Select_Where (Fleet = In_Fleet) loop
         Ships.Append (Ship);
      end loop;
   end Get_Ships;

   --------------
   -- Is_Armed --
   --------------

   function Is_Armed
     (Ship : Athena.Handles.Ship.Ship_Class)
      return Boolean
   is
   begin
      return not Ship_Vector (All_Ships.Element (Ship.Identifier))
        .Weapons.Is_Empty;
   end Is_Armed;

   ------------------------
   -- Iterate_Components --
   ------------------------

   procedure Iterate_Components
     (On_Ship : Athena.Handles.Ship.Ship_Class;
      Process : not null access
        procedure
          (Component : Athena.Handles.Ship_Component.Ship_Component_Class))
   is
      Rec : constant Ship_Record :=
              Ship_Vector.Element (All_Ships (On_Ship.Identifier));
   begin
      Process (Rec.Drive.Component);
      Process (Rec.Shield.Component);
      Process (Rec.Cargo.Component);
      Process (Rec.Repair.Component);

      for Component of Rec.Weapons loop
         Process (Component.Component);
      end loop;
   end Iterate_Components;

   ----------
   -- Load --
   ----------

   procedure Load
     (Actor    : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
   is
   begin
      Add_Order
        (To       => Actor,
         Action   => Athena.Db.Load,
         Cargo    => Cargo,
         Quantity => Quantity,
         Star     => Athena.Handles.Star.Empty_Handle);
   end Load;

   ----------------
   -- Load_Ships --
   ----------------

   procedure Load_Ships is

   begin
      Ada.Text_IO.Put ("Loading ships ...");
      Ada.Text_IO.Flush;

      for Ship of Athena.Handles.Ship.Selections.Select_All loop
         Add_Ship (Ship);
      end loop;

      Ada.Text_IO.Put_Line (" done");

   end Load_Ships;

   ----------
   -- Mass --
   ----------

   function Mass
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real
   is
   begin
      return Ship_Vector (All_Ships.Element (Of_Ship.Identifier)).Mass
        + Of_Ship.Material + Of_Ship.Colonists + Of_Ship.Industry;
   end Mass;

   -------------
   -- Move_To --
   -------------

   procedure Move_To
     (Actor : Athena.Handles.Ship.Ship_Class;
      Star  : Athena.Handles.Star.Star_Class)
   is
   begin
      Add_Order
        (To       => Actor,
         Action   => Athena.Db.Move,
         Cargo    => Athena.Db.Colonists,
         Quantity => 0.0,
         Star     => Star);
   end Move_To;

   ----------------
   -- Name_Fleet --
   ----------------

   function Name_Fleet
     (Owner     : Athena.Handles.Empire.Empire_Class;
      Base_Name : String)
      return String
   is
      function Exists (Name : String) return Boolean;

      ------------
      -- Exists --
      ------------

      function Exists (Name : String) return Boolean is
         use type Athena.Db.Empire_Reference;
      begin
         for Fleet of Athena.Db.Fleet.Select_By_Name (Name) loop
            if Fleet.Empire = Owner.Reference_Empire then
               return True;
            end if;
         end loop;
         return False;
      end Exists;

   begin
      return New_Name (Base_Name, Exists'Access);
   end Name_Fleet;

   ---------------
   -- Name_Ship --
   ---------------

   function Name_Ship
     (Owner     : Athena.Handles.Empire.Empire_Class;
      Base_Name : String)
      return String
   is

      function Exists (Name : String) return Boolean;

      ------------
      -- Exists --
      ------------

      function Exists (Name : String) return Boolean is
         use type Athena.Db.Empire_Reference;
      begin
         for Ship of Athena.Db.Ship.Select_By_Name (Name) loop
            if Ship.Empire = Owner.Reference_Empire then
               return True;
            end if;
         end loop;
         return False;
      end Exists;

   begin
      return New_Name (Base_Name, Exists'Access);
   end Name_Ship;

   --------------
   -- New_Name --
   --------------

   function New_Name
     (Base_Name : String;
      Exists    : not null access
        function (Name : String) return Boolean)
      return String
   is
      Index : Positive := 1;
   begin
      loop
         declare
            Name  : constant String :=
                      Base_Name & " " & WL.Numerics.Roman.Roman_Image (Index);
         begin
            if not Exists (Name) then
               return Name;
            end if;
         end;

         Index := Index + 1;
      end loop;
   end New_Name;

   ----------------
   -- On_Arrival --
   ----------------

   procedure On_Arrival
     (Arriving_Ship : Athena.Handles.Ship.Ship_Class)
   is
      use Athena.Handles.Star_Knowledge;
      use Athena.Handles.Star_Knowledge.Selections;
      Knowledge : constant Star_Knowledge_Handle :=
                    First_Where (Empire = Arriving_Ship.Empire
                                  and Star = Arriving_Ship.Star);
   begin
      if not Knowledge.Has_Element then
         Create (Arriving_Ship.Star, Arriving_Ship.Empire, True, False);
      elsif not Knowledge.Visited then
         Knowledge.Update_Star_Knowledge
           .Set_Visited (True)
           .Done;
      end if;
   end On_Arrival;

   --------------------------
   -- Select_Managed_Ships --
   --------------------------

   function Select_Managed_Ships
     (Managed_By : Athena.Handles.Empire_Manager.Empire_Manager_Class)
      return Athena.Handles.Ship.Selections.Selection
   is
      use Athena.Handles.Ship.Selections;
   begin
      return Select_Where (Manager = Managed_By);
   end Select_Managed_Ships;

   -----------
   -- Speed --
   -----------

   function Speed
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real
   is
      Drive : constant Athena.Handles.Ship_Component.Ship_Component_Class :=
                Get_Drive (Of_Ship);
   begin
      if Drive.Has_Element then
         return 5.0 * Drive.Condition * Drive.Tec_Level
           * Drive.Design_Component.Mass
           / Mass (Of_Ship);
      else
         return 0.0;
      end if;
   end Speed;

   -----------------------
   -- Total_Cargo_Space --
   -----------------------

   function Total_Cargo_Space
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real
   is
   begin
      return Ship_Vector (All_Ships.Element (Of_Ship.Identifier)).Space;
   end Total_Cargo_Space;

   ------------
   -- Unload --
   ------------

   procedure Unload
     (Actor    : Athena.Handles.Ship.Ship_Class;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Non_Negative_Real)
   is
   begin
      Add_Order
        (To       => Actor,
         Action   => Athena.Db.Unload,
         Cargo    => Cargo,
         Quantity => Quantity,
         Star     => Athena.Handles.Star.Empty_Handle);
   end Unload;

   procedure Upgrade_Component
     (Component     : Athena.Handles.Ship_Component.Ship_Component_Class;
      New_Tec_Level : Non_Negative_Real)
   is
      use all type Athena.Db.Component_Function_Type;
      Rec : Ship_Record renames
              Ship_Vector (All_Ships (Component.Ship.Identifier));
   begin

      Component.Update_Ship_Component
        .Set_Tec_Level (New_Tec_Level)
        .Done;

      case Component.Design_Component.Component.Class is
         when Drive =>
            Rec.Drive.Tec_Level := New_Tec_Level;
         when Shield =>
            Rec.Shield.Tec_Level := New_Tec_Level;
         when Cargo =>
            Rec.Cargo.Tec_Level := New_Tec_Level;
            Rec.Space :=
              New_Tec_Level
                * (Component.Design_Component.Mass
                   + Component.Design_Component.Mass ** 2 / 20.0);
         when Repair =>
            Rec.Repair.Tec_Level := New_Tec_Level;
         when Beam | Missile | Fighter =>
            for Item of Rec.Weapons loop
               if Item.Component.Identifier = Component.Identifier then
                  Item.Tec_Level := New_Tec_Level;
                  exit;
               end if;
            end loop;
      end case;

   end Upgrade_Component;

end Athena.Ships;
