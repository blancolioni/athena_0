with WL.Numerics.Roman;

with Athena.Logging;
with Athena.Real_Images;

with Athena.Empires;
with Athena.Technology;

with Athena.Handles.Design_Component.Selections;
with Athena.Handles.Ship.Selections;
with Athena.Handles.Ship_Component.Selections;
with Athena.Handles.Ship_Order;
with Athena.Handles.Star_Knowledge.Selections;

with Athena.Db.Ship;
with Athena.Db.Ship_Order;

package body Athena.Ships is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   procedure Add_Order
     (To       : Athena.Handles.Ship.Ship_Class;
      Action   : Athena.Db.Ship_Action;
      Cargo    : Athena.Db.Cargo_Type;
      Quantity : Long_Float;
      Star     : Athena.Handles.Star.Star_Class);

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
        (To.Name
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

   -------------------
   -- For_All_Ships --
   -------------------

   procedure For_All_Ships
     (Process : not null access
        procedure (Ship : Athena.Handles.Ship.Ship_Class))
   is
      use Athena.Handles.Ship.Selections;
   begin
      for Ship of Select_All loop
         Process (Ship);
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
      use Athena.Handles.Ship.Selections;
   begin
      for Ship of Select_Where (Empire = Owned_By) loop
         Process (Ship);
      end loop;
   end For_All_Ships;

   ---------------
   -- Get_Drive --
   ---------------

   function Get_Drive
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Athena.Handles.Ship_Component.Ship_Component_Class
   is
      use type Athena.Db.Component_Function_Type;
      use Athena.Handles.Ship_Component.Selections;
   begin
      for Component of Select_Where (Ship = Of_Ship) loop
         if Component.Component.Class = Athena.Db.Drive then
            return Component;
         end if;
      end loop;
      return Athena.Handles.Ship_Component.Empty_Handle;
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

   ------------------------
   -- Iterate_Components --
   ------------------------

   procedure Iterate_Components
     (On_Ship : Athena.Handles.Ship.Ship_Class;
      Process : not null access
        procedure
          (Component : Athena.Handles.Ship_Component.Ship_Component_Class))
   is
      use Athena.Handles.Ship_Component.Selections;
   begin
      for Component of Select_Where (Ship = On_Ship) loop
         Process (Component);
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

   ----------
   -- Mass --
   ----------

   function Mass
     (Of_Ship : Athena.Handles.Ship.Ship_Class)
      return Non_Negative_Real
   is
      use Athena.Handles.Ship_Component.Selections;
      Mass : Non_Negative_Real := 0.0;
   begin
      for Component of Select_Where (Ship = Of_Ship) loop
         Mass := Mass + Component.Design_Component.Mass;
      end loop;
      return Mass + Of_Ship.Material + Of_Ship.Colonists + Of_Ship.Industry;
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

   ---------------
   -- Name_Ship --
   ---------------

   function Name_Ship
     (Owner     : Athena.Handles.Empire.Empire_Class;
      Base_Name : String)
      return String
   is
      Index : Positive := 1;
   begin
      loop
         declare
            use type Athena.Db.Empire_Reference;
            Name  : constant String :=
                      Base_Name & " " & WL.Numerics.Roman.Roman_Image (Index);
            Found : Boolean := False;
         begin
            for Ship of Athena.Db.Ship.Select_By_Name (Name) loop
               if Ship.Empire = Owner.Reference_Empire then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               return Name;
            end if;
            Index := Index + 1;
         end;
      end loop;
   end Name_Ship;

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
      use type Athena.Db.Component_Function_Type;
      use Athena.Handles.Ship_Component.Selections;
      Handle : Handles.Ship_Component.Ship_Component_Handle :=
                 Handles.Ship_Component.Empty_Handle;
   begin
      for Component of Select_Where (Ship = Of_Ship) loop
         if Component.Component.Class = Athena.Db.Cargo then
            Handle := Component;
            exit;
         end if;
      end loop;

      if not Handle.Has_Element then
         return 0.0;
      else
         return Handle.Tec_Level
           * (Handle.Design_Component.Mass
              + Handle.Design_Component.Mass ** 2 / 20.0);
      end if;
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

end Athena.Ships;
