with Athena.Logging;
with Athena.Real_Images;
with Athena.Turns;

with Athena.Empires;
with Athena.Stars;

with Athena.Handles.Colony_Order;
with Athena.Handles.Fleet_Order;
with Athena.Handles.Research_Order;
with Athena.Handles.Ship_Build_Order;
with Athena.Handles.Transport_Order;

package body Athena.Orders is

   function Image (X : Real) return String
                   renames Athena.Real_Images.Approximate_Image;

   -----------------
   -- Build_Ships --
   -----------------

   procedure Build_Ships
     (Empire   : Athena.Handles.Empire.Empire_Class;
      Design   : Athena.Handles.Ship_Design.Ship_Design_Class;
      Fleet    : Athena.Handles.Fleet.Fleet_Class;
      Manager  : Athena.Handles.Manager.Manager_Class;
      Send_To  : Athena.Handles.Star.Star_Class;
      Count    : Positive;
      Priority : Positive)
   is
   begin
      Athena.Logging.Log
        (Empire.Name & " orders"
         & (if Count = 1 then " " else Count'Image & " x ")
         & Design.Name);

      for I in 1 .. Count loop
         Athena.Handles.Ship_Build_Order.Create
           (Turn        => Athena.Turns.Current_Turn,
            Empire      => Empire,
            Priority    => Priority,
            Ship_Design => Design,
            Manager     =>
              Athena.Empires.Get_Manager (Empire, Manager),
            Fleet       => Fleet,
            Send_To     => Send_To);
      end loop;

   end Build_Ships;

   ----------------
   -- Move_Cargo --
   ----------------

   procedure Move_Cargo
     (Cargo    : Cargo_Type;
      Quantity : Non_Negative_Real;
      From     : Athena.Handles.Colony.Colony_Class;
      To       : Athena.Handles.Star.Star_Class;
      Priority : Positive)
   is
   begin
      Athena.Logging.Log
        (From.Empire.Name & " colony on "
         & From.Star.Name
         & " ordered to move "
         & Image (Quantity)
         & " "
         & Cargo'Image
         & " to "
         & To.Name);
      Athena.Handles.Transport_Order.Create
        (Turn      => Athena.Turns.Current_Turn,
         Empire    => From.Empire,
         Priority  => Priority,
         From      => From.Star,
         To        => To,
         Cargo     => Cargo,
         Quantity  => Quantity);
   end Move_Cargo;

   ----------------
   -- Move_Fleet --
   ----------------

   procedure Move_Fleet
     (Fleet       : Athena.Handles.Fleet.Fleet_Class;
      Destination : Athena.Handles.Star.Star_Class)
   is
   begin
      Athena.Logging.Log
        (Fleet.Empire.Name & " fleet "
         & Fleet.Name
         & " ordered to move to "
         & Destination.Name);

      Athena.Handles.Fleet_Order.Create
        (Turn        => Athena.Turns.Current_Turn,
         Empire      => Fleet.Empire,
         Priority    => 10,
         Fleet       => Fleet,
         Destination => Destination);
   end Move_Fleet;

   --------------------
   -- Order_Industry --
   --------------------

   procedure Order_Industry
     (Colony   : Athena.Handles.Colony.Colony_Class;
      Quantity : Non_Negative_Real;
      Priority : Positive)
   is
   begin
      Athena.Logging.Log
        (Colony.Empire.Name & " colony on "
         & Colony.Star.Name
         & " ordered to produce"
         & Natural'Image (Natural (Quantity))
         & " industry");
      Athena.Handles.Colony_Order.Create
        (Turn        => Athena.Turns.Current_Turn,
         Empire      => Colony.Empire,
         Priority    => Priority,
         Colony      => Colony,
         Category    => Athena.Db.Build_Industry,
         Value       => Quantity);
   end Order_Industry;

   -------------------------
   -- Research_Technology --
   -------------------------

   procedure Research_Technology
     (Empire     : Athena.Handles.Empire.Empire_Class;
      Technology : Athena.Handles.Technology.Technology_Class;
      Construct  : Non_Negative_Real;
      Priority   : Positive)
   is
   begin
      Athena.Logging.Log
        (Empire.Name & " invests " & Image (Construct)
         & " construct in " & Technology.Tag & " research");

      Athena.Handles.Research_Order.Create
        (Turn       => Athena.Turns.Current_Turn,
         Empire     => Empire,
         Priority   => Priority,
         Technology => Technology,
         Construct  => Construct);
   end Research_Technology;

   ---------------------
   -- Set_Destination --
   ---------------------

   procedure Set_Destination
     (Ship        : Athena.Handles.Ship.Ship_Class;
      Destination : Athena.Handles.Star.Star_Class;
      Priority    : Positive)
   is
      pragma Unreferenced (Priority);
   begin
      Athena.Logging.Log
        (Ship.Empire.Adjective
         & " ship " & Ship.Name
         & " at " & Ship.Star.Name
         & " ordered to " & Destination.Name
         & " distance "
         & Image (Athena.Stars.Distance (Ship.Star, Destination)));
      Ship.Update_Ship
        .Set_Destination (Destination.Reference_Star)
        .Set_Progress (0.0)
        .Done;
   end Set_Destination;

end Athena.Orders;
