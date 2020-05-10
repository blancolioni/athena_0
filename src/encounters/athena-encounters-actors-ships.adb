with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Numerics;

with Nazar.Colors;

with Athena.Elementary_Functions;
with Athena.Logging;
with Athena.Random;

with Athena.Ships;

with Athena.Handles.Ship_Component;

with Athena.Db;

package body Athena.Encounters.Actors.Ships is

   type Weapon_Class is (Beam, Missile, Fighter);

   type Weapon_Record is
      record
         Class       : Weapon_Class;
         Component   : Athena.Handles.Ship_Component.Ship_Component_Handle;
         Max_Charge  : Non_Negative_Real;
         Charge      : Non_Negative_Real;
         Charge_Rate : Non_Negative_Real;
         Condition   : Unit_Real;
         Remaining   : Natural;
      end record;

   package Weapon_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Weapon_Record);

   type Ship_Actor_Type is
     new Root_Actor_Type with
      record
         Max_Shield     : Non_Negative_Real;
         Current_Shield : Non_Negative_Real;
         Weapons        : Weapon_Lists.List;
      end record;

   overriding function Current_Sprite
     (Ship : Ship_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type;

   overriding function Image
     (Ship : Ship_Actor_Type)
      return String;

   overriding procedure Update
     (Ship : in out Ship_Actor_Type);

   overriding procedure Iterate_Beam_Weapons
     (Ship : Ship_Actor_Type;
      Process : not null access
        procedure (Component : Athena.Handles.Ship_Component
                   .Ship_Component_Class;
                   Charge    : Unit_Real));

   overriding procedure Weapon_Fired
     (Ship   : in out Ship_Actor_Type;
      Weapon : Athena.Handles.Ship_Component.Ship_Component_Class);

   overriding procedure Apply_Hit
     (Ship : in out Ship_Actor_Type;
      Hit  : Hit_Record);

   procedure Apply_Hull_Damage
     (Ship   : in out Ship_Actor_Type'Class;
      Damage : Non_Negative_Real);

   function Choose_Mass_Weighted_Component
     (Ship : Athena.Handles.Ship.Ship_Class)
      return Athena.Handles.Ship_Component.Ship_Component_Class;

   function To_Nazar_Color
     (Value : Natural)
      return Nazar.Colors.Nazar_Color
   is (Nazar.Colors.Nazar_Color'
         (Red   =>
             Nazar.Nazar_Unit_Float (Real (Value / 65536 mod 256) / 255.0),
          Green =>
             Nazar.Nazar_Unit_Float (Real (Value / 256 mod 256) / 255.0),
          Blue  =>
             Nazar.Nazar_Unit_Float (Real (Value mod 256) / 255.0),
          Alpha => 1.0));

   ---------------
   -- Apply_Hit --
   ---------------

   overriding procedure Apply_Hit
     (Ship : in out Ship_Actor_Type;
      Hit  : Hit_Record)
   is
      Shield_Level  : constant Unit_Real :=
                        (if Ship.Max_Shield = 0.0 then 0.0
                         else Ship.Current_Shield / Ship.Max_Shield);
      Shield_Damage : constant Non_Negative_Real :=
                        Real'Min (Hit.Damage * Shield_Level,
                                  Ship.Current_Shield);
      Hull_Damage   : constant Non_Negative_Real :=
                        Hit.Damage - Shield_Damage;
   begin
      Ship.Current_Shield := Ship.Current_Shield - Shield_Damage;
      if Shield_Damage > 0.0 then
         Athena.Logging.Log
           (Ship.Image & ": shields now "
            & Image (Ship.Current_Shield)
            & "/" & Image (Ship.Max_Shield));
      end if;

      if Hull_Damage > 0.0 then
         Apply_Hull_Damage (Ship, Hull_Damage);
      end if;
   end Apply_Hit;

   -----------------------
   -- Apply_Hull_Damage --
   -----------------------

   procedure Apply_Hull_Damage
     (Ship   : in out Ship_Actor_Type'Class;
      Damage : Non_Negative_Real)
   is
      Remaining : Non_Negative_Real := Damage;
      Destroyed : Boolean := False;
   begin
      while Remaining > 0.0 loop
         declare
            use Athena.Handles.Ship_Component;
            Component : constant Ship_Component_Class :=
                          Choose_Mass_Weighted_Component
                            (Ship.Ship);
         begin
            if not Component.Has_Element then
               Destroyed := True;
               exit;
            end if;

            declare
               Mass : constant Non_Negative_Real :=
                        Component.Design_Component.Mass;
               Old_Damage : constant Non_Negative_Real := Component.Damage;
               Available  : constant Non_Negative_Real :=
                              Mass - Old_Damage;
               Applied    : constant Non_Negative_Real :=
                              Real'Min (Remaining, Available);
               New_Damage : constant Non_Negative_Real :=
                              Old_Damage + Applied;
               New_Condition : constant Unit_Real :=
                                 (1.0 - (New_Damage / Mass)) ** 2;
            begin
               Component.Update_Ship_Component
                 .Set_Damage (New_Damage)
                 .Set_Condition (New_Condition)
                 .Done;

               if New_Condition > 0.0 then
                  Athena.Logging.Log
                    (Ship.Image
                     & ": damage to "
                     & Component.Component.Tag
                     & "; condition now "
                     & Image (New_Condition * 100.0) & "%");
               else
                  Athena.Logging.Log
                    (Ship.Image
                     & ": "
                     & Component.Component.Tag
                     & " destroyed!");
               end if;

               Remaining := Remaining - Applied;
            end;
         end;
      end loop;

      Ship.Speed := Real'Min (Ship.Speed, Athena.Ships.Speed (Ship.Ship));

      if Destroyed then
         Athena.Logging.Log
           (Ship.Image & ": destroyed!");
         Ship.Dead := True;
         Athena.Ships.Destroy (Ship.Ship);
--
--           Ship.Ship.Update_Ship
--             .Set_Fleet (Athena.Handles.Fleet.Empty_Handle)
--             .Set_Destination (Athena.Handles.Star.Empty_Handle)
--             .Done;
      end if;
   end Apply_Hull_Damage;

   ------------------------------------
   -- Choose_Mass_Weighted_Component --
   ------------------------------------

   function Choose_Mass_Weighted_Component
     (Ship : Athena.Handles.Ship.Ship_Class)
      return Athena.Handles.Ship_Component.Ship_Component_Class
   is
      Total_Mass : Non_Negative_Real := 0.0;

      package Component_Lists is
        new Ada.Containers.Indefinite_Doubly_Linked_Lists
          (Athena.Handles.Ship_Component.Ship_Component_Class,
           Athena.Handles.Ship_Component."=");

      List     : Component_Lists.List;

      procedure Add_Component
        (Component : Athena.Handles.Ship_Component.Ship_Component_Class);

      -------------------
      -- Add_Component --
      -------------------

      procedure Add_Component
        (Component : Athena.Handles.Ship_Component.Ship_Component_Class)
      is
      begin
         if Component.Condition > 0.0 then
            List.Append (Component);
            Total_Mass := Total_Mass + Component.Design_Component.Mass;
         end if;
      end Add_Component;

   begin
      Athena.Ships.Iterate_Components (Ship, Add_Component'Access);

      if Total_Mass = 0.0 then
         return Athena.Handles.Ship_Component.Empty_Handle;
      else
         declare
            R : Non_Negative_Real :=
                  Athena.Random.Unit_Random * Total_Mass;
         begin

            while R > List.First_Element.Design_Component.Mass loop
               R := R - List.First_Element.Design_Component.Mass;
               List.Delete_First;
            end loop;
         end;

         return List.First_Element;
      end if;

   end Choose_Mass_Weighted_Component;

   ------------------
   -- Create_Actor --
   ------------------

   function Create_Actor
     (Index   : Encounter_Actor_Reference;
      Tick    : Encounter_Tick;
      Ship    : Athena.Handles.Ship.Ship_Class;
      X, Y    : Real;
      Heading : Athena.Trigonometry.Angle)
      return Actor_Type
   is
      function Initial_Shield return Non_Negative_Real;
      function Initial_Weapons return Weapon_Lists.List;

      --------------------
      -- Initial_Shield --
      --------------------

      function Initial_Shield return Non_Negative_Real is
         use Athena.Handles.Ship_Component;
         Shield : constant Ship_Component_Class :=
                    Athena.Ships.Get_Shield (Ship);
      begin
         if Shield.Has_Element then
            return Shield.Design_Component.Mass * Shield.Condition
              * Shield.Tec_Level;
         else
            return 0.0;
         end if;
      end Initial_Shield;

      ---------------------
      -- Initial_Weapons --
      ---------------------

      function Initial_Weapons return Weapon_Lists.List is

         List : Weapon_Lists.List;

         procedure Add_Weapon
           (Weapon : Athena.Handles.Ship_Component.Ship_Component_Class);

         ----------------
         -- Add_Weapon --
         ----------------

         procedure Add_Weapon
           (Weapon : Athena.Handles.Ship_Component.Ship_Component_Class)
         is
            Class : constant Weapon_Class :=
                      (case Weapon.Component.Class is
                          when Athena.Db.Beam    => Beam,
                          when Athena.Db.Missile => Missile,
                          when Athena.Db.Fighter => Fighter,
                          when others            =>
                             raise Constraint_Error with
                       Weapon.Component.Class'Image &
                         " is not a known weapon class");

            Condition : constant Unit_Real :=
                          Weapon.Condition;
            Rec       : constant Weapon_Record :=
                          Weapon_Record'
                            (Class       => Class,
                             Component   =>
                               Athena.Handles.Ship_Component.Get
                                 (Weapon.Reference_Ship_Component),
                             Max_Charge  =>
                               Weapon.Design_Component.Mass * Condition,
                             Charge      =>
                               Weapon.Design_Component.Mass * Condition,
                             Charge_Rate =>
                               Weapon.Design_Component.Mass
                             * Condition * 0.02 * Weapon.Tec_Level,
                             Condition   => Condition,
                             Remaining   => 100);
         begin
            List.Append (Rec);
         end Add_Weapon;

      begin
         Athena.Ships.Iterate_Weapons
           (Ship, Add_Weapon'Access);
         return List;
      end Initial_Weapons;

      Shield : constant Non_Negative_Real := Initial_Shield;
      Weapons : constant Weapon_Lists.List := Initial_Weapons;

      Pi          : constant := Ada.Numerics.Pi;
      Mass        : constant Non_Negative_Real :=
                      Athena.Ships.Design_Mass (Ship.Ship_Design);
      Radius      : constant Non_Negative_Real :=
                      Athena.Elementary_Functions."**"
                        (3.0 * Mass / 4.0 / Pi, 1.0 / 3.0);
      Target_Size : constant Non_Negative_Real :=
                      Pi * Radius ** 2;
   begin
      return new Ship_Actor_Type'
        (Index               => Index,
         Class               => Ship_Actor,
         Dead                => False,
         Mass                => Athena.Ships.Mass (Ship),
         Size                => Target_Size,
         Location            => (X, Y),
         Heading             => Heading,
         Destination         => (X, Y),
         Have_Destination    => False,
         Target_Heading      => Heading,
         Have_Target_Heading => False,
         Follow              => 1,
         Follow_Range        => 0.0,
         Follow_Bearing      => Athena.Trigonometry.From_Degrees (0.0),
         Is_Following        => False,
         Speed               => 0.0,
         Owner               =>
           Athena.Handles.Empire.Get (Ship.Empire.Reference_Empire),
         Ship                =>
           Athena.Handles.Ship.Get (Ship.Reference_Ship),
         First_Tick          => Tick,
         Last_Tick           => Encounter_Tick'Last,
         Hits                => <>,
         Max_Shield          => Shield,
         Current_Shield      => Shield,
         Weapons             => Weapons);
   end Create_Actor;

   --------------------
   -- Current_Sprite --
   --------------------

   overriding function Current_Sprite
     (Ship : Ship_Actor_Type)
      return Athena.Encounters.Sprites.Sprite_Type
   is
   begin
      return Athena.Encounters.Sprites.Make_Sprite
        (Class    => Ship_Actor,
         Size     => Ship.Mass,
         Location => Ship.Location,
         Heading  => Ship.Heading,
         Target   => (if Ship.Have_Destination
                      or else Ship.Is_Following
                      then Ship.Destination
                      else Ship.Location),
         Color    =>
           (if Ship.Is_Dead
            then (0.4, 0.4, 0.4, 1.0)
            else To_Nazar_Color (Ship.Owner.Rgb)),
         Shield   =>
           (if Ship.Max_Shield > 0.0
            then Ship.Current_Shield / Ship.Max_Shield
            else 0.0));
   end Current_Sprite;

   -----------
   -- Image --
   -----------

   overriding function Image
     (Ship : Ship_Actor_Type)
      return String
   is
   begin
      return Ship.Ship.Empire.Adjective
        & " ship " & Ship.Ship.Identifier
        & " " & Ship.Ship.Name;
--          & ": location (" & Image (Ship.Location.X)
--          & "," & Image (Ship.Location.Y)
--          & "); heading "
--          & Image (Athena.Trigonometry.To_Degrees (Ship.Heading))
--          & "; speed " & Image (Ship.Speed)
--          & "/" & Image (Athena.Ships.Speed (Ship.Ship))
--          & (if Ship.Have_Destination
--             then "; destination (" & Image (Ship.Destination.X)
--             & "," & Image (Ship.Destination.Y) & ")"
--             else "");
   end Image;

   --------------------------
   -- Iterate_Beam_Weapons --
   --------------------------

   overriding procedure Iterate_Beam_Weapons
     (Ship    : Ship_Actor_Type;
      Process : not null access
        procedure (Component : Athena.Handles.Ship_Component
                   .Ship_Component_Class;
                   Charge    : Unit_Real))
   is
   begin
      for Weapon of Ship.Weapons loop
         if Weapon.Class = Beam
           and then Weapon.Max_Charge > 0.0
         then
            Process (Weapon.Component, Weapon.Charge / Weapon.Max_Charge);
         end if;
      end loop;
   end Iterate_Beam_Weapons;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Ship : in out Ship_Actor_Type)
   is
   begin
      Root_Actor_Type (Ship).Update;
      for Weapon of Ship.Weapons loop
         if Weapon.Charge < Weapon.Max_Charge then
            Weapon.Charge :=
              Real'Min
                (Weapon.Max_Charge, Weapon.Charge + Weapon.Charge_Rate);
         end if;
      end loop;
   end Update;

   ------------------
   -- Weapon_Fired --
   ------------------

   overriding procedure Weapon_Fired
     (Ship   : in out Ship_Actor_Type;
      Weapon : Athena.Handles.Ship_Component.Ship_Component_Class)
   is
   begin
      for Element of Ship.Weapons loop
         if Element.Component.Identifier = Weapon.Identifier then
            Element.Charge := 0.0;
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "cannot find weapon with id " & Weapon.Identifier
        & " on " & Ship.Ship.Empire.Name
        & " ship " & Ship.Ship.Name;

   end Weapon_Fired;

end Athena.Encounters.Actors.Ships;
