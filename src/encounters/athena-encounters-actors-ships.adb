with Ada.Containers.Doubly_Linked_Lists;

with Nazar.Colors;

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
                               Weapon.Design_Component.Mass * Condition * 0.02,
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

   begin
      return new Ship_Actor_Type'
        (Index               => Index,
         Class               => Ship_Actor,
         Mass                => Athena.Ships.Mass (Ship),
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
         Target   => Ship.Location,
         Color    => To_Nazar_Color (Ship.Owner.Rgb),
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
      return Ship.Ship.Identifier
        & " " & Ship.Ship.Name
        & ": location (" & Image (Ship.Location.X)
        & "," & Image (Ship.Location.Y)
        & "); heading "
        & Image (Athena.Trigonometry.To_Degrees (Ship.Heading))
        & "; speed " & Image (Ship.Speed)
        & "/" & Image (Athena.Ships.Speed (Ship.Ship))
        & (if Ship.Have_Destination
           then "; destination (" & Image (Ship.Destination.X)
           & "," & Image (Ship.Destination.Y) & ")"
           else "");
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
         if Weapon.Class = Beam then
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
