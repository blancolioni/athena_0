package body Athena.Encounters.Situation is

   -------------------
   -- Allied_Centre --
   -------------------

   function Allied_Centre
     (Situation : Situation_Interface'Class) return Encounter_Point
   is
      Total_Mass : Non_Negative_Real := 0.0;
      Sum_X      : Real := 0.0;
      Sum_Y      : Real := 0.0;

      procedure Add_Ally
        (Ally : Athena.Encounters.Situation.Situation_Actor);

      --------------
      -- Add_Ally --
      --------------

      procedure Add_Ally
        (Ally : Athena.Encounters.Situation.Situation_Actor)
      is
      begin
         Total_Mass := Total_Mass + Ally.Mass;
         Sum_X := Sum_X + Ally.DX;
         Sum_Y := Sum_Y + Ally.DY;
      end Add_Ally;

   begin
      Situation.Iterate_Allies (Add_Ally'Access);
      return (Sum_X / Total_Mass, Sum_Y / Total_Mass);
   end Allied_Centre;

   --------------------
   -- Hostile_Centre --
   --------------------

   function Hostile_Centre
     (Situation : Situation_Interface'Class) return Encounter_Point
   is
      Total_Mass : Non_Negative_Real := 0.0;
      Sum_X      : Real := 0.0;
      Sum_Y      : Real := 0.0;

      procedure Add_Hostile
        (Hostile : Athena.Encounters.Situation.Situation_Actor);

      -----------------
      -- Add_Hostile --
      -----------------

      procedure Add_Hostile
        (Hostile : Athena.Encounters.Situation.Situation_Actor)
      is
      begin
         Total_Mass := Total_Mass + Hostile.Mass;
         Sum_X := Sum_X + Hostile.DX;
         Sum_Y := Sum_Y + Hostile.DY;
      end Add_Hostile;

   begin
      Situation.Iterate_Hostiles (Add_Hostile'Access);
      return (Sum_X / Total_Mass, Sum_Y / Total_Mass);
   end Hostile_Centre;

end Athena.Encounters.Situation;
