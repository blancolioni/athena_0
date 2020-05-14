with Athena.Logging;

with Athena.Turns;

with Athena.Handles.Relationship.Selections;
with Athena.Handles.Turn;

with Athena.Db.War;

package body Athena.Treaties is

   function Get_Relationship
     (From_Empire, To_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Relationship.Relationship_Class;

   ------------
   -- At_War --
   ------------

   function At_War (E1, E2 : Athena.Handles.Empire.Empire_Class) return Boolean
   is
   begin
      return Get_Relationship (E1, E2).War;
   end At_War;

   -----------------
   -- Declare_War --
   -----------------

   procedure Declare_War
     (E1, E2 : Athena.Handles.Empire.Empire_Class)
   is
      R_To   : constant Athena.Handles.Relationship.Relationship_Class :=
                 Get_Relationship (E1, E2);
      R_From : constant Athena.Handles.Relationship.Relationship_Class :=
                 Get_Relationship (E2, E1);
      New_Opinion : constant Integer :=
                      R_From.Opinion - 100;
   begin
      R_To.Update_Relationship
        .Set_War (True)
        .Done;
      R_From.Update_Relationship
        .Set_War (True)
        .Set_Opinion (New_Opinion)
        .Done;

      Athena.Handles.War.Create
        (Attacker => E1,
         Defender => E2,
         Start    => Athena.Turns.Current_Turn,
         Finish   => Athena.Handles.Turn.Empty_Handle);

      Athena.Logging.Log
        (E1.Name & " declared war on " & E2.Name);
   end Declare_War;

   ----------------------
   -- Get_Relationship --
   ----------------------

   function Get_Relationship
     (From_Empire, To_Empire : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.Relationship.Relationship_Class
   is
      use Athena.Handles.Relationship.Selections;
      Relationship : constant Athena.Handles.Relationship.Relationship_Class :=
                       First_Where (From = From_Empire and To = To_Empire);
   begin
      if Relationship.Has_Element then
         return Relationship;
      else
         return Athena.Handles.Relationship.Create
           (From    => From_Empire,
            To      => To_Empire,
            Opinion => 0,
            War     => False,
            Hostile => False,
            Allied  => False,
            Trade   => False);
      end if;
   end Get_Relationship;

   -------------
   -- Get_War --
   -------------

   function Get_War
     (E1, E2 : Athena.Handles.Empire.Empire_Class)
      return Athena.Handles.War.War_Class
   is
      War_1 : constant Athena.Db.War.War_Type :=
                Athena.Db.War.Get_By_War
                  (E1.Reference_Empire, E2.Reference_Empire,
                   Athena.Db.Null_Turn_Reference);
      War_2 : constant Athena.Db.War.War_Type :=
                Athena.Db.War.Get_By_War
                  (E2.Reference_Empire, E1.Reference_Empire,
                   Athena.Db.Null_Turn_Reference);
      War_Ref : constant Athena.Db.War_Reference :=
                  (if War_1.Has_Element
                   then War_1.Get_War_Reference
                   else War_2.Get_War_Reference);
   begin
      if not War_1.Has_Element and then not War_2.Has_Element then
         Athena.Logging.Log
           ("warning: encounter but no war between "
            & E1.Name & " and " & E2.Name);
      end if;

      return Athena.Handles.War.Get (War_Ref);
   end Get_War;

end Athena.Treaties;
