with Athena.Handles.Relationship.Selections;

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

end Athena.Treaties;
