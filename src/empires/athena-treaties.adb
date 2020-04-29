package body Athena.Treaties is

   ------------
   -- At_War --
   ------------

   function At_War (E1, E2 : Athena.Handles.Empire.Empire_Class) return Boolean
   is
   begin
      --  ALWAYS WAR!!!
      return E1.Identifier /= E2.Identifier;
   end At_War;

end Athena.Treaties;
