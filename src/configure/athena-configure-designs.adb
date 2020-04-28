with Ada.Characters.Handling;

with Tropos.Reader;

with Athena.Identifiers;

with Athena.Handles.Component.Selections;
with Athena.Handles.Design_Component;

with Athena.Paths;

package body Athena.Configure.Designs is

   function To_Name (Tag : String) return String;

   -----------------
   -- Load_Design --
   -----------------

   function Load_Design
     (Empire : Athena.Handles.Empire.Empire_Class;
      Name   : String)
      return Athena.Handles.Ship_Design.Ship_Design_Class
   is
      Config : constant Tropos.Configuration :=
                 Tropos.Reader.Read_Config
                   (Path =>
                      Athena.Paths.Config_File
                        ("designs/" & Name & ".design"));
      Id : constant Athena.Identifiers.Object_Identifier :=
             Athena.Identifiers.Next_Identifier;
      Design : constant Athena.Handles.Ship_Design.Ship_Design_Handle :=
                 Athena.Handles.Ship_Design.Create
                   (Identifier => Id,
                    Name       => To_Name (Name),
                    Empire     => Empire);

      function Get_Component
        (T : String)
         return Athena.Handles.Component.Component_Handle;

      procedure Create_Component
        (Config : Tropos.Configuration);

      ----------------------
      -- Create_Component --
      ----------------------

      procedure Create_Component
        (Config : Tropos.Configuration)
      is
         Mass  : constant Long_Float := Config.Get (1);
      begin
         Athena.Handles.Design_Component.Create
           (Ship_Design => Design,
            Component   => Get_Component (Config.Config_Name),
            Mass        => Mass);
      end Create_Component;

      ---------------
      -- Component --
      ---------------

      function Get_Component
        (T : String)
         return Athena.Handles.Component.Component_Handle
      is
         use Athena.Handles.Component.Selections;
      begin
         return First_Where (Tag = T);
      end Get_Component;

   begin
      for Component_Config of Config loop
         Create_Component (Component_Config);
      end loop;

      return Design;

   end Load_Design;

   -------------
   -- To_Name --
   -------------

   function To_Name (Tag : String) return String is
      Capitalize : Boolean := True;
   begin
      return Name : String := Tag do
         for Ch of Name loop
            if Capitalize then
               Ch := Ada.Characters.Handling.To_Upper (Ch);
               Capitalize := False;
            elsif Ch = ' ' or else Ch = '_' then
               Capitalize := True;
            end if;
         end loop;
      end return;
   end To_Name;

end Athena.Configure.Designs;
