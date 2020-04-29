with Athena.Handles.Colony;
with Athena.Handles.Empire;
with Athena.Handles.Star;

package Athena.Colonies is

   procedure Produce_Material
     (Colony   : Athena.Handles.Colony.Colony_Class;
      Quantity : Non_Negative_Real);

   function Can_Provide
     (Colony      : Athena.Handles.Colony.Colony_Class;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0)
      return Boolean;

   procedure Use_Assets
     (Colony      : Athena.Handles.Colony.Colony_Class;
      Construct   : Non_Negative_Real := 0.0;
      Material    : Non_Negative_Real := 0.0;
      Description : String);

   procedure For_All_Colonies
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Process  : not null access
        procedure (Colony : Athena.Handles.Colony.Colony_Class));

   function Find_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Test     : not null access
        function (Colony : Athena.Handles.Colony.Colony_Class) return Boolean)
      return Athena.Handles.Colony.Colony_Class;

   function Best_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      Score    : not null access
        function (Colony : Athena.Handles.Colony.Colony_Class) return Real)
      return Athena.Handles.Colony.Colony_Class;

   function Nearest_Colony
     (Owned_By : Athena.Handles.Empire.Empire_Class;
      To_Star  : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class;

   function Get_Colony
     (At_Star : Athena.Handles.Star.Star_Class)
      return Athena.Handles.Colony.Colony_Class;

   procedure New_Colony
     (At_Star : Athena.Handles.Star.Star_Class;
      Owner   : Athena.Handles.Empire.Empire_Class;
      Pop     : Non_Negative_Real;
      Ind     : Non_Negative_Real;
      Mat     : Non_Negative_Real);

   procedure Load_Colonies;

end Athena.Colonies;
