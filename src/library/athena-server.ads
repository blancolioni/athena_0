package Athena.Server is

   procedure Initialize;

   procedure Create_Scenario;

   procedure Add_Empire
     (Name      : String;
      Plural    : String;
      Adjective : String;
      Capital   : String;
      Color     : String);

   procedure Load;
   procedure Save;

end Athena.Server;
