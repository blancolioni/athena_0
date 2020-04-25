with Athena.Handles.Empire;
with Athena.Handles.Ship_Design;

package Athena.Configure.Designs is

   function Load_Design
     (Empire : Athena.Handles.Empire.Empire_Class;
      Name   : String)
      return Athena.Handles.Ship_Design.Ship_Design_Class;

end Athena.Configure.Designs;
