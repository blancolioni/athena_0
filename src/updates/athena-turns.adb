with Athena.Handles.Turn.Selections;

package body Athena.Turns is

   ------------------
   -- Current_Turn --
   ------------------

   function Current_Turn return Positive is
      use Athena.Handles.Turn.Selections;
   begin
      return First_Where (Current = True).Turn_Number;
   end Current_Turn;

   ------------------
   -- Current_Turn --
   ------------------

   function Current_Turn return Athena.Handles.Turn.Turn_Class is
      use Athena.Handles.Turn.Selections;
   begin
      return First_Where (Current = True);
   end Current_Turn;

   ------------------------
   -- Current_Turn_Image --
   ------------------------

   function Current_Turn_Image return String is
      Image : String := "0000";
      Raw   : constant String := Positive'Image (Current_Turn);
      Index : Natural := Image'Last;
   begin
      for Ch of reverse Raw loop
         exit when Ch not in '0' .. '9';
         Image (Index) := Ch;
         Index := Index - 1;
      end loop;
      return Image;
   end Current_Turn_Image;

   ---------------
   -- Next_Turn --
   ---------------

   procedure Next_Turn is
      use Athena.Handles.Turn.Selections;
      Previous : constant Athena.Handles.Turn.Turn_Handle :=
                   First_Where (Current = True);
   begin
      Previous.Update_Turn.Set_Current (False).Done;
      Athena.Handles.Turn.Create
        (Turn_Number => Previous.Turn_Number + 1,
         Current     => True);
   end Next_Turn;

   -------------------
   -- Previous_Turn --
   -------------------

   function Previous_Turn return Athena.Handles.Turn.Turn_Class is
      use Athena.Handles.Turn.Selections;
      Current : constant Positive := Current_Turn;
   begin
      if Current = 1 then
         return Athena.Handles.Turn.Empty_Handle;
      else
         return First_Where (Turn_Number = Current_Turn - 1);
      end if;
   end Previous_Turn;

end Athena.Turns;
