-- $Id: counter.adb 7 2010-06-09 15:09:17Z gmd $

package body Counter is

   procedure Initialize
   is
   begin
      The_Count.Initialize;
   end Initialize;


   function Get_Count return Natural
   is
   begin
      return The_Count.Get_Count_Value;
   end Get_Count;


   procedure Increment_Counter
   is
   begin
      The_Count.Increment;
   end Increment_Counter;


   protected body Count
     is
     procedure Initialize
     is
     begin
        Value := 0;
     end Initialize;

     procedure Increment
     is
     begin
        Value := Natural'Succ(Value);
     end Increment;

     function Get_Count_Value return Natural
     is
     begin
        return Value;
     end Get_Count_Value;

   end Count;


end Counter;
