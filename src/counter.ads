-- $Id: counter.ads 7 2010-06-09 15:09:17Z gmd $

package Counter
is

   procedure Initialize;

   procedure Increment_Counter;

   function Get_Count return Natural;

   protected type Count
   is
      procedure Initialize;
      procedure Increment;
      function Get_Count_Value return Natural;
   private
      Value : Natural;
   end Count;


private

   The_Count : Count;

end Counter;
