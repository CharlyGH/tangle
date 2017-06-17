-- $Id: solutions.ads 443 2016-07-02 11:41:13Z gmd $
with Ta_Types;
with Ada.Containers.Vectors;

generic


   with package So_Types is new Ta_Types (<>);


package Solutions
is

   use So_Types;


   function Equals (Lb : in Board_Type;
                    Rb : in Board_Type)
                    return Boolean;


   package Board_Vectors is new Ada.Containers.Vectors (Positive, Board_Type, Equals);
   use Board_Vectors;

   procedure Check_Solution (Board : in Board_Type;
                             Is_New : out Boolean);

   protected type Solution
   is
      procedure Check (Board : in Board_Type;
                       Is_New : out Boolean);

   private
      Solution_Tab : Vector;
   end Solution;

--private

   The_Solution : Solution;

end Solutions;
