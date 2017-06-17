-- $Id: solutions.adb 443 2016-07-02 11:41:13Z gmd $

package body Solutions is


   function Equals (Lb : in Board_Type;
                    Rb : in Board_Type)
                    return Boolean
   is
   begin
      for Row in Row_Type loop
         for Col in Column_Type loop
            if Chip_Name(Lb (Row, Col).Instance) /= Chip_Name(Rb (Row, Col).Instance) or else
            Lb (Row, Col).Rotation /= Rb (Row, Col).Rotation then
               return False;
            end if;
         end loop;
      end loop;
      return True;
   end Equals;



   procedure Check_Solution (Board : in Board_Type;
                             Is_New : out Boolean)
   is
   begin
      The_Solution.Check (Board, Is_New);
   end Check_Solution;


   protected body Solution is
      procedure Check (Board : in Board_Type;
                       Is_New : out Boolean)
      is
      begin
         if not Contains (Solution_Tab, Board) then
            Is_New := True;
            Append (Solution_Tab, Board);
         else
            Is_New := False;
         end if;
      end Check;

   end Solution;

end Solutions;
