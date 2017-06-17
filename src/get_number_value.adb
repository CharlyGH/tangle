-- $Id: get_number_value.adb 7 2010-06-09 15:09:17Z gmd $


function Get_Number_Value (Strg : in String;
                           Default : in Number_type) return Number_Type
is
   Num : Number_Type;
begin
   begin
      Num := Number_Type'Value(Strg);
   exception
      when others =>
         Num := Default;
   end;
   return Num;

end Get_Number_Value;


