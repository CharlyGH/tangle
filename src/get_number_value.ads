-- $Id: get_number_value.ads 7 2010-06-09 15:09:17Z gmd $


generic
   type Number_Type is (<>);

function Get_Number_Value (Strg : in String;
                           Default : in Number_Type) return Number_Type;


