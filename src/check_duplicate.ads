-- $Id: check_duplicate.ads 7 2010-06-09 15:09:17Z gmd $

with Ta_Types;

generic

   with package Pa_Types is new Ta_Types (<>);

package Check_Duplicate
  is

   use Pa_Types;

   function Minimum (Le : in Chip_Id_Type;
                     Ri : in Chip_Id_Type)
                     return Chip_Id_Type;


   function Rotate (Dir : in Direction_Type;
                    Rot : in Rotation_Type)
                    return Direction_Type;


   function Id (Inp : in Row_Type)
                return Row_Type;
   pragma Inline (Id);

   function Id (Inp : in Row_Type)
                 return Column_Type;
   pragma Inline (Id);

   function Id (Inp : in Column_Type)
                 return Row_Type;
   pragma Inline (Id);

   function Id (Inp : in Column_Type)
                return Column_Type;
   pragma Inline (Id);

   function Inv (Inp : in Row_Type)
                 return Row_Type;

   function Inv (Inp : in Row_Type)
                 return Column_Type;

   function Inv (Inp : in Column_Type)
                 return Row_Type;

   function Inv (Inp : in Column_Type)
                 return Column_Type;


   procedure Get_Rotation (Board : in Board_Type;
                           Rot   : out Rotation_Type);


   procedure Rotate_Board (Board : in Board_Type;
                           New_Board : out Board_Type;
                           Rot   : in Rotation_Type);


   procedure Normalize_Board (Board : in Board_Type;
                              New_Board : out Board_Type);



end Check_Duplicate;

