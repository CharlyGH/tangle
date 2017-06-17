-- $Id: ta_types.ads 7 2010-06-09 15:09:17Z gmd $

generic

   Max_Row : Positive;

   Max_Column  : Positive;

   Different : Positive;


package Ta_Types is

   Max_Chip : constant Positive := Max_Row * Max_Column;

   type Extended_Row_Type is new Positive range 1 .. (Max_Row+1);

   subtype Row_Type is Extended_Row_Type  range 1 .. Extended_Row_Type(Max_Row);

   type Extended_Column_Type is new Positive range 1 .. (Max_Column+1);

   subtype Column_Type is Extended_Column_Type range 1 .. Extended_Column_Type(Max_Column);


   type Extended_Chip_Id_Type is new Natural range 0 .. (Max_Chip+1);

   subtype Chip_Id_Type is Extended_Chip_Id_Type range 1 .. Extended_Chip_Id_Type(Max_Chip);

   type Extended_Direction_Type is (Before, Up, Left, Down, Right, After);

   subtype Direction_Type is Extended_Direction_Type range Up .. Right;

   Max_Proc : Positive := Different;

   type Extended_Proc_Id_Type is new Natural range 0 .. Max_Proc;

   subtype Proc_Id_Type is Extended_Proc_Id_Type range 1 .. Extended_Proc_Id_Type (Max_Proc);

   type Position_Type is
      record
         Rotation : Direction_Type;
         Instance : Chip_Id_Type;
      end record;

   type Board_Type is array (Row_Type, Column_Type) of Position_Type;

   type Chip_Name_Type is array (Chip_Id_Type) of Character;

   Chip_Name : constant Chip_Name_Type :=
                 ('A', 'B', 'C', 'D', 'E',
                  'F', 'G', 'H', 'I', 'J',
                  'K', 'L', 'M', 'N', 'O',
                  'P', 'Q', 'R', 'S', 'T',
                  'U', 'V', 'W', 'X', 'X');

   type Dir_Name_Type is array (Direction_Type) of Character;

   Direction_Name : Dir_Name_Type := ('^', '<', 'v', '>');

   type Rotation_Type is (None, Left, Full, Right);


end Ta_Types;
