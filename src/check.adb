-- $Id: check.adb 7 2010-06-09 15:09:17Z gmd $

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ta_Types;
with Check_Duplicate;

procedure Check
is

   use Ada.Text_IO;
   use Ada.Characters.Latin_1;

   Data_Error : exception;

   Max_Row : constant Positive := 5;

   Max_Column : constant Positive := 5;

   Different  : constant Positive := 24;

   package Ta_Types_Pkg is new Ta_Types (Max_Row, Max_Column, Different);
   use Ta_Types_Pkg;

   package Check_Dup_Pkg is new Check_Duplicate (Ta_Types_Pkg);
   use Check_Dup_Pkg;

   type Input_Output is (Input, Output);

   procedure Get_Chip_Id (Io   : in Input_Output;
                          Char : in out Character;
                          Id   : in out Chip_Id_Type)
   is
   begin
      case Io is
        when Input =>
         Id := Chip_Id_Type (Character'Pos (Char) - Character'Pos ('@'));
        when Output =>
         Char := Chip_Name (Id);
      end case;

   end Get_Chip_Id;


   procedure Get_Direction (Io   : in Input_Output;
                            Char : in out Character;
                            Dir  : in out Direction_Type)
   is
   begin
      case Io is
         when Input =>
            case Char is
               when '^' =>
                  Dir := Up;
               when '<' =>
                  Dir := Left;
               when 'v' =>
                  Dir := Down;
               when '>' =>
                  Dir := Right;
               when others =>
                  raise Data_Error with "invalid direction char [" & Char & "]";
            end case;
         when Output =>
            Char := Direction_Name (Dir);
      end case;
   end Get_Direction;


   procedure Read_Board (Board : in out Board_Type)
   is
      Buffer : Character;
   begin
      for Row in Row_Type loop
         for Column in Column_Type loop
            Get (Buffer);
            Get_Chip_Id (Input, Buffer, Board (Row, Column).Instance);
            Get (Buffer);
            Get_Direction (Input, Buffer, Board (Row, Column).Rotation);
            Get (Buffer);
         end loop;
      end loop;
      Get (Buffer);
   end Read_Board;


   procedure Write_Board (Board : in out Board_Type)
   is
      Buffer : Character := Space;
   begin
      for Row in Row_Type loop
         for Column in Column_Type loop
            Get_Chip_Id (Output, Buffer, Board (Row, Column).Instance);
            Put (Buffer);
            Get_Direction (Output, Buffer, Board (Row, Column).Rotation);
            Put (Buffer);
            Put (Space);
         end loop;
         New_Line;
      end loop;
      Put (Space);
      New_Line;
   end Write_Board;


   use Board_Vectors;


   Board : Board_Type;
   Count : Natural := 0;
   Rot : Rotation_Type;
   Tab : Vector;
begin

   while not End_Of_File loop
      Read_Board (Board);
      Count := Natural'Succ (Count);
      Get_Rotation (Board, Rot);
      Rotate_Board (Board, Rot);
      if not Contains (Tab, Board) then
         Write_Board (Board);
         Append (Tab, Board);
      end if;
   end loop;

end Check;
