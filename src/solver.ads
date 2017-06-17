-- $Id: solver.ads 7 2010-06-09 15:09:17Z gmd $
-- Solve Rublic's tangle
private with Ada.Characters.Latin_1;

with Ta_Types;

generic

   with package So_Types is new Ta_Types (<>);


package Solver is


   use So_Types;


   procedure Solve_Tangle(Pid        : in Extended_Proc_Id_Type;
                          Start_Chip : in Chip_Id_Type;
                          Verbose    : in Natural);


private

   use Ada.Characters.Latin_1;


   Internal_Error : exception;


   type Side_Type is new Positive range 12 .. 43;

   type Name_Type is array (Side_Type) of Character;

   type Line_Type is (Low, High, Both);

   type Border_Type is array (Direction_Type) of Side_Type;

   type Chip_Side_Type is (Top, Left, Bottom, Right);

   type Chip_Type is
      record
         Top    : Border_Type;
         Left   : Border_Type;
         Bottom : Border_Type;
         Right  : Border_Type;
      end record;

   type Deck_Type is array (Chip_Id_Type) of Chip_Type;

   type Table_Type is array (Chip_Id_Type) of Chip_Id_Type;


   procedure Init_Index_Table
     (Table     : in out Table_Type;
      Offset    : in Chip_Id_Type;
      Different : in Chip_Id_Type);

   procedure Try_Chip
     (Pid       : in Extended_Proc_Id_Type;
      Table     : in out Table_Type;
      Board     : in out Board_Type;
      Level     : in Chip_Id_Type;
      Top_Id    : in Chip_Id_Type;
      Row       : in Row_Type;
      Column    : in Column_Type;
      Chip_Id   : in Chip_Id_Type;
      Direction : in Direction_Type;
      Verbose   : in Natural);


   procedure Try_Chip
     (Pid     : in Extended_Proc_Id_Type;
      Table   : in out Table_Type;
      Board   : in out Board_Type;
      Level   : in Chip_Id_Type;
      C_Idx   : in Chip_Id_Type;
      Top_Id  : in Chip_Id_Type;
      Verbose : in Natural);


   procedure Print_Board
     (Board   : in out Board_Type;
      Verbose : in     Natural);


   procedure Print_Chip
     (Chip_Id : in Chip_Id_Type;
                         Dir     : in Direction_Type);

   procedure Print_Chip
     (Chip    : in out Chip_Type;
      Dir     : in     Direction_Type;
      Side    : in     Chip_Side_Type;
      Line    : in     Line_Type);

   function Color_Letter
     (Chr : in Character) return String;

   procedure Get_Chip
     (Table   : in out Table_Type;
      Top_Id  : in     Chip_Id_Type;
      C_Idx   : in     Chip_Id_Type;
      Chip_Id :    out Chip_Id_Type);

   procedure Put_Chip
     (Table  : in out Table_Type;
      Top_Id : in     Chip_Id_Type;
      C_Idx  : in     Chip_Id_Type);

   function Check
     (Board     : in Board_Type;
      Chip_Id   : in Chip_Id_Type;
      Row       : in Row_Type;
      Column    : in Column_Type;
      Direction : in Direction_Type) return Boolean;


   The_Deck : constant Deck_Type :=
     (((21,31,42,34),(43,12,13,24),(24,43,12,13),(31,42,34,21)),
      ((41,31,24,32),(23,14,13,42),(42,23,14,13),(31,24,32,41)),
      ((31,41,23,42),(24,13,14,32),(32,24,13,14),(41,23,42,31)),
      ((41,21,34,23),(32,14,12,43),(43,32,14,12),(21,34,23,41)),
      ((21,41,32,43),(34,12,14,23),(23,34,12,14),(41,32,43,21)),
      ((31,21,43,24),(42,13,12,34),(34,42,13,12),(21,43,24,31)),
      ((42,32,14,31),(13,24,23,41),(41,13,24,23),(32,14,31,42)),
      ((32,42,13,41),(14,23,24,31),(31,14,23,24),(42,13,41,32)),
      ((42,12,34,13),(31,24,21,43),(43,31,24,21),(12,34,13,42)),
      ((12,42,31,43),(34,21,24,13),(13,34,21,24),(42,31,43,12)),
      ((12,32,41,34),(43,21,23,14),(14,43,21,23),(32,41,34,12)),
      ((32,12,43,14),(41,23,21,34),(34,41,23,21),(12,43,14,32)),
      ((43,23,14,21),(12,34,32,41),(41,12,34,32),(23,14,21,43)),
      ((23,43,12,41),(14,32,34,21),(21,14,32,34),(43,12,41,23)),
      ((43,13,24,12),(21,34,31,42),(42,21,34,31),(13,24,12,43)),
      ((13,43,21,42),(24,31,34,12),(12,24,31,34),(43,21,42,13)),
      ((13,23,41,24),(42,31,32,14),(14,42,31,32),(23,41,24,13)),
      ((23,13,42,14),(41,32,31,24),(24,41,32,31),(13,42,14,23)),
      ((34,24,13,21),(12,43,42,31),(31,12,43,42),(24,13,21,34)),
      ((24,34,12,31),(13,42,43,21),(21,13,42,43),(34,12,31,24)),
      ((34,14,23,12),(21,43,41,32),(32,21,43,41),(14,23,12,34)),
      ((14,34,21,32),(23,41,43,12),(12,23,41,43),(34,21,32,14)),
      ((14,24,31,23),(32,41,42,13),(13,32,41,42),(24,31,23,14)),
      ((24,14,32,13),(31,42,41,23),(23,31,42,41),(14,32,13,24)),
      ((24,14,32,13),(31,42,41,23),(23,31,42,41),(14,32,13,24)));



   Row_Index : array(Chip_Id_Type) of Row_Type := (1, 1, 1, 1, 1,
                                                   2, 2, 2, 2, 2,
                                                   3, 3, 3, 3, 3,
                                                   4, 4, 4, 4, 4,
                                                   5, 5, 5, 5, 5);

   Column_Index : array(Chip_Id_Type) of Column_Type := (1, 2, 3, 4, 5,
                                                         1, 2, 3, 4, 5,
                                                         1, 2, 3, 4, 5,
                                                         1, 2, 3, 4, 5,
                                                         1, 2, 3, 4, 5);

   type Full_Color_Type is (None, Red, Green, Blue, Yellow);

   subtype Color_Type is Full_Color_Type range Red .. Yellow;

   type Color_Table_Type is array (Full_Color_Type) of String(1 .. 7);

   High_Names : Name_Type := (12 => '1', 13 => '1', 14 => '1',
                              21 => '2', 23 => '2', 24 => '2',
                              31 => '3', 32 => '3', 34 => '3',
                              41 => '4', 42 => '4', 43 => '4', others => ' ');

   Low_Names : Name_Type := (12 => '2', 13 => '3', 14 => '4',
                             21 => '1', 23 => '3', 24 => '4',
                             31 => '1', 32 => '2', 34 => '4',
                             41 => '1', 42 => '2', 43 => '3', others => ' ');

   Color_Table : constant Color_Table_Type := (ESC & "[30;0m",
                                               ESC & "[31;1m",
                                               ESC & "[32;1m",
                                               ESC & "[34;1m",
                                               ESC & "[30;1m");

   type Color_Name_Type is array (Color_Type) of Character;

   Color_Name : constant Color_Name_Type := ('R', 'G', 'B', 'Y');

end Solver;
