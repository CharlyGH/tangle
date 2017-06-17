-- $Id: solver.adb 7 2010-06-09 15:09:17Z gmd $
-- Solve Rublic's tangle
with Ada.Text_IO;
with Counter;

with Solutions;
with Check_Duplicate;


package body Solver is

   package Atio  renames Ada.Text_IO;

   package Solutions_Pgk is new Solutions (So_Types);
   use Solutions_Pgk;

   package Check_Duplicate_Pkg is new Check_Duplicate (So_Types);
   use Check_Duplicate_Pkg;

   use Counter;

   procedure Solve_Tangle
     (Pid        : in Extended_Proc_Id_Type;
      Start_Chip : in Chip_Id_Type;
      Verbose    : in Natural)
   is
      Table : Table_Type;
      Board : Board_Type;
      Diff  : Chip_Id_Type := Chip_Id_Type(Different);
   begin
      Init_Index_Table(Table,Start_Chip,Diff);
      Try_Chip(Pid,Table,Board,Chip_Id_Type'First,Chip_Id_Type'First,Chip_Id_Type'Last,Verbose);
   end Solve_Tangle;


   procedure Init_Index_Table
     (Table : in out Table_Type;
      Offset : in Chip_Id_Type;
      Different : in Chip_Id_Type)
   is
      Value : Chip_Id_Type := Offset;
   begin
      for Idx in Chip_Id_Type'First .. Different loop
         Table(Idx) := Value;
         Value := Chip_Id_Type'Succ(Value);
         if Value > Different then
            Value := Table_Type'First;
         end if;
      end loop;
      for Idx in Chip_Id_Type'Succ(Different) .. Table_Type'Last loop
         Table(Idx) := Idx;
      end loop;
   end Init_Index_Table;


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
      Verbose   : in Natural)
is
      Is_New : Boolean;
      New_Board : Board_Type;
   begin
      Board(Row,Column).Instance := Chip_Id;
      Board(Row,Column).Rotation := Direction;

      if Top_Id = 1 then
         Normalize_Board (Board, New_Board);

         Check_Solution (New_Board, Is_New);
         if Is_New then
            Increment_Counter;
            if Verbose > 0 then
               Print_Board (New_Board,Verbose);
               Atio.New_Line;
            end if;
         end if;
      else
        for Idx in 1 .. Chip_Id_Type'Pred(Top_Id) loop
            Try_Chip(Pid,Table,Board,Chip_Id_Type'Succ(Level),Idx,Chip_Id_Type'Pred(Top_Id),Verbose);
         end loop;
      end if;

   end Try_Chip;


   procedure Try_Chip
     (Pid     : in Extended_Proc_Id_Type;
      Table   : in out Table_Type;
      Board   : in out Board_Type;
      Level   : in Chip_Id_Type;
      C_Idx   : in Chip_Id_Type;
      Top_Id  : in Chip_Id_Type;
      Verbose : in Natural)
is
      Chip_Id : Chip_Id_Type;
      Row : Row_Type := Row_Index(Level);
      Column : Column_Type := Column_Index(Level);
   begin

      Get_Chip(Table,Top_Id,C_Idx,Chip_Id);
      for Direction in Direction_Type loop
         if Check(Board,Chip_Id,Row,Column,Direction) then
            Try_Chip(Pid,Table,Board,Level,Top_Id,Row,Column,Chip_Id,Direction,Verbose);
         end if;
      end loop;
      Put_Chip(Table,Top_Id,C_Idx);

   end Try_Chip;


   procedure Print_Board
     (Board   : in out Board_Type;
      Verbose : in Natural)
   is
      Dir      : Direction_Type;
      Chip_Id  : Chip_Id_Type;
      Chip     : Chip_Type;
   begin
      for Row in Row_Type loop
         if Verbose = 1 then
            for Column in Column_Type loop
               Dir      := Board(Row,Column).Rotation;
               Chip_Id  := Board(Row,Column).Instance;
               Print_Chip (Chip_Id, Dir);
            end loop;
         else

            for Column in Column_Type loop
               Dir      := Board(Row,Column).Rotation;
               Chip_Id  := Board(Row,Column).Instance;
               Chip     := The_Deck(Chip_Id);
               Print_Chip(Chip,Dir,Top,Both);
            end loop;

            Atio.New_Line;

            for Column in Column_Type loop
               Dir      := Board(Row,Column).Rotation;
               Chip_Id  := Board(Row,Column).Instance;
               Chip     := The_Deck(Chip_Id);
               Print_Chip(Chip,Dir,Left,High);
               Print_Chip(Chip,Dir,Right,High);
            end loop;

            Atio.New_Line;

            for Column in Column_Type loop
               Dir      := Board(Row,Column).Rotation;
               Chip_Id  := Board(Row,Column).Instance;
               Chip     := The_Deck(Chip_Id);
               Print_Chip(Chip,Dir,Left,Low);
               Print_Chip(Chip,Dir,Right,Low);
            end loop;

            Atio.New_Line;

            for Column in Column_Type loop
               Dir      := Board(Row,Column).Rotation;
               Chip_Id  := Board(Row,Column).Instance;
               Chip     := The_Deck(Chip_Id);
               Print_Chip(Chip,Dir,Bottom,Both);
            end loop;
         end if;

         Atio.New_Line;

      end loop;

      Atio.Put_Line(" ");

   end Print_Board;


   procedure Print_Chip
     (Chip_Id : in Chip_Id_Type;
      Dir     : in Direction_Type)
   is
   begin
      Atio.Put(Chip_Name (Chip_Id) & Direction_Name(Dir) & " ");
   end Print_Chip;


   procedure Print_Chip
     (Chip    : in out Chip_Type;
      Dir     : in Direction_Type;
      Side    : in Chip_Side_Type;
      Line    : in Line_Type)
   is
   begin
      case Side is
         when Top =>
            Atio.Put("    " & Color_Letter(High_Names(Chip.Top(Dir)))
                              & Color_Letter(Low_Names(Chip.Top(Dir))) & "   ");
         when Left =>
            if Line = High then
               Atio.Put("   " & Color_Letter(High_Names(Chip.Left(Dir))) & " ");
            else
               Atio.Put("   " & Color_Letter(Low_Names(Chip.Left(Dir))) & " ");
            end if;
         when Bottom =>
            Atio.Put("    " & Color_Letter(High_Names(Chip.Bottom(Dir)))
                              & Color_Letter(Low_Names(Chip.Bottom(Dir))) & "   ");
         when Right =>
            if Line = High then
               Atio.Put(" " & Color_Letter(High_Names(Chip.Right(Dir))) & "  ");
            else
               Atio.Put(" " & Color_Letter(Low_Names(Chip.Right(Dir))) & "  ");
            end if;
      end  case;

   end Print_Chip;


   function Color_Letter
     (Chr : in Character) return String
   is
      N : Natural;
      Out_Chr : Character;
   begin
      N := Character'Pos (Chr) - Character'Pos ('0');
      Out_Chr := Color_Name(Color_Type'Val(N));
      return Color_Table (Full_Color_Type'Val(N)) & Out_Chr & Color_Table (Full_Color_Type'Val (0));
   end Color_Letter;


   procedure Get_Chip
     (Table   : in out Table_Type;
      Top_Id  : in Chip_Id_Type;
      C_Idx   : in Chip_Id_Type;
      Chip_Id : out Chip_Id_Type)
is
   begin
      Chip_Id       := Table(C_Idx);
      Table(C_Idx)  := Table(Top_Id);
      Table(Top_Id) := Chip_Id;
   end Get_Chip;


   procedure Put_Chip
     (Table  : in out Table_Type;
      Top_Id : in Chip_Id_Type;
      C_Idx  : in Chip_Id_Type)
   is
      Temp : Chip_Id_Type := Table(Top_Id);
   begin
      Table(Top_Id) := Table(C_Idx);
      Table(C_Idx)  := Temp;
   end Put_Chip;


   function Check
     (Board     : in Board_Type;
      Chip_Id   : in Chip_Id_Type;
      Row       : in Row_Type;
      Column    : in Column_Type;
      Direction : in Direction_Type) return Boolean
is
      Old_Id : Chip_Id_Type := Chip_Id_Type'First;
      Old_Dir : Direction_Type := Direction_Type'First;
   begin
      if Row > 1 then
         Old_Id := Board(Row-1,Column).Instance;
         Old_Dir := Board(Row-1,Column).Rotation;
         if The_Deck(Chip_Id).Top(Direction) /= The_Deck(Old_Id).Bottom(Old_Dir) then
            return False;
         end if;
      end if;

      if Column > 1 then
         Old_Id := Board(Row,Column-1).Instance;
         Old_Dir := Board (Row, Column - 1).Rotation;
         if The_Deck(Chip_Id).Left(Direction) /= The_Deck(Old_Id).Right(Old_Dir) then
            return False;
         end if;
      end if;

      return True;

   exception
      when Ex : others =>
         Atio.Put_Line ("Chip_Id=" & Chip_Id'Img & ",  Old_Id=" & Old_Id'Img & ",  Old_Dir=" & Old_Dir'Img);
         raise;

   end Check;

end Solver;
