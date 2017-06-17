-- $Id$

with Ada.Exceptions;
with Ada.Text_IO;
with Counter;

package body Serial
is

   use Pa_Solver;
   use Counter;

   procedure Initialize
     (Ind : in out Indices;
      Lvl : in Natural)
   is
   begin
      Ind.Level := Lvl;
      Ind.Act_Chip  := Extended_Chip_Id_Type'First;
      Ind.Last_Chip := Chip_Id_Type(Different);
      Ind.Status    := True;
   end Initialize;



   procedure Get_Next_Index
     (Ind  : in out Indices;
      Chip : out Chip_Id_Type;
      Stat : out Boolean)
   is
   begin
      if not Ind.Status then
         Chip := Chip_Id_Type'First;
         Stat := False;
         return;
      end if;
      Ind.Act_Chip := Chip_Id_Type'Succ (Ind.Act_Chip);
      if Ind.Act_Chip > Ind.Last_Chip then
         Ind.Status := False;
         Chip := Chip_Id_Type'First;
         Stat := False;
         return;
      end if;
      Chip := Ind.Act_Chip;
      Stat := Ind.Status;
   end Get_Next_Index;


   procedure Work
     (Ind  : in out Indices)
   is
      Proc_Id : Extended_Proc_Id_Type := 0;
      Chip_Id : Chip_Id_Type := Chip_Id_Type'First;
      Status  : Boolean := True;
   begin
      while Status loop
         Ind.Get_Next_Index (Chip_Id, Status);
         exit when not Status;
         Solve_Tangle(Proc_Id,Chip_Id,Ind.Level);
      end loop;
   end Work;


end Serial;
