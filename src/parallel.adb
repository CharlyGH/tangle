-- $Id: parallel.adb 7 2010-06-09 15:09:17Z gmd $

with Ada.Exceptions;
with Ada.Text_IO;
with Counter;

package body Parallel
is

   use Pa_Solver;
   use Counter;

   procedure Initialize(Level : in Natural)
   is
   begin
      The_Level := Level;
      The_Index.Initialize;
      Initialize;
   end Initialize;


   protected body Indices
   is
      procedure Initialize
      is
      begin
         Act_Chip  := Extended_Chip_Id_Type'First;
         Last_Chip := Chip_Id_Type(Different);
         Status    := True;
      end Initialize;

      procedure Get_Next_Index
        (Chip : out Chip_Id_Type;
         Stat : out Boolean)
      is
      begin
         if not Status then
            Chip := Chip_Id_Type'First;
            Stat := False;
            return;
         end if;
         Act_Chip := Chip_Id_Type'Succ (Act_Chip);
         if Act_Chip > Last_Chip then
            Status := False;
            Chip := Chip_Id_Type'First;
            Stat := False;
            return;
         end if;
         Chip := Act_Chip;
         Stat := Status;
      end Get_Next_Index;

   end Indices;




   task body Process
   is
      Proc_Id : Extended_Proc_Id_Type;
      Chip_Id : Chip_Id_Type := Chip_Id_Type'First;
      Status  : Boolean := True;
   begin
      accept Start (Id : in Extended_Proc_Id_Type) do
         --Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         --                      "starting thread #" & Proc_Id_Type'Image (Id));
         Proc_Id := Id;
      end Start;

      while Status loop
         The_Index.Get_Next_Index (Chip_Id, Status);
         exit when not Status;
         Work (Proc_Id, Chip_Id);
      end loop;

      accept Wait (Code : out Extended_Proc_Id_Type) do
         Code := Proc_Id;
      end Wait;
   exception
      when Ex : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Ex));
   end Process;

   procedure Work
     (Pid  : in Extended_Proc_Id_Type;
      Chip : in Chip_Id_Type)
   is
   begin
      Solve_Tangle(Pid,Chip,The_Level);
   end Work;


end Parallel;

