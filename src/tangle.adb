-- $Id: tangle.adb 7 2010-06-09 15:09:17Z gmd $
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with System.Multiprocessors;
with GNAT.Command_Line;
with GNAT.Traceback.Symbolic;
with Ta_Types;
with Parallel;
with Serial;
with Get_Number_Value;
with Counter;
with Ta_Types;

procedure Tangle is

   package Atio renames Ada.Text_IO;
   package Gcl  renames GNAT.Command_Line;
   package Sm   renames System.Multiprocessors;
   package Art  renames Ada.Real_Time;

   Max_Row : constant Positive := 5;

   Max_Column : constant Positive := 5;

   Different  : constant Positive := 24;

   package Ta_Types_Pkg is new Ta_Types (Max_Row, Max_Column, Different);

   package Ta_Parallel is new Parallel (Ta_Types_Pkg);

   package Ta_Serial is new Serial (Ta_Types_Pkg);


   Task_Count : Natural := Integer (Sm.Number_Of_CPUs);
   Verbose_Level : Natural := 0;

--   Internal_Error : exception;
   Res : Ta_Types_Pkg.Proc_Id_Type;

   function Get_Natural_Value is new Get_Number_Value(Natural);

   Start : Art.Time;
   Stop  : Art.Time;
   Diff  : Long_Integer;
   use type Art.Time;
   use type Art.Time_Span;

   pragma Warnings (Off);

   function Convert is
     new Ada.Unchecked_Conversion (Art.Time, Long_Integer);


   pragma Warnings (On);

begin

     loop
        case Gcl.Getopt ("t: v:") is  -- Accepts '-t argument' '-v argument'
         when ASCII.NUL =>
            exit;

           when 't' =>
              Task_Count := Get_Natural_Value(Gcl.Parameter, Integer (Sm.Number_Of_CPUs)); --Get_Nprocs);

           when 'v' =>
              Verbose_Level := Get_Natural_Value(Gcl.Parameter,0);

           when others =>
              raise Program_Error;         -- cannot occur!
        end case;
     end loop;

   Atio.Put (Atio.Standard_Error, "using: " & Natural'Image (Task_Count) & " tasks");
   Atio.New_Line (Atio.Standard_Error);

   Start := Art.Clock;

   if Task_Count = 0 then
      declare
         Ind : ta_Serial.Indices;
      begin
         Ind.Initialize (Verbose_Level);
         Ind.Work;
      end;
   else

      declare
         Worker : Ta_Parallel.Processes (1 .. Ta_Types_Pkg.Proc_Id_Type(Task_Count));
      begin

         Ta_Parallel.Initialize(Verbose_Level);

         for Idx in Worker'Range loop
            Worker (Idx).Start (Idx);
         end loop;


         for Idx in Worker'Range loop
            Worker (Idx).Wait (Res);
         end loop;
      end;
   end if;


   Stop := Art.Clock;
   Diff := (Convert (Stop) - Convert (Start))/1_000_000;


   Atio.Put_Line (Atio.Standard_Error, "duration" & Diff'Img & " ms");
--   Atio.Put_Line (Atio.Standard_Error, "found " & Natural'Image (Counter.Get_Count) & " solutions");


exception
   when Ex : others =>
      Ada.Text_IO.Put_Line ("################################");
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (Ex));
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (Ex));
      Ada.Text_IO.Put_Line (Gnat.Traceback.Symbolic.Symbolic_Traceback(Ex));
      Ada.Text_IO.Put_Line ("################################");

end Tangle;
