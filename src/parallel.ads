-- $Id: parallel.ads 7 2010-06-09 15:09:17Z gmd $

with Ta_Types;
with Solver;

generic

   with package Pa_Types is new Ta_Types (<>);


package Parallel
is

   use Pa_Types;

   package Pa_Solver is new Solver(Pa_Types);

   procedure Initialize
     (Level : in Natural);

   protected type Indices
   is
      procedure Initialize;

      procedure Get_Next_Index
        (Chip : out Chip_Id_Type;
         Stat : out Boolean);
   private
      Act_Chip  : Extended_Chip_Id_Type;
      Last_Chip : Chip_Id_Type;
      Status    : Boolean;
   end Indices;



   task type Process
   is
      entry Start
        (Id : in Extended_Proc_Id_Type);
      entry Wait
        (Code : out Extended_Proc_Id_Type);
   end Process;

   type Processes is array (Proc_Id_Type range <>) of Process;

   procedure Work
     (Pid : in Extended_Proc_Id_Type;
      Chip : in Chip_Id_Type);


private

   The_Index : Indices;
   The_Level : Natural;

end Parallel;

