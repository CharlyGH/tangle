-- $Id$

with Ta_Types;
with Solver;

generic

   with package Pa_Types is new Ta_Types (<>);


package Serial
is

   use Pa_Types;

   package Pa_Solver is new Solver(Pa_Types);


   type Indices is tagged private;

   procedure Initialize
     (Ind : in out Indices;
      Lvl : in     Natural);


   procedure Get_Next_Index
     (Ind  : in out Indices;
      Chip : out    Chip_Id_Type;
      Stat : out    Boolean);


   procedure Work
     (Ind  : in out Indices);


private

   type Indices is tagged record
      Act_Chip  : Extended_Chip_Id_Type;
      Last_Chip : Chip_Id_Type;
      Level     : Natural;
      Status    : Boolean;
   end record;


end Serial;
