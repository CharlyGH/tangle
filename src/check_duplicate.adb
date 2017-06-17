-- $Id: check_duplicate.adb 7 2010-06-09 15:09:17Z gmd $


package body Check_Duplicate is

   function Minimum (Le : in Chip_Id_Type;
                     Ri : in Chip_Id_Type)
                     return Chip_Id_Type
   is
   begin
      if Ri < Le then
         return Ri;
      else
         return Le;
      end if;
   end Minimum;


   function Rotate (Dir : in Direction_Type;
                    Rot : in Rotation_Type)
                    return Direction_Type
   is
   begin
      case Rot is
         when None =>
            return Dir;
         when Left =>
            case Dir is
               when Up | Left | Down =>
                  return Direction_Type'Succ (Dir);
               when Right =>
                  return Up;
            end case;
         when Full =>
            case Dir is
               when Up | Left  =>
                  return Direction_Type'Succ (Direction_Type'Succ (Dir));
               when Down | Right =>
                  return Direction_Type'Pred (Direction_Type'Pred (Dir));
            end case;
         when Right =>
            case Dir is
               when Left | Down | Right =>
                  return Direction_Type'Pred (Dir);
               when Up =>
                  return Right;
            end case;
      end case;
   end Rotate;



   function Id (Inp : in Row_Type)
                return Row_Type
   is
   begin
      return Inp;
   end Id;


   function Id (Inp : in Row_Type)
                return Column_Type
   is
   begin
      return Column_Type (Inp);
   end Id;


   function Id (Inp : in Column_Type)
                return Row_Type
   is
   begin
      return Row_Type (Inp);
   end Id;


   function Id (Inp : in Column_Type)
                return Column_Type
   is
   begin
      return Inp;
   end Id;


   function Inv (Inp : in Row_Type)
                 return Row_Type
   is
      Idx : Positive := Positive (Inp);
      Min : Positive := Positive (Row_Type'First);
      Max : Positive := Positive (Row_Type'Last);
   begin
      return Row_Type (Max + Min - Idx);
   end Inv;


   function Inv (Inp : in Row_Type)
                 return Column_Type
   is
      Idx : Positive := Positive (Inp);
      Min : Positive := Positive (Row_Type'First);
      Max : Positive := Positive (Row_Type'Last);
   begin
      return Column_Type (Max + Min - Idx);
   end Inv;


   function Inv (Inp : in Column_Type)
                 return Row_Type
   is
      Idx : Positive := Positive (Inp);
      Min : Positive := Positive (Row_Type'First);
      Max : Positive := Positive (Row_Type'Last);
   begin
      return Row_Type (Max + Min - Idx);
   end Inv;


   function Inv (Inp : in Column_Type)
                 return Column_Type
   is
      Idx : Positive := Positive (Inp);
      Min : Positive := Positive (Row_Type'First);
      Max : Positive := Positive (Row_Type'Last);
   begin
      return Column_Type (Max + Min - Idx);
   end Inv;


   procedure Get_Rotation (Board : in Board_Type;
                           Rot   : out Rotation_Type)
   is
      Nw : Chip_Id_Type := Board (Row_Type'First, Column_Type'First).Instance;
      Ne : Chip_Id_Type := Board (Row_Type'First, Column_Type'Last).Instance;
      Sw : Chip_Id_Type := Board (Row_Type'Last, Column_Type'First).Instance;
      Se : Chip_Id_Type := Board (Row_Type'Last, Column_Type'Last).Instance;
      Min : Chip_Id_Type := Minimum(Minimum(Nw,Ne),Minimum(Sw,Se));
   begin
      if Min = Nw then
         Rot := None;
      elsif Min = Ne then
         Rot := Left;
      elsif Min = Sw then
         Rot := Right;
      else
         Rot := Full;
      end if;
   end Get_Rotation;



   procedure Rotate_Board (Board : in Board_Type;
                           New_Board : out Board_Type;
                           Rot   : in Rotation_Type)
   is
   begin
      case Rot is
         when None =>
            for Row in Row_Type loop
               for Col in Column_Type loop
                  New_Board (Row, Col).Instance := Board (Id (Row), Id (Col)).Instance;
                  New_Board (Row, Col).Rotation := Rotate (Board (Id (Row), Id (Col)).Rotation, None);
               end loop;
            end loop;
            return;
         when Right =>
            for Row in Row_Type loop
               for Col in Column_Type loop
                  New_Board (Row, Col).Instance := Board (Inv (Col), Id (Row)).Instance;
                  New_Board (Row, Col).Rotation := Rotate (Board (Inv (Col), Id (Row)).Rotation, Right);
               end loop;
            end loop;
         when Left =>
            for Row in Row_Type loop
               for Col in Column_Type loop
                  New_Board (Row, Col).Instance := Board (Id (Col), Inv (Row)).Instance;
                  New_Board (Row, Col).Rotation := Rotate (Board (Id (Col), Inv (Row)).Rotation, Left);
               end loop;
            end loop;
         when Full =>
            for Row in Row_Type loop
               for Col in Column_Type loop
                  New_Board (Row, Col).Instance := Board (Inv (Row), Inv (Col)).Instance;
                  New_Board (Row, Col).Rotation := Rotate (Board (Inv (Row), Inv (Col)).Rotation, Full);
               end loop;
            end loop;
      end case;
   end Rotate_Board;


   procedure Normalize_Board (Board : in Board_Type;
                              New_Board : out Board_Type)
   is
      Rot : Rotation_Type;
   begin
      Get_Rotation (Board, Rot);
      Rotate_Board (Board, New_Board, Rot);
   end Normalize_Board;



end Check_Duplicate;
