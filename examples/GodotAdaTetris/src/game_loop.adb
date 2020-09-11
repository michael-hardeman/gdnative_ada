------------------------------------------------------------------------------
--                                                                          --
--                             GNAT EXAMPLE                                 --
--                                                                          --
--                      Copyright (C) 2016, AdaCore                         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Real_Time; use Ada.Real_Time;
with Interfaces; use Interfaces;


package body Game_Loop is
   --  The current time.  We don't really use a ravenscar architecture: no
   --  periodic task.

   Slow_Fall_Period  : constant Time_Span := Milliseconds (1000 / 2 / 4);
   --  Quarter of time to wait before a piece falls.  We allow 2 rotations
   --  or moves per fall, and 4 falls per second.

   --  Time to wait when a piece is dropped

   --  Blinking period for leds at the end of the game

   Fall_Period : Time_Span;
   pragma Unreferenced (Fall_Period);

   Nbr_Pieces : Natural;
   --  Number of pieces fallen.  This is the score.

   Rnd : Unsigned_32 := 137;
   --  Simple random generator.

   Success : Boolean;

   State : Cycle_State := Init;

   function Get_State return Cycle_State is
   begin
      return State;
   exception
      when others =>
         return Init;
   end Get_State;

   procedure Cycle is
   begin
      Line_Destroyed := (others => False);

      if State = Lost then
         return;
      end if;

      if State = Init then
         Cur_Board := (others => (others => Empty));
         Nbr_Pieces := 0;

         State := New_Piece;
         return;
      end if;

      if State = New_Piece then
         --  Add a new piece
         Rnd := Rnd * 1103515245 + 12345;

         Cur_Piece := (S => Cell'Val (1 + ((Rnd / 65536) mod 7)),
                       D => North,
                       X => X_Size / 2,
                       Y => Y_Coord'First);

         Cur_State := Piece_Falling;

         if not Valid_Configuration then
            State := Lost;
            return;
         end if;

         Fall_Period := Slow_Fall_Period;

         State := Falling;
         return;
      end if;

      if State = Falling then
         --  Fall and continue unless the piece hits the ground
         Do_Action (Move_Down, Success);

         if Success then
            -- continue to fall
            return;
         end if;

         --  Done with that piece
         Cur_State := Piece_Blocked;
         Nbr_Pieces := Nbr_Pieces + 1;

         Include_Piece_In_Board;
         Delete_Complete_Lines;
         State := New_Piece;
      end if;
   exception
      when others =>
         null;
   end Cycle;


   function Get_Kind (X, Y : Integer) return Cell is
   begin
      return Cur_Board (Y)(X);
   exception
      when others =>
         return Empty;
   end Get_Kind;


   function Get_Cur_Piece return Piece is
   begin
      return Cur_Piece;
   exception
      when others =>
         return (others => <>);
   end Get_Cur_Piece;

   function Check_Possible_I_Shapes
     (D : Direction; X, Y : Integer) return CSharp_Bool is
   begin
      return CSharp_Bool (Possible_I_Shapes (D) (Y, X));
   exception
      when others =>
         return False;
   end Check_Possible_I_Shapes;

   function Check_Possible_Three_Shapes
     (S : Shape; D : Direction; X, Y : Integer) return CSharp_Bool is
   begin
      return CSharp_Bool(Possible_Three_Shapes (S, D) (Y, X));
   exception
      when others =>
         return False;
   end Check_Possible_Three_Shapes;

   function Do_Action (A : Action) return CSharp_Bool is
      R : Boolean;
   begin
      Do_Action(A, R);
      return CSharp_Bool(R);
   exception
      when others =>
         return False;
   end Do_Action;

   function Is_Line_Destroyed (Y : Integer) return CSharp_Bool is
   begin
      return CSharp_Bool (Line_Destroyed (Y));
   exception
      when others =>
         return False;
   end Is_Line_Destroyed;



end Game_Loop;