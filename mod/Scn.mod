IMPLEMENTATION MODULE Scn ;

FROM ASCII IMPORT cr, lf, bs ;
FROM osinit IMPORT ScreenAddress ;
FROM PortIO IMPORT In8, Out8 ;

FROM SYSTEM IMPORT ADDRESS, BITSET, BYTE,
                   OnOrOff, TurnInterrupts ;



(* FROM Graphic IMPORT SetScreenMode, SetActiveDisplayPage, SetCursorType ; *)

CONST
   Width     = 80 ;
   Height    = 25 ;
   underline = 0160C ;
   normal    = 0007C ;

TYPE
   DisplayUnit = RECORD
                    char  : CHAR __ATTRIBUTE__ ((ALIGNED(1))) ;
                    attrib: CHAR __ATTRIBUTE__ ((ALIGNED(1))) ;
                 END ;

   Line        = ARRAY [0..Width-1] OF DisplayUnit __ATTRIBUTE__ ((ALIGNED(1))) ;
   Screen      = ARRAY [0..Height-1] OF Line __ATTRIBUTE__ ((ALIGNED(1))) ;

VAR
   xcur, ycur: CARDINAL ;
   screen    : POINTER TO Screen ;


PROCEDURE Write (ch: CHAR) ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   screen^[ycur][xcur].attrib := normal ;
   IF ch=cr
   THEN
      xcur := 0 ;
   ELSIF ch=lf
   THEN
      incy
   ELSIF ch=bs
   THEN
      IF xcur>0
      THEN
         DEC(xcur)
      ELSIF ycur>0
      THEN
         DEC(ycur) ;
         xcur := Width-1
      END
   ELSIF ch>=' '
   THEN
      screen^[ycur][xcur].char := ch ;
      incx ;
   END ;
   screen^[ycur][xcur].attrib := underline ;
   ToOldState := TurnInterrupts(ToOldState)
END Write ;


PROCEDURE incy ;
BEGIN
   IF ycur=Height-1
   THEN
      ScrollUp
   ELSE
      INC(ycur)
   END
END incy ;


PROCEDURE incx ;
BEGIN
   IF xcur=Width-1
   THEN
      incy ;
      xcur := 0
   ELSE
      INC(xcur)
   END
END incx ;


PROCEDURE ScrollUp ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR j := 0 TO Height-2 DO
      (* there appears to be a gm2 bug as this line should achieve the same
         as the inner for loop:

         screen^[j] := screen^[j+1]

         so for now we work around this bug and do it square at a line instead.
      *)
      FOR i := 0 TO Width-1 DO
         screen^[j][i] := screen^[j+1][i]
      END
   END ;
   BlankBottomLine
END ScrollUp ;


PROCEDURE BlankBottomLine ;
BEGIN
   ClearLine(Height-1)
END BlankBottomLine ;


(*
   ClearLine - clears line, y, filling it with spaces.
*)

PROCEDURE ClearLine (y: CARDINAL) ;
VAR
   ToOldState: OnOrOff ;
   i         : CARDINAL ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   FOR i := 0 TO Width-1 DO
      screen^[y][i].char := ' ' ;
      screen^[y][i].attrib := normal
   END ;
   ToOldState := TurnInterrupts(ToOldState)
END ClearLine ;


(*
   AppendLine - adds another line after, y, to the display, all the
                other lines move down one line.
*)

PROCEDURE AppendLine (y: CARDINAL) ;
VAR
   ToOldState: OnOrOff ;
   Bottom    : CARDINAL ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   Bottom := Height-1 ;
   WHILE Bottom-1>y DO
      screen^[Bottom] := screen^[Bottom-1] ;
      DEC(Bottom)
   END ;
   ToOldState := TurnInterrupts(ToOldState)
END AppendLine ;


(*
   DeleteLine - deletes line, y, from the screen, the lower lines
                move up to fill in the gap.
*)

PROCEDURE DeleteLine (y: CARDINAL) ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   WHILE y<Height-1 DO
      screen^[y] := screen^[y+1] ;
      INC(y)
   END ;
   BlankBottomLine ;
   ToOldState := TurnInterrupts(ToOldState)
END DeleteLine ;


PROCEDURE MoveCursor (x, y: CARDINAL) ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   screen^[ycur][xcur].attrib := normal ;
   xcur := x ;
   ycur := y ;
   screen^[ycur][xcur].attrib := underline ;
   ToOldState := TurnInterrupts(ToOldState)
END MoveCursor ;
   

PROCEDURE ClearScreen ;
VAR
   i, j      : CARDINAL ;
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   FOR j := 0 TO Height-1 DO
      FOR i := 0 TO Width-1 DO
         screen^[j][i].char := ' ' ;
         screen^[j][i].attrib := normal
      END
   END ;
   ToOldState := TurnInterrupts(ToOldState)
END ClearScreen ;


(*
   OnSound - 
*)

PROCEDURE OnSound (tone: CARDINAL) ;
VAR
   b: BYTE ;
   s: BITSET ;
BEGIN
   Out8(043H, 0B6H) ;               (*  tell timer prepare for new sound *)
   Out8(042H, VAL(BYTE, tone MOD 0100H)) ;      (* new tone to timer LSB *)
   Out8(042H, VAL(BYTE, tone DIV 0100H)) ;      (* LSB -> timer          *)
   b := In8(061H) ;                           (* enable speaker via time *)
   s := VAL(BITSET, b) ;
   s := s + {0, 1} ;
   Out8(061H, VAL(BYTE, s))
END OnSound ;


(*
   OffSound - 
*)

PROCEDURE OffSound ;
VAR
   b: BYTE ;
   s: BITSET ;
BEGIN
   b := In8(061H) ;         (* disable speaker    *)
   s := VAL(BITSET, b) ;
   s := s - {0, 1} ;
   Out8(061H, VAL(BYTE, s)) (* output via a timer *)
END OffSound ;


(*
   Delay - 
*)

PROCEDURE Delay (MaxI, MaxJ: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR i := 0 TO MaxI DO
      FOR j := 0 TO MaxJ DO
      END
   END
END Delay ;



PROCEDURE Bell ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   OnSound(500) ;
   Delay(10, 400) ;
   OffSound ;
   ToOldState := TurnInterrupts(ToOldState)
END Bell ;


(*
   Slide - 
*)

PROCEDURE Slide ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 1 TO 500 DO
      OnSound(i) ;
      Delay(10, 400) ;
      OffSound
   END ;
END Slide ;


(*
   Init - initialize all module data structures.
*)

PROCEDURE Init ;
BEGIN
   screen := ScreenAddress() ;
   xcur := 0 ;
   ycur := 0 ;
   MoveCursor(0, 24) ;
   Bell
END Init ;


END Scn.
