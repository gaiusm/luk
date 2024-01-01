IMPLEMENTATION MODULE Consoles ;


FROM osinit IMPORT ScreenAddress ;
FROM ASCII IMPORT cr, lf, bs, esc, bel ;
FROM Kernel IMPORT PtrToProcDes, Suspend, Resume, CurrentProcess ;

IMPORT Scn ;
IMPORT TTIO ;
IMPORT StdIO ;


CONST
   MaxConsoles   =     4 ;
   Width         =    80 ;
   Height        =    25 ;
   Underline     = 0160C ;
   Normal        = 0007C ;

TYPE
   DisplayUnit = RECORD
                    Char  : CHAR ;           (* Character       *)
                    Attrib: CHAR ;           (* Attribute       *)
                 END ;

   StateMachine= RECORD
                    EscFound,                (* esc input found *)
                    MoveFound,               (* esc w found     *)
                    YFound   : BOOLEAN ;     (* esc w Y found   *)
                    NewY     : CARDINAL ;    (* Y value         *)
                 END ;

   Line        = ARRAY [0..Width-1] OF DisplayUnit ;

   ScreenInfo  = ARRAY [0..Height-1] OF Line ;

   ConsoleInfo = RECORD
                    ScreenBuf: ScreenInfo ;
                    Finite   : StateMachine ;
                    Xpos     ,
                    Ypos     : CARDINAL ;    (* Cursor address  *)
                    Waiting  : PtrToProcDes ;
                 END ;

VAR
   Console       : ARRAY [1..MaxConsoles] OF ConsoleInfo ;
   CurrentConsole: CARDINAL ;                (* Active console. *)
   Screen        : POINTER TO ScreenInfo ;   (* Colour CGA.     *)


(*
   UpCursor - removes the cursor from the display.
*)

PROCEDURE UpCursor (c: CARDINAL) ;
BEGIN
   IF c=CurrentConsole
   THEN
      WITH Console[CurrentConsole] DO
         Screen^[Ypos][Xpos].Attrib := Normal
      END
   END
END UpCursor ;

      
(*
   DownCursor - places the cursor down on the display.
*)

PROCEDURE DownCursor (c: CARDINAL) ;
BEGIN
   IF c=CurrentConsole
   THEN
      WITH Console[CurrentConsole] DO
         Screen^[Ypos][Xpos].Attrib := Underline
      END
   END
END DownCursor ;


(*
   Read - reads a character, ch, from a specified console, c.
*)

PROCEDURE Read (c: CARDINAL; VAR ch: CHAR) ;
BEGIN
   IF CurrentConsole=c
   THEN
      TTIO.Read(ch)
   ELSE
      Console[c].Waiting := CurrentProcess ;
      Suspend(CurrentProcess) ;  (* Wait until console is active *)
      Console[c].Waiting := NIL ;
      StdIO.Read(ch)
   END
END Read ;


(*
   Write - writes a character, ch, to a specified console, c.
*)

PROCEDURE Write (c: CARDINAL; ch: CHAR) ;
BEGIN
   UpCursor(c) ;
   IF NOT IsEscapeSequence(c, ch)
   THEN
      IF NOT IsControl(c, ch) AND (ch>=' ')
      THEN
         PutChar(c, ch)
      END
   END ;
   DownCursor(c)
END Write ;


(*
   IsEscapeSequence - returns true if the character, ch, can be accepted
                      into the escape sequence.
*)

PROCEDURE IsEscapeSequence (c: CARDINAL; ch: CHAR) : BOOLEAN ;
BEGIN
   WITH Console[c].Finite DO
      IF (NOT EscFound) AND (ch=esc)
      THEN
         EscFound := TRUE ;
         RETURN( TRUE )
      ELSIF (NOT MoveFound) AND EscFound AND (ch='v')
      THEN
         ClearScreen(c) ;
         MoveCursor(c, 0, 0) ;
         EscFound := FALSE ;
         RETURN( TRUE )
      ELSIF EscFound AND (NOT MoveFound) AND (ch='Y')
      THEN
         MoveFound := TRUE ;
         RETURN( TRUE )
      ELSIF MoveFound AND (NOT YFound)
      THEN
         IF ch>=' '
         THEN
            NewY := (ORD(ch)-ORD(' ')) MOD Height ;
            YFound := TRUE ;
            RETURN( TRUE )
         ELSE
            MoveFound := FALSE ;
            EscFound := FALSE ;
            RETURN( FALSE )
         END
      ELSIF YFound AND (ch>=' ')
      THEN
         MoveCursor(c, (ORD(ch)-ORD(' ')) MOD Width, NewY) ;
         YFound := FALSE ;
         EscFound := FALSE ;
         MoveFound := FALSE ;
         RETURN( TRUE )
      ELSE
         RETURN( FALSE )
      END
   END
END IsEscapeSequence ;


(*
   PutChar - places a character, ch, onto console, c.
*)

PROCEDURE PutChar (c: CARDINAL; ch: CHAR) ;
BEGIN
   WITH Console[c] DO
      IF CurrentConsole=c
      THEN
         Screen^[Ypos][Xpos].Char := ch
      ELSE
         ScreenBuf[Ypos][Xpos].Char := ch
      END ;
      IncX(c)
   END
END PutChar ;


(*
   IsControl - returns true if, ch, is a control character and if
               it has been consumed.
*)

PROCEDURE IsControl (c: CARDINAL; ch: CHAR) : BOOLEAN ;
BEGIN
   WITH Console[c] DO
      IF ch=cr
      THEN
         Xpos := 0 ;
         RETURN( TRUE )
      ELSIF ch=lf
      THEN
         IncY(c) ;
         RETURN( TRUE )
      ELSIF ch=bs
      THEN
         IF Xpos>0
         THEN
            DEC(Xpos)
         ELSIF Ypos>0
         THEN
            DEC(Ypos) ;
            Xpos := Width-1
         END ;
         RETURN( TRUE )
      ELSIF ch=bel
      THEN
         Scn.Bell ;
         RETURN( TRUE )
      ELSE
         RETURN( FALSE )
      END
   END
END IsControl ;


(*
   IncY - increment Ypos of the console, c, if it exceeds Height then
          the console is scrolled.
*)

PROCEDURE IncY (c: CARDINAL) ;
BEGIN
   WITH Console[c] DO
      IF Ypos=Height-1
      THEN
         ScrollUp(c)
      ELSE
         INC( Ypos )
      END
   END
END IncY ;


(*
   IncX - increment Xpos of the console, c, if it exceeds Width then
          Xpos is set to zero and Ypos is incremented.
*)

PROCEDURE IncX (c: CARDINAL) ;
BEGIN
   WITH Console[c] DO
      IF Xpos=Width-1
      THEN
         IncY(c) ;
         Xpos := 0
      ELSE
         INC( Xpos )
      END
   END
END IncX ;


(*
   ScrollUp - scrolls up the window, c.
*)

PROCEDURE ScrollUp (c: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   IF CurrentConsole=c
   THEN
      FOR j := 0 TO Height-2 DO
         Screen^[j] := Screen^[j+1]
      END ;
      FOR i := 0 TO Width-1 DO
         Screen^[Height-1][i].Char := ' '
      END
   ELSE
      WITH Console[c] DO
         FOR j := 0 TO Height-2 DO
            ScreenBuf[j] := ScreenBuf[j+1]
         END ;
         FOR i := 0 TO Width-1 DO
            ScreenBuf[Height-1][i].Char := ' '
         END
      END
   END
END ScrollUp ;


(*
   MoveCursor - moves the cursor on console, c, to x, y.
*)

PROCEDURE MoveCursor (c: CARDINAL; x, y: CARDINAL) ;
BEGIN
   WITH Console[c] DO
      Xpos := x ;
      Ypos := y
   END
END MoveCursor ;
   

(*
   ClearScreen - clears the screen on console, c.
*)

PROCEDURE ClearScreen (c: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   IF c=CurrentConsole
   THEN
      FOR j := 0 TO Height-1 DO
         FOR i := 0 TO Width-1 DO
            Screen^[j][i].Char := ' '
         END
      END
   ELSE
      WITH Console[c] DO
         FOR j := 0 TO Height-1 DO
            FOR i := 0 TO Width-1 DO
               ScreenBuf[j][i].Char := ' '
            END
         END
      END
   END
END ClearScreen ;


(*
   InitScreen - Init clears the screen on console, c, including the attributes.
*)

PROCEDURE InitScreen (c: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   WITH Console[c] DO
      FOR j := 0 TO Height-1 DO
         FOR i := 0 TO Width-1 DO
            ScreenBuf[j][i].Char := ' ' ;
            ScreenBuf[j][i].Attrib := Normal
         END
      END
   END ;
   FOR j := 0 TO Height-1 DO
      FOR i := 0 TO Width-1 DO
         Screen^[j][i].Char := ' ' ;
         Screen^[j][i].Attrib := Normal
      END
   END
END InitScreen ;


(*
   SetCurrentConsole - sets the current console to c.
*)

PROCEDURE SetCurrentConsole (c: CARDINAL) ;
BEGIN
   IF (c>=1) AND (c<=MaxConsoles)
   THEN
      UpCursor(CurrentConsole) ;
      Console[CurrentConsole].ScreenBuf := Screen^ ;
      CurrentConsole := c ;
      WITH Console[CurrentConsole] DO
         Screen^ := ScreenBuf ;
         DownCursor(CurrentConsole) ;
         IF Waiting#NIL
         THEN
            Resume(Waiting)
         END
      END
   END
END SetCurrentConsole ;


(*
   Init - initialises the console data structures.
*)

PROCEDURE Init ;
VAR
   i: CARDINAL ;
BEGIN
   Screen := ScreenAddress() ;
   FOR i := 1 TO MaxConsoles DO
      WITH Console[i] DO
         Xpos := 0 ;
         Ypos := 0 ;
         Waiting := NIL ;
         InitScreen(i) ;
         MoveCursor(i, 0, 0) ;
         Finite.EscFound := FALSE ;
         Finite.MoveFound := FALSE ;
         Finite.YFound := FALSE
      END
   END ;
   CurrentConsole := 1
END Init ;


BEGIN
   Init
END Consoles.
