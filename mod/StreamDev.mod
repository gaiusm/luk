IMPLEMENTATION MODULE StreamDev ;

(*
   Title      : StreamDev
   Author     : Gaius Mulley
   Date       : 11/8/89
   LastEdit   : 16/7/96
   System     : LOGITECH MODULA-2/86
   Description: Provides a textual command to set up a stream.
*)

FROM IO IMPORT STREAM,
               NewStream, SwapStream, DisposeStream, InitStream,
               DupStream ;
FROM StdIO IMPORT ProcWrite, ProcRead ;
FROM Kernel IMPORT CurrentProcess, Suspend, Resume ;
FROM StrLib IMPORT StrEqual, StrCopy ;
FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM StdIO IMPORT Read, Write ;
FROM CmdArgs IMPORT GetArg, Narg ;

IMPORT Scn ;
IMPORT TTIO ;
IMPORT Consoles ;
IMPORT SerIOA ;
IMPORT SerIOB ;

(* IMPORT SerIOB4 ; *)


CONST
   MaxLine = 30 ;


(*
   SetStream - sets the STREAM, s, to any io dependant upon string, a.
*)

PROCEDURE SetStream (s: STREAM; a: ARRAY OF CHAR) ;
VAR
   r   : ProcRead ;
   w   : ProcWrite ;
   i   : CARDINAL ;
   ArgN: ARRAY [0..MaxLine] OF CHAR ;
BEGIN
   i := Narg(a) ;
   IF i>0
   THEN
      DEC(i)
   END ;
   IF (GetArg(a, i, ArgN) AND StrEqual(ArgN, '&')) OR
      (CurrentProcess^.PStream=NIL)
   THEN
      r := StopRead ;
      w := StopWrite
   ELSE
      r := CurrentProcess^.PStream^.inchar ;
      w := CurrentProcess^.PStream^.outchar
   END ;
   i := 0 ;
   WHILE GetArg(a, i, ArgN) DO
      IF StrEqual(ArgN, '>')
      THEN
         INC(i) ;
         IF GetArg(a, i, ArgN) AND GetWriteProc(ArgN, w)
         THEN
         ELSE
            WriteString(' - output undefined') ; WriteLn
         END
      ELSIF StrEqual(ArgN, '<')
      THEN
         INC(i) ;
         IF GetArg(a, i, ArgN) AND GetReadProc(ArgN, r)
         THEN
         ELSE
            WriteString(' - input undefined') ; WriteLn
         END
      END ;
      INC(i)
   END ;
   s^.inchar := r ;
   s^.outchar := w ;
   s^.errchar := Scn.Write
END SetStream ;


(*
   GetReadProc - returns true if a correct read file was specified.
                 The read procedure variable, r, is set to the
                 specified procedure.
*)

PROCEDURE GetReadProc (a: ARRAY OF CHAR; VAR r: ProcRead) : BOOLEAN ;
BEGIN
   (* Table of Device entries *)
   (* TTIO *)
   IF    StrEqual(a, '/dev/tty0')  THEN r := TTY0r  ; RETURN( TRUE )

   (* Serial Port A *)
   ELSIF StrEqual(a, '/dev/tty1')  THEN r := TTY1r  ; RETURN( TRUE )

   (* Serial Port B *)
   ELSIF StrEqual(a, '/dev/tty2')  THEN r := TTY2r  ; RETURN( TRUE )
(*
   (* SerIOA4 - Read and Write procedures *)
   ELSIF StrEqual(a, '/dev/tty1a') THEN r := TTY1Ar ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty1b') THEN r := TTY1Br ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty1c') THEN r := TTY1Cr ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty1d') THEN r := TTY1Dr ; RETURN( TRUE )

   (* SerIOB4 - Read and Write procedures *)
   ELSIF StrEqual(a, '/dev/tty2a') THEN r := TTY2Ar ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty2b') THEN r := TTY2Br ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty2c') THEN r := TTY2Cr ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty2d') THEN r := TTY2Dr ; RETURN( TRUE )
*)

   (* Virtual Consoles 1..4 *)
   ELSIF StrEqual(a, '/dev/tty0a') THEN r := TTY0Ar ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty0b') THEN r := TTY0Br ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty0c') THEN r := TTY0Cr ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty0d') THEN r := TTY0Dr ; RETURN( TRUE )

   ELSIF StrEqual(a, '/dev/nul')   THEN r := NULr   ; RETURN( TRUE )

   ELSE
      RETURN( FALSE )   (* No entry found - return false *)
   END
END GetReadProc ;


(*
   GetWriteProc - returns true if a correct write file was specified.
                  The write procedure variable, w, is set to the
                  specified procedure.
*)

PROCEDURE GetWriteProc (a: ARRAY OF CHAR; VAR w: ProcWrite) : BOOLEAN ;
BEGIN
   (* Table of Device entries *)
   IF    StrEqual(a, '/dev/tty0')  THEN w := TTY0w  ; RETURN( TRUE )
   (* Serial Port A *)
   ELSIF StrEqual(a, '/dev/tty1')  THEN w := TTY1w  ; RETURN( TRUE )
   (* Serial Port B *)
   ELSIF StrEqual(a, '/dev/tty2')  THEN w := TTY2w  ; RETURN( TRUE )
(*
   (* Serial A4 (4 serial ports on comm1 *)
   ELSIF StrEqual(a, '/dev/tty1a') THEN w := TTY1Aw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty1b') THEN w := TTY1Bw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty1c') THEN w := TTY1Cw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty1d') THEN w := TTY1Dw ; RETURN( TRUE )
*)
(*
   (* Serial A4 (4 serial ports on comm2 *)
   ELSIF StrEqual(a, '/dev/tty2a') THEN w := TTY2Aw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty2b') THEN w := TTY2Bw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty2c') THEN w := TTY2Cw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty2d') THEN w := TTY2Dw ; RETURN( TRUE )
*)
   (* Virtual Consoles 1..4 *)
   ELSIF StrEqual(a, '/dev/tty0a') THEN w := TTY0Aw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty0b') THEN w := TTY0Bw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty0c') THEN w := TTY0Cw ; RETURN( TRUE )
   ELSIF StrEqual(a, '/dev/tty0d') THEN w := TTY0Dw ; RETURN( TRUE )

   ELSIF StrEqual(a, '/dev/nul')   THEN w := NULw   ; RETURN( TRUE )

   ELSE
      RETURN( FALSE )   (* No entry found - return false *)
   END
END GetWriteProc ;


(*
   LsDev - displays the character devices.
*)

PROCEDURE LsDev ;
BEGIN
   WriteString('/dev/nul') ; WriteLn ;
   WriteString('/dev/tty0') ; WriteLn ;
   WriteString('/dev/tty1') ; WriteLn ;
   WriteString('/dev/tty2') ; WriteLn ;
   WriteString('/dev/tty0a') ; WriteLn ;
   WriteString('/dev/tty0b') ; WriteLn ;
   WriteString('/dev/tty0c') ; WriteLn ;
   WriteString('/dev/tty0d') ; WriteLn ;
(*
   WriteString('/dev/tty1a') ; WriteLn ;
   WriteString('/dev/tty1b') ; WriteLn ;
   WriteString('/dev/tty1c') ; WriteLn ;
   WriteString('/dev/tty1d') ; WriteLn ;
   WriteString('/dev/tty2a') ; WriteLn ;
   WriteString('/dev/tty2b') ; WriteLn ;
   WriteString('/dev/tty2c') ; WriteLn ;
   WriteString('/dev/tty2d') ; WriteLn ;
*)
END LsDev ;


(*
   StopRead - a Read procedure that suspends the current process.
*)

PROCEDURE StopRead (VAR ch: CHAR) ;
BEGIN
   Suspend(CurrentProcess) ;
   Read(ch)
END StopRead ;


(*
   StopWrite - a Write procedure that suspends the current process.
*)

PROCEDURE StopWrite (ch: CHAR) ;
BEGIN
   Suspend(CurrentProcess) ;
   Write(ch)
END StopWrite ;


(*
   NULr - stops the current process.
*)

PROCEDURE NULr (VAR ch: CHAR) ;
BEGIN
   StopRead(ch)
END NULr ;


(*
   NULw - eats up characters.
*)

PROCEDURE NULw (ch: CHAR) ;
BEGIN
END NULw ;


PROCEDURE TTY0r (VAR ch: CHAR) ; BEGIN TTIO.Read(ch) ; END TTY0r ;
PROCEDURE TTY1r (VAR ch: CHAR) ; BEGIN SerIOA.Read(ch) ; END TTY1r ;
PROCEDURE TTY2r (VAR ch: CHAR) ; BEGIN SerIOB.Read(ch) ; END TTY2r ;

PROCEDURE TTY0w (ch: CHAR) ; BEGIN TTIO.Write(ch) ; END TTY0w ;
PROCEDURE TTY1w (ch: CHAR) ; BEGIN SerIOA.Write(ch) ; END TTY1w ;
PROCEDURE TTY2w (ch: CHAR) ; BEGIN SerIOB.Write(ch) ; END TTY2w ;


(* Consoles - Read and Write procedures *)

PROCEDURE TTY0Ar (VAR ch: CHAR) ; BEGIN Consoles.Read(1, ch) ; END TTY0Ar ;
PROCEDURE TTY0Br (VAR ch: CHAR) ; BEGIN Consoles.Read(2, ch) ; END TTY0Br ;
PROCEDURE TTY0Cr (VAR ch: CHAR) ; BEGIN Consoles.Read(3, ch) ; END TTY0Cr ;
PROCEDURE TTY0Dr (VAR ch: CHAR) ; BEGIN Consoles.Read(4, ch) ; END TTY0Dr ;

PROCEDURE TTY0Aw (ch: CHAR) ; BEGIN Consoles.Write(1, ch) ; END TTY0Aw ;
PROCEDURE TTY0Bw (ch: CHAR) ; BEGIN Consoles.Write(2, ch) ; END TTY0Bw ;
PROCEDURE TTY0Cw (ch: CHAR) ; BEGIN Consoles.Write(3, ch) ; END TTY0Cw ;
PROCEDURE TTY0Dw (ch: CHAR) ; BEGIN Consoles.Write(4, ch) ; END TTY0Dw ;


(* SerIOB4 - Read and Write procedures *)
(*
PROCEDURE TTY2Ar (VAR ch: CHAR) ; BEGIN SerIOB4.Read1(ch) ; END TTY2Ar ;
PROCEDURE TTY2Br (VAR ch: CHAR) ; BEGIN SerIOB4.Read2(ch) ; END TTY2Br ;
PROCEDURE TTY2Cr (VAR ch: CHAR) ; BEGIN SerIOB4.Read3(ch) ; END TTY2Cr ;
PROCEDURE TTY2Dr (VAR ch: CHAR) ; BEGIN SerIOB4.Read4(ch) ; END TTY2Dr ;

PROCEDURE TTY2Aw (ch: CHAR) ; BEGIN SerIOB4.Write1(ch) ; END TTY2Aw ;
PROCEDURE TTY2Bw (ch: CHAR) ; BEGIN SerIOB4.Write2(ch) ; END TTY2Bw ;
PROCEDURE TTY2Cw (ch: CHAR) ; BEGIN SerIOB4.Write3(ch) ; END TTY2Cw ;
PROCEDURE TTY2Dw (ch: CHAR) ; BEGIN SerIOB4.Write4(ch) ; END TTY2Dw ;
*)


END StreamDev.
