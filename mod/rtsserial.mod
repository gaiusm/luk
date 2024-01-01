MODULE rtsserial ;

IMPORT Scn ;
IMPORT TTIO ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT PushOutput ;
FROM SYSTEM IMPORT OnOrOff, TurnInterrupts ;
(* IMPORT Consoles ; *)
FROM MonStrIO IMPORT DebuggingStream ;
FROM SerIOA IMPORT Write, Read, Init ;
FROM DeviceConfiguration  IMPORT Parity ;
FROM ASCII IMPORT cr, lf ;
FROM Ps IMPORT DoPs ;
FROM NumberIO IMPORT WriteCard ;
FROM Kernel IMPORT ProcType, InitProcess, Resume, Sleep, PtrToProcDes ;


(*
   LocalDebug - 
*)

PROCEDURE LocalDebug (ch: CHAR) ;
BEGIN
(*   Consoles.Write(1, ch) *)
END LocalDebug ;


(*
   Timer - debugging handle.
*)

PROCEDURE Timer ;
BEGIN
   LOOP
      Sleep(1200) ;  (* 60 secs *)
      DoPs ;
   END
END Timer ;


(*
   CheckOut - 
*)

PROCEDURE CheckOut ;
VAR
   ch: CHAR ;
   i : CARDINAL ;
BEGIN
   FOR i := 0 TO 9 DO
      Write(cr) ; Write(lf) ;
      FOR ch := 'a' TO 'z' DO
         Write(ch)
      END ;
      FOR ch := 'A' TO 'Z' DO
         Write(ch)
      END ;
      FOR ch := '0' TO '9' DO
         Write(ch)
      END ;
      Write(cr) ; Write(lf)
   END
END CheckOut ;


VAR
   ch : CHAR ;
   Old: OnOrOff ;
   db : PtrToProcDes ;
BEGIN
   InitProcess(db, Timer, 0, 1, 50000, 0, System, NIL, 'DeadMansHandle') ;
   Resume(db) ;

   Old := TurnInterrupts(On) ;
   PushOutput(Scn.Write) ;
   DebuggingStream(Scn.Write) ;
   WriteString('\nstart typing keys on the remote terminal..\n') ;
   CheckOut ;
   DoPs ;

   Write('-') ; Write('>') ; Write(' ') ; Write(ch) ;
   LOOP
      Read(ch) ;
      Scn.Write(ch) ;
      Write(ch) ;
      IF ch='c'
      THEN
         CheckOut
      END ;
      IF ch='x'
      THEN
         Scn.Write('E') ;
         Scn.Write('N') ;
         Scn.Write('B') ;
         IF Init(38400, 1, 8, None, TRUE)
         THEN
         END
      END ;
      IF ch='z'
      THEN
         Scn.Write('D') ;
         Scn.Write('I') ;
         Scn.Write('S') ;
         IF Init(38400, 1, 8, None, FALSE)
         THEN
         END
      END
   END
END rtsserial.
