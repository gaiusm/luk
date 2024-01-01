MODULE rtstest ;


FROM TTIO IMPORT Read ;
FROM StrIO IMPORT WriteString ;
FROM StdIO IMPORT Write, PushOutput ;
FROM SYSTEM IMPORT OnOrOff, TurnInterrupts ;
IMPORT Consoles ;
FROM MonStrIO IMPORT DebuggingStream ;
FROM SLoad IMPORT LoadProgram ;


(*
   LocalDebug - 
*)

PROCEDURE LocalDebug (ch: CHAR) ;
BEGIN
(*   DebugIO.Write(ch) ; *)
   Consoles.Write(1, ch)
END LocalDebug ;



VAR
   ch : CHAR ;
   Old: OnOrOff ;
BEGIN
   Old := TurnInterrupts(On) ;
   Consoles.SetCurrentConsole(1) ;
   PushOutput(LocalDebug) ;
   DebuggingStream(LocalDebug) ;
   LOOP
      WriteString('hit the b key to boot') ;
      Read(ch) ;
      IF ch='b'
      THEN
         LoadProgram
      END
   END
END rtstest.
