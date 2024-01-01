MODULE rtswin ;


IMPORT WindowDevice ;

FROM WindowDevice IMPORT Window, InitWindow, SetWindow, TitleWindow,
                         WriteChar, PutOnTop ;

FROM Kernel IMPORT InitProcess, PtrToProcDes, Resume, ProcType,
                   Suspend, CurrentProcess ;

FROM SYSTEM IMPORT TurnInterrupts, OnOrOff ;
FROM Colours IMPORT Blue, Red, Brown, White, Green, Yellow ;
FROM TTIO IMPORT Read ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT PushOutput, Write ;
FROM MonStrIO IMPORT DebuggingStream ;
FROM Ps IMPORT DoPs ;


VAR
   First,
   Second   : Window ;
   Debugging: Window ;


(*
   SetupWindows - sets up three windows, First, Second and Debugging.
                  After this procedure has been called all StdIO
                  writes will go through LocalWrite.
*)

PROCEDURE SetupWindows ;
BEGIN
   WriteString('\nBefore SetWindow') ;

   (* first process window *)
   First := SetWindow(InitWindow(), Blue, White, 38, 9, 1, 1, TRUE) ;
   WriteString('\nBefore TitleWindow') ;
   TitleWindow(First, 'Initial process') ;

   (* second process window *)
   Second := SetWindow(InitWindow(), Brown, White, 36, 9, 42, 1, TRUE) ;
   TitleWindow(Second, 'Second process') ;

   (* debugging window at the bottom *)
   Debugging := SetWindow(InitWindow(), Red, White, 77, 11, 1, 12, TRUE) ;
   TitleWindow(Debugging, 'Debugging output') ;
   PutOnTop(Debugging) ;

   DebuggingStream(LocalWrite) ;
   PushOutput(LocalWrite)
END SetupWindows ;


(*
   LocalWrite - 
*)

PROCEDURE LocalWrite (ch: CHAR) ;
BEGIN
   IF CurrentProcess=ProcA
   THEN
      WindowDevice.WriteChar(First, ch)
   ELSIF CurrentProcess=ProcB
   THEN
      WindowDevice.WriteChar(Second, ch)
   ELSE
      WindowDevice.WriteChar(Debugging, ch)
   END
END LocalWrite ;


(*
   ProcessA - 
*)

PROCEDURE ProcessA ;
BEGIN
   OldInts := TurnInterrupts(On) ;
   LOOP
      WriteString('A: is this going to work? ') ;
   END
END ProcessA ;


(*
   ProcessB - 
*)

PROCEDURE ProcessB ;
BEGIN
   OldInts := TurnInterrupts(On) ;
   LOOP
      WriteString('B: is this going to work? ') ;
   END
END ProcessB ;




CONST
   StackSize = 010000H ;


VAR
   ProcA, ProcB: PtrToProcDes ;
   OldInts     : OnOrOff ;
   ch          : CHAR ;
BEGIN
   WriteString('got to OS\n') ;

   ProcA := NIL ;
   ProcB := NIL ;
   SetupWindows ;

   WriteString('lots of text to be displayed\n') ;
   WriteString('now to create a process...\n') ;

   InitProcess(ProcA, ProcessA, 4, 1, 4000, 0, User, NIL, 'Process1') ;
   Resume(ProcA) ;
   InitProcess(ProcB, ProcessB, 4, 1, 4000, 0, User, NIL, 'Process2') ;
   Resume(ProcB) ;

   WriteString('\nnow to turn ints on\n') ;
   OldInts := TurnInterrupts(On) ;
   LOOP
      Read(ch) ;
      Write(ch) ;
      IF ch='p'
      THEN
         DoPs
      END
   END
END rtswin.
