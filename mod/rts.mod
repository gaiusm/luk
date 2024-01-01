MODULE rts ;


FROM SYSTEM IMPORT ADDRESS, PROCESS, TRANSFER, NEWPROCESS,
                   BYTE, TurnInterrupts, OnOrOff, In, Out ;

FROM SysStorage IMPORT ALLOCATE ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT PushOutput ;
FROM NumberIO IMPORT WriteCard, WriteHex ;
FROM Scn IMPORT MoveCursor, ClearScreen ;

FROM osinit IMPORT ScreenAddress,
                   ExtendedMemoryEnd,
                   ExtendedMemoryStart,
                   BaseMemoryStart,
                   BaseMemoryEnd,
                   SizeOfOS ;

FROM WindowDevice IMPORT Window, InitWindow, SetWindow, TitleWindow,
                         WriteChar, PutOnTop ;
FROM Colours IMPORT Blue, Red, Brown, White, Green, Yellow ;
FROM Dump IMPORT DumpDown, DumpUp ;
FROM IRQ IMPORT InitIRQ, EnableIRQ ;

IMPORT WindowDevice ;



VAR
   First,
   Second   : Window ;
   Debugging: Window ;

(*
   SetupWindows - 
*)

PROCEDURE SetupWindows ;
BEGIN
   WriteString('\nBefore InitWindow') ;
   InitWindow(First) ;
   WriteString('\nBefore SetWindow') ;

   (* first process window *)
   SetWindow(First, Blue, White, 38, 9, 1, 1, TRUE) ;
   WriteString('\nBefore TitleWindow') ;
   TitleWindow(First, 'Initial process') ;

   (* second process window *)
   InitWindow(Second) ;
   SetWindow(Second, Brown, White, 36, 9, 42, 1, TRUE) ;
   TitleWindow(Second, 'Second process') ;

   (* debugging window at the bottom *)
   InitWindow(Debugging) ;
   SetWindow(Debugging, Red, White, 77, 11, 1, 12, TRUE) ;
   TitleWindow(Debugging, 'Debugging output') ;
   PutOnTop(Debugging) ;

   PushOutput(LocalWrite)
END SetupWindows ;


(*
   LocalWrite - 
*)

PROCEDURE LocalWrite (ch: CHAR) ;
BEGIN
   WindowDevice.WriteChar(Debugging, ch)
END LocalWrite ;


(*
   First - the first process to be created.
*)

PROCEDURE First ;
BEGIN
   LOOP
      WindowDevice.WriteString(First, 'look it worked...') ;
      TRANSFER(p2, p1)
   END
END First ;


(*
   Test - 
*)

PROCEDURE Test (t: CARDINAL) ;
BEGIN
   WriteString('\nStack after pusha/pushf\n') ;
   ASM
      movl    $1, %eax
      movl    $2, %ebx
      movl    $3, %ecx
      movl    $4, %edx
      pusha                       // push all registers
      pushf
      pushl   $-1                 // check to see if we see this
      pushl   %esp
      popl    %eax
      movl    %eax, 8(%ebp)       // t := esp
   END ;
   DumpUp(ADDRESS(t), 040H) ;
END Test ;



(*
   ProcessA - 
*)

PROCEDURE ProcessA ;
BEGIN
   LOOP
      WindowDevice.WriteString(Second, 'is this going to work? ') ;
      TRANSFER(p1, p2)
   END
END ProcessA ;



VAR
   Count: CARDINAL ;

(*
   Timer - 
*)

PROCEDURE Timer (IrqNo: CARDINAL) ;
BEGIN
   IF (Count MOD 20) = 0
   THEN
      WriteCard(Count DIV 20, 4)
   END ;
   INC(Count) ;
   StartClock(Divisor) ;
   EnableIRQ(0)
END Timer ;


CONST
   Divisor        = 59500 ;  (* Divisor yields a clock freq 1/20  *)

   Counter0       = 040H ;   (* Constants for the 8253 timer chip *)
   Counter1       = 041H ;
   Counter2       = 042H ;
   ControlWordReg = 043H ;


(*
   StartClock - sets the Count into clock.
*)

PROCEDURE StartClock (Count: CARDINAL) ;
BEGIN
   (* Tell 8253 that we wish to Read or Write *)
   (* to it. Mode 0 Counter 0.                *)
   Out(ControlWordReg, VAL(BYTE, {4,5})) ;

   Out(Counter0, VAL(BYTE, Count MOD 0100H)) ;   (* Least Significant Byte *)
   Out(Counter0, VAL(BYTE, Count DIV 0100H))     (* Most significant Byte  *)
END StartClock ;


CONST
   StackSize = 010000H ;


VAR
   a      : ADDRESS ;
   p1, p2 : PROCESS ;
   OldInts: OnOrOff ;
BEGIN
   WriteString('got to OS\n') ;

   WriteString('\nExtendedMem = ') ; WriteCard(CARDINAL(ExtendedMemoryEnd()), 4) ;
   WriteString('\nVideo       = ') ; WriteHex(CARDINAL(ScreenAddress()), 8) ;
   WriteString('\nI+D size    = ') ; WriteCard(SizeOfOS(), 4) ;

(*
   SetupWindows ;
*)
(*
   WriteString('lots of text to be displayed ') ;
   WindowDevice.WriteString(First, 'in this window ') ;
   WindowDevice.WriteString(Second, 'and also in this window ') ;
*)
   WriteString('now to create a process...workspace = ') ;
(*
   ALLOCATE(a, StackSize) ;
   WriteHex(CARDINAL(a), 8) ;
   NEWPROCESS(First, a, StackSize, p2) ;
   WriteString('\naddress of process stack = ') ; WriteHex(CARDINAL(p2), 8) ;
   WriteString('\nwell that was harmless enough, now to TRANSFER...\n') ;
   DumpUp(ADDRESS(p2), 040H) ;
*)
   WriteString('\nbefore InitIRQ') ;
   (*  ProcessA *)

   InitIRQ(0, Timer) ;

   Count := 0 ;
   WriteString('\nnow to enable irq 0') ;
   EnableIRQ(0) ;
   WriteString('\nnow to turn ints on') ;
   LOOP
      OldInts := TurnInterrupts(On)
   END
END rts.
