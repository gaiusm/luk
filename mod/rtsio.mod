MODULE rtsio ;


FROM SYSTEM IMPORT ADDRESS, PROCESS, TRANSFER, NEWPROCESS,
                   BYTE, TurnInterrupts, OnOrOff,
                   IOTRANSFER ;

FROM PortIO IMPORT In8, Out8 ;
FROM Storage IMPORT ALLOCATE ;
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

FROM Colours IMPORT Blue, Red, Brown, White, Green, Yellow ;
FROM Dump IMPORT DumpDown, DumpUp ;
FROM IRQ IMPORT InitIRQ, EnableIRQ ;



VAR
   Count: CARDINAL ;

(*
   Timer - 
*)

PROCEDURE Timer ;
VAR
   old: OnOrOff ;
BEGIN
   old := TurnInterrupts(Off) ;
   WriteString('clock starting ') ;
   LOOP
      IF (Count MOD 20) = 0
      THEN
         WriteCard(Count DIV 20, 4)
      END ;
      INC(Count) ;
      StartClock(Divisor) ;
      IOTRANSFER(p2, p1, 020H) ;
   END
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
   Out8(ControlWordReg, VAL(BYTE, {4,5})) ;

   Out8(Counter0, VAL(BYTE, Count MOD 0100H)) ;   (* Least Significant Byte *)
   Out8(Counter0, VAL(BYTE, Count DIV 0100H))     (* Most significant Byte  *)
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

   WriteString('now to create a process...workspace = ') ;

   ALLOCATE(a, StackSize) ;
   WriteHex(CARDINAL(a), 8) ;
   NEWPROCESS(Timer, a, StackSize, p2) ;
   WriteString('\naddress of process stack = ') ; WriteHex(CARDINAL(p2), 8) ;
   WriteString('\nwell that was harmless enough, now to TRANSFER...\n') ;
   DumpUp(ADDRESS(p2), 040H) ;
   WriteString('\nhere we go.......\n') ;
   TRANSFER(p1, p2) ;
   WriteString('\nnow to turn ints on') ;
   LOOP
      OldInts := TurnInterrupts(On)
   END
END rtsio.
