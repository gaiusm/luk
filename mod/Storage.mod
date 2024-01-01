IMPLEMENTATION MODULE Storage ;

(*
   Standard dynamic storage management
*)

FROM SYSTEM IMPORT BYTE, ADR, TurnInterrupts, OnOrOff ;
FROM MemRegion IMPORT EndOfOS ;
FROM M2RTS IMPORT Halt ;

FROM osinit IMPORT ExtendedMemoryEnd,
                   ExtendedMemoryStart, MainStackSize ;



VAR
   TopOfHeap: ADDRESS ;
   Used     : CARDINAL ;
   Left     : CARDINAL ;


PROCEDURE ALLOCATE (VAR a: ADDRESS; Size: CARDINAL) ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   IF Left<Size
   THEN
      Halt(__FILE__, __LINE__, __FUNCTION__, 'not enough free memory')
   ELSE
      INC(Used, Size) ;
      a := ADDRESS(CARDINAL(TopOfHeap)-Used) ;
      DEC(Left, Size)
   END ;
   ToOldState := TurnInterrupts(ToOldState)
END ALLOCATE ;


PROCEDURE DEALLOCATE (a: ADDRESS; Size: CARDINAL) ;
BEGIN
END DEALLOCATE ;


(*
   Available - returns TRUE if, Size, bytes can be allocated.
*)

PROCEDURE Available (Size: CARDINAL) : BOOLEAN;
BEGIN
   RETURN( Size<=Left )
END Available ;


(*
   Init - initializes the heap.
          There are two different scenarios:

          (i)   that the OS is placed into base memory <1M
                in which case the heap lies 1M..MAX-Stack
          (ii)  that the OS is placed at 1M upwards in this
                more difficult case we have two heaps.

                (a)  0..base memory top
                (b)  end of OS .. MAX-Stack

          We detect which case at runtime, for now (a) is thrown away.
*)

PROCEDURE Init ;
BEGIN
   Used := 0 ;
   IF EndOfOS()<ExtendedMemoryStart()
   THEN
      (* case (i) *)
      Left := CARDINAL(ExtendedMemoryEnd() - ExtendedMemoryStart()) - MainStackSize() -1
   ELSE
      (* case (ii) *)
      Left := CARDINAL(ExtendedMemoryEnd() - EndOfOS()) - MainStackSize() -1
   END ;
   TopOfHeap := ExtendedMemoryEnd() - ADDRESS(MainStackSize()) - ADDRESS(1)
END Init ;


END Storage.
