IMPLEMENTATION MODULE IRQ ;



FROM MonStrIO IMPORT WriteCard, WriteString ;
FROM InterruptVector IMPORT InitInterruptVector ;

FROM SYSTEM IMPORT BYTE, BITSET,
                   TurnInterrupts, OnOrOff ;

FROM PortIO IMPORT In8, Out8 ;


VAR
   IRQHandler: ARRAY [0..MaxIRQ] OF ProcIRQ ;
   Cache21,
   CacheA1: BITSET ;   (* cache bytes for the two 8259s *)


(*
   DisableIRQ - disable irq, IrqNo.
*)

PROCEDURE DisableIRQ (IrqNo: CARDINAL) ;
VAR
   Back: OnOrOff ;
BEGIN
   Back := TurnInterrupts(Off) ;
   IF IrqNo<8
   THEN
      INCL(Cache21, IrqNo) ;
      Out8(021H, VAL(BYTE, Cache21))
   ELSIF IrqNo<16
   THEN
      IrqNo := IrqNo MOD 8 ;
      INCL(CacheA1, IrqNo) ;
      Out8(0A1H, VAL(BYTE, CacheA1))
   END ;
   Back := TurnInterrupts(Back)
END DisableIRQ ;


(*
   EnableIRQ - enable irq, IrqNo.
*)

PROCEDURE EnableIRQ (IrqNo: CARDINAL) ;
VAR
   Back: OnOrOff ;
BEGIN
   Back := TurnInterrupts(Off) ;
(*
   WriteString('\nEnableIRQ ') ; WriteCard(IrqNo, 4) ;
*)
   IF IrqNo<8
   THEN
      EXCL(Cache21, IrqNo) ;
      Out8(021H, VAL(BYTE, Cache21))
   ELSIF IrqNo<16
   THEN
      IrqNo := IrqNo MOD 8 ;
      EXCL(CacheA1, IrqNo) ;
      Out8(0A1H, VAL(BYTE, CacheA1))
   END ;
   Back := TurnInterrupts(Back)
END EnableIRQ ;


(*
   InitIRQ - initialises irq, IrqNo, to call PROCEDURE, p,
             when this interrupt occurs.
*)

PROCEDURE InitIRQ (IrqNo: CARDINAL; p: ProcIRQ) ;
VAR
   Back: OnOrOff ;
BEGIN
   Back := TurnInterrupts(Off) ;
   (* WriteString('\nInitIRQ ') ; WriteCard(IrqNo, 4) ; *)
   IRQHandler[IrqNo] := p ;
   IF IrqNo<8
   THEN
      EXCL(Cache21, IrqNo) ;
      Out8(021H, VAL(BYTE, Cache21))
   ELSIF IrqNo<16
   THEN
      EXCL(Cache21, 4) ;
      EXCL(CacheA1, IrqNo-8) ;
      Out8(021H, VAL(BYTE, Cache21)) ;
      Out8(0A1H, VAL(BYTE, CacheA1))
   ELSE
      WriteString('InitIRQ IrqNo out of range:') ; WriteCard(IrqNo, 4)
   END ;
   Back := TurnInterrupts(Back)
END InitIRQ ;


(*
   GenericHandler - the generic handler which will call the
                    appropriate IRQ handler.
*)

PROCEDURE GenericHandler (IntNo: CARDINAL) ;
VAR
   irq: CARDINAL ;
   b  : BYTE ;
BEGIN
(*
   WriteString('\nInside GenericHandler') ;
*)
   IF (IntNo>=32) AND (IntNo<48)
   THEN
      (* ok correct interrupt range for our IRQ *)
      irq := IntNo-32 ;
      IF irq<8
      THEN
         b := In8(021H) ;
         (* Turn 8259 interrupt irq off *)
         INCL(Cache21, irq) ;
         Out8(021H, VAL(BYTE, Cache21)) ;
         Out8(020H, 020H) ;
         IRQHandler[irq](IntNo)
         (* user must turn 8259 irq back on again *)
      ELSE
         b := In8(0A1H) ;
         (* Turn 8259 interrupt irq off *)
         INCL(CacheA1, irq-8) ;
         Out8(0A1H, VAL(BYTE, CacheA1)) ;
         Out8(0A0H, 020H) ;
         Out8(020H, 020H) ;
         IRQHandler[irq](IntNo)
         (* user must turn 8259 irq back on again *)
      END
   ELSE
      WriteString('error IRQ handler expecting interrupt in range 32..47  ') ;
      WriteCard(IntNo, 4) ;
      HALT
   END
END GenericHandler ;


(*
   UnscheduledIRQ - writes an error message and then terminates.
*)

PROCEDURE UnscheduledIRQ (IrqNo: CARDINAL) ;
BEGIN
   WriteString('unscheduled IRQ received:') ; WriteCard(IrqNo-32, 4)
END UnscheduledIRQ ;


(*
   KillIRQ - removes an IRQ handler.
*)

PROCEDURE KillIRQ (IrqNo: CARDINAL) ;
BEGIN
   DisableIRQ(IrqNo) ;
   IRQHandler[IrqNo] := UnscheduledIRQ
END KillIRQ ;


(*
   IgnoreIRQ - used to service the cascade IRQ number 2.
*)

PROCEDURE IgnoreIRQ (IrqNo: CARDINAL) ;
BEGIN
   (* nothing here *)
   EnableIRQ(2)
END IgnoreIRQ ;


(*
   Init - initializes the module data structures and assigns default
          irq handlers.
*)

PROCEDURE Init ;
VAR
   i: CARDINAL ;
BEGIN
   Cache21 := {0, 1, 2, 3, 4, 5, 6, 7} ;
   CacheA1 := {0, 1, 2, 3, 4, 5, 6, 7} ;
   FOR i := 0 TO MaxIRQ DO
      InitInterruptVector(i+020H, GenericHandler) ;
      InitIRQ(i, UnscheduledIRQ)
   END ;
   InitIRQ(2, IgnoreIRQ)    (* used to cascade two 8259s together *)
END Init ;


END IRQ.
