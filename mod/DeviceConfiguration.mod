IMPLEMENTATION MODULE DeviceConfiguration ;


FROM SYSTEM IMPORT TurnInterrupts, OnOrOff,
                   BITSET, BYTE ;

FROM PortIO IMPORT In8, Out8 ;


CONST
   LineContrReg = 3 ;


(*
   GetIrqNo - returns the irq number for a UART.
              This function returns 256 if UART has no IRQ
              else IRQ level (2-7).

              It disables CPU ints.
              generates an interrupt from UART
              checks to see which IRQ has gone high.
              returns irq level.

              I don't fully understand what is happening
              when the code interogates the 8259 I guess
              I would if I read the data sheets for this
              interrupt controller. I translated the code
              from the C code given in ../../Serial
*)

PROCEDURE GetIrqNo (BaseAddress: CARDINAL) : CARDINAL ;
VAR
   OldState: OnOrOff ;
   none, mask, irq, imr, ier, lcr, mcr: BYTE ;
BEGIN
   (*
      Taken from the Serial documentation gathered from the net.

      This useful function has been provided by Mike Surikov; it allows you to
      detect which interrupt is used by a certain UART.
   *)
   OldState := TurnInterrupts(Off) ;            (* disable CPU interrupts *)

   lcr := In8(BaseAddress+3) ;                  (* read lcr               *)
   DLAB(BaseAddress, 0) ;                       (* Clear DLAB             *)

   ier := In8(BaseAddress+1) ;                  (* Read IER               *)
   Out8(BaseAddress+1, 000H) ;                  (* Disable UART ints      *)
   mcr := In8(BaseAddress+4) ;                  (* Read MCR               *)
   mcr := VAL(BYTE, (VAL(BITSET, mcr) - {4}) + {2,3}) ;
                                                (* Enable UART interrupt  *)
                                                (* generation             *)
   Out8(BaseAddress+4, mcr) ;
   imr := In8(021H) ;                           (* Read the interrupt     *)
                                                (* mask register.         *)
   Out8(020H, 00AH) ;                           (* Prepare to read the    *)
                                                (* IRR                    *)
   (* Here transmitter must be already empty *)

   mask := 0FCH ;                               (* The mask for IRQ2-7    *)
   Out8(BaseAddress+1, 002H) ;                  (* Enable 'Transmitter    *)
                                                (* Empty' interrupt.      *)
   mask := VAL(BYTE,
               VAL(BITSET, mask) * VAL(BITSET, In8(020H))) ;
                                                (* Select risen interrupt *)
                                                (* request.               *)

   Out8(BaseAddress+1, 000H) ;                  (* Disable 'Transmitter   *)
                                                (* Empty' interrupt.      *)
   mask := VAL(BYTE,
               VAL(BITSET, mask) *
               ({7,6,5,4,3,2,1,0} - VAL(BITSET, In8(020H)))) ;
                                                (* Select fallen          *)
                                                (* interrupt request.     *)
   Out8(BaseAddress+1, 002H) ;                  (* Enable 'Transmitter    *)
                                                (* Empty' interrupt.      *)
   mask := VAL(BYTE,
               VAL(BITSET, mask) *
               VAL(BITSET, In8(020H))) ;        (* Select risen interrupt *)
                                                (* request.               *)
   Out8(021H, VAL(BYTE,
                  {7,6,5,4,3,2,1,0} - VAL(BITSET, mask))) ;
                                                (* Unmask only this       *)
                                                (* interrupt(s)           *)
   Out8(020H, 00CH) ;                           (* Enter the poll mode    *)
   irq := In8(020H) ;                           (* Accept the high level  *)
                                                (* interrupt.             *)
   none := In8(BaseAddress+5) ;                 (* Read LSR to reset line *)
                                                (* status interrupt.      *)
   none := In8(BaseAddress+0) ;                 (* Read RBR to reset data *)
                                                (* ready interrupt.       *)
   none := In8(BaseAddress+2) ;                 (* Read IIR to reset      *)
                                                (* transmitter empty      *)
                                                (* interrupt.             *)
   none := In8(BaseAddress+6) ;                 (* Read MSR to reset      *)
                                                (* modem status interrupt *)
   Out8(BaseAddress+1, ier) ;                   (* Restore Interrupt      *)
                                                (* Enable Reg.            *)
   Out8(BaseAddress+3, lcr) ;                   (* Restore Line Control   *)
                                                (* Reg.                   *)
   Out8(BaseAddress+4, mcr) ;                   (* Restore Modem Control  *)
                                                (* Reg.                   *)
   Out8(020H, 020H) ;                           (* End of interrupt mode. *)
   Out8(021H, imr) ;                            (* Restore Interrupt Mask *)
                                                (* Reg.                   *)
   OldState := TurnInterrupts(OldState) ;

   IF 7 IN VAL(BITSET, irq)
   THEN
      RETURN VAL(CARDINAL, VAL(BITSET, irq) * {0,1,2})
   ELSE
      RETURN( 256 )
   END
END GetIrqNo ;


PROCEDURE DLAB (BaseAddress: CARDINAL; Bit: CARDINAL) ;
VAR
   Status : BITSET ;
   Byte   : CHAR ;
BEGIN
   Byte := In8( BaseAddress+LineContrReg ) ;
   Status := BITSET( ORD( Byte ) ) ;
   IF Bit=1
   THEN
      INCL(Status, 7)
   ELSE
      EXCL(Status, 7)
   END ;
   Out8(BaseAddress+LineContrReg, VAL(BYTE, Status))
END DLAB ;


END DeviceConfiguration.
