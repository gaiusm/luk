IMPLEMENTATION MODULE DebugIO ;


FROM ASCII IMPORT dc1, dc3 ;
FROM DeviceConfiguration IMPORT Parity ;
FROM SYSTEM IMPORT INBYTE, OUTBYTE, LISTEN ;

(* Set up constants for the ports that are accessed.          *)

CONST
   (* the following constant declaration may be modified to change *)
   (* the module for the alternate communications adapter.         *)

   Com1 = 0 ; (* = 1 means TRUE  or COM1 is used *)
              (* = 0 means FALSE or COM2 is used *)

   (* IOBase = 2F0H + Com1 * 100H; *)
   (* IOBase = 01A0H ; *)
   IOBase = 01A0H - 08H ;

   LineContrReg    = IOBase+0BH; (* to specify format of transmitted data  *)
   LowBaudRateDiv  = IOBase+08H; (* lower byte of divisor                  *)
   HighBaudRateDiv = IOBase+09H; (* higher byte of divisor                 *)
   LineStatusReg   = IOBase+0DH; (* holds status info on the data transfer *)
   ReceiverReg     = IOBase+08H; (* received char is in this register      *)
   TransmitReg     = IOBase+08H; (* char to send is to put in this reg     *)
   IntEnableReg    = IOBase+09H; (* to enable the selected interrupt       *)
   ModemContrReg   = IOBase+0CH; (* controls the interface to a modem      *)
   IntIdentReg     = IOBase+0AH; (* Determines what caused interrupt.      *)
   ModemStatusReg  = IOBase+0EH; (* Determines status on the ctrl lines.   *)

   AsyncInterrupt  = 0BH+Com1 ;  (* vector used by communications contr.   *)

   DeviceMaskBit   = 3+Com1 ;    (* bit in device mask for asynchronous    *)
                                 (* communications controller              *)


   (* Interrupt priority level for async. comm. contr. *)

   CommCtrlLevel   = 7-DeviceMaskBit ;

   (* Line Status Registers Bits *)

   DataReadyBit         = 00H ;
   TxHoldingRegEmptyBit = 05H ;
   TxShiftRegEmptyBit   = 06H ;

   (* Interrupt Enable Register Bits *)

   EnableDataAvailableInt     = 00H ;
   EnableTxHoldingRegEmptyInt = 01H ;
   EnableModemStatusInt       = 03H ;


   (* CONSTANTS for the 8259 Interrupt controller *)

   I8259CtrlWord1 = 21H;  (* Interrupt controller,
                              Operation Control Word 1 *)
   I8259CtrlWord2 = 20H;  (* Interrupt controller,
                              Operation Control Word 2 *)
   EndOfInterrupt = 20H;   (* code to send to the 8259 in the ISR *)

(*
   LineContrReg   = 3FBH ;
   LineStatusReg  = 3FDH ;
   AsyncInterrupt = 00CH ;
   ReceiverReg    = 3F8H ;
   TransmitReg    = 3F8H ;
   IntEnableReg   = 3F9H ;
   IntIdentReg    = 3FAH ;
   IntCtrlMask    = 021H ;
   CommCtrlLevel  = 004H ;
   ModemCtrlReg   = 3FCH ;
   ModemStatusReg = 3FEH ;
*)


PROCEDURE DLAB (Bit: CARDINAL) ;
VAR
   Status : BITSET ;
   Byte   : CHAR ;
BEGIN
   INBYTE( LineContrReg, Byte ) ;
   Status := BITSET( ORD( Byte ) ) ;
   IF Bit=1
   THEN
      INCL(Status, 7)
   ELSE
      EXCL(Status, 7)
   END ;
   OUTBYTE(LineContrReg, CHR(CARDINAL(Status))) ;
END DLAB ;


(*
   Init - Initializes the serial port to specific values.
          The legal values for the parameters are:
             BaudRate  : 300..9600
             StopBits  : 1..2
             WordLength: 5..8
             Parity    : None, Odd, Even
*)

PROCEDURE Init (BaudRate, StopBits, WordLength: CARDINAL ;
                p: Parity ; VAR Done: BOOLEAN) ;
VAR
   DivisorLow,
   DivisorHigh: CARDINAL ;
   Parameters : BITSET ;

BEGIN
   Done := TRUE ;
   CASE BaudRate OF

   300   : DivisorLow := 80H ;
           DivisorHigh := 1H |

   600   : DivisorLow := 0C0H ;
           DivisorHigh := 0H |

   1200  : DivisorLow := 60H ;
           DivisorHigh := 0H |

   2400  : DivisorLow := 30H ;
           DivisorHigh := 0H |

   4800  : DivisorLow := 18H ;
           DivisorHigh := 0H |

   9600  : DivisorLow := 0CH ;
           DivisorHigh := 0H |

   19200 : DivisorLow := 06H ;
           DivisorHigh := 0H

   ELSE
      Done := FALSE
   END ;
   IF Done
   THEN
      (* load the Divisor of the baud rate generator: *)
      DLAB(1) ;
      OUTBYTE(HighBaudRateDiv, DivisorHigh) ;
      OUTBYTE(LowBaudRateDiv, DivisorLow) ;
      DLAB(0) ;

      (* prepare the Parameters: *)
      Parameters := {};
      IF StopBits=2
      THEN
         INCL(Parameters, 2) ;
      ELSIF StopBits#1
      THEN
         Done := FALSE
      END ;
      IF (p=Odd) OR (p=Even)
      THEN
         INCL(Parameters, 3) ;
         IF p=Even
         THEN
            INCL(Parameters, 4)
         END
      END ;
      CASE WordLength OF

      5 : |
      6 : INCL(Parameters, 0) |
      7 : INCL(Parameters, 1) |
      8 : INCL(Parameters, 0) ;
          INCL(Parameters, 1)

      ELSE
         Done := FALSE
      END ;
      IF Done
      THEN
         OUTBYTE(LineContrReg, Parameters)
      END
   END
END Init ;



PROCEDURE SetUpCommPort ;
VAR
   BitSet: BITSET ;
   ch    : CHAR ;
BEGIN
   (* Disable Interrupts *)
   DLAB(0) ;
   OUTBYTE(LineContrReg, 0H) ;
   OUTBYTE(IntEnableReg, 0H) ;

   (* clear the controller: *)
   INBYTE (LineStatusReg, BitSet);
   IF DataReadyBit IN BitSet
   THEN
      (* data ready *)
      INBYTE (ReceiverReg, ch);
   END ;

   IF TxHoldingRegEmptyBit IN BitSet
   THEN
      OUTBYTE (TransmitReg, ch)
   END ;

   (* We can set the line status to {} see page 1-213 of the IBM-PC *)
   (* technical manual.                                             *)
   OUTBYTE(LineStatusReg, 00H) ;   (* Controller cleared *)

END SetUpCommPort ;



PROCEDURE Read (VAR ch: CHAR) ;
VAR
   BitSet: BITSET ;
BEGIN
   (* Set DTR active *)
   INBYTE(ModemContrReg, BitSet) ;
   INCL(BitSet, 0) ;
   OUTBYTE(ModemContrReg, BitSet) ;
   (* wait until character has arrived in the receiver register *)
   REPEAT
      INBYTE(LineStatusReg, BitSet)
   UNTIL DataReadyBit IN BitSet ;
   (* ok now read data *)
   INBYTE(ReceiverReg, ch) ;
   (* Set DTR Non Active *)
   INBYTE(ModemContrReg, BitSet) ;
   EXCL(BitSet, 0) ;
   OUTBYTE(ModemContrReg, BitSet)
END Read ;


PROCEDURE Write (ch: CHAR) ;
VAR
   BitSet: BITSET ;
BEGIN
(*
   (* Set RTS active - purely as a precaution against cables which *)
   (* double back the RTS and CTS wires!                           *)
   INBYTE(ModemContrReg, BitSet) ;
   INCL(BitSet, 1) ;    (* RTS bit *)
   OUTBYTE(ModemContrReg, BitSet) ;
   (* wait until transmitter buffer is totally free *)
   REPEAT
      INBYTE(LineStatusReg, BitSet)
   UNTIL (TxHoldingRegEmptyBit IN BitSet) AND (TxShiftRegEmptyBit IN BitSet) ;
   (* wait until CTS becomes active *)
   REPEAT
      INBYTE(ModemStatusReg, BitSet)
   UNTIL 4 IN BitSet ;
   OUTBYTE(TransmitReg, ch) ;
   (* Set RTS non active - purley as a precaution for compatibility *)
   INBYTE(ModemContrReg, BitSet) ;
   EXCL(BitSet, 1) ;    (* RTS bit *)
   OUTBYTE(ModemContrReg, BitSet) ;
*)
   WHILE NOT BusyWrite(ch) DO END
END Write ;



PROCEDURE BusyWrite (ch: CHAR) : BOOLEAN ;
VAR
   BitSet: BITSET ;
   ok    : BOOLEAN ;
BEGIN
   (* wait until transmitter buffer is totally free *)
   INBYTE(LineStatusReg, BitSet) ;
   ok := (TxHoldingRegEmptyBit IN BitSet) AND (TxShiftRegEmptyBit IN BitSet) ;
   IF ok
   THEN
      OUTBYTE(TransmitReg, ch)
   END ;
   RETURN( ok )
END BusyWrite ;



VAR
   Done: BOOLEAN ;

BEGIN
   SetUpCommPort ;
   Init( 9600, 1, 7, Even, Done ) ;
   IF NOT Done
   THEN
      HALT
   END
END DebugIO.
