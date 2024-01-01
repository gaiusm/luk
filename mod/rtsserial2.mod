MODULE rtsserial2 ;


FROM ASCII IMPORT dc1, dc3, cr, lf ;
FROM DeviceConfiguration IMPORT Parity ;
FROM SYSTEM IMPORT BITSET, BYTE ;
FROM PortIO IMPORT In8, Out8 ;


CONST
   (* the following constant declaration may be modified to change *)
   (* the module for the alternate communications adapter.         *)

   Com1 = 1 ; (* = 1 means TRUE  or COM1 is used *)
              (* = 0 means FALSE or COM2 is used *)

   IrqBase= 020H ;   (* irq interrupts live from 020..030 *)

   IOBase = 2F8H + Com1 * 100H;

   LineContrReg    = IOBase+03H; (* to specify format of transmitted data  *)
   LowBaudRateDiv  = IOBase+00H; (* lower byte of divisor                  *)
   HighBaudRateDiv = IOBase+01H; (* higher byte of divisor                 *)
   LineStatusReg   = IOBase+05H; (* holds status info on the data transfer *)
   ReceiverReg     = IOBase+00H; (* received char is in this register      *)
   TransmitReg     = IOBase+00H; (* char to send is to put in this reg     *)
   IntEnableReg    = IOBase+01H; (* to enable the selected interrupt       *)
   ModemContrReg   = IOBase+04H; (* controls the interface to a modem      *)
   IntIdentReg     = IOBase+02H; (* Determines what caused interrupt.      *)
   ModemStatusReg  = IOBase+06H; (* Determines status on the ctrl lines.   *)

   (* Line Status Registers Bits *)

   DataReadyBit         = 00H ;
   OverrunErrorBit      = 01H ;
   ParityErrorBit       = 02H ;
   FramingErrorBit      = 03H ;
   BreakIndicatorBit    = 04H ;
   TxHoldingRegEmptyBit = 05H ;
   TxShiftRegEmptyBit   = 06H ;
   FifoErrorBit         = 07H ;

   (* Interrupt Enable Register Bits *)

   EnableDataAvailableInt     = 00H ;
   EnableTxHoldingRegEmptyInt = 01H ;
   EnableModemStatusInt       = 03H ;


PROCEDURE DLAB (Bit: CARDINAL) ;
VAR
   Status : BITSET ;
   Byte   : BYTE ;
BEGIN
   Byte := In8(LineContrReg) ;
   Status := VAL(BITSET, Byte) ;
   IF Bit=1
   THEN
      INCL(Status, 7)
   ELSE
      EXCL(Status, 7)
   END ;
   Out8(LineContrReg, VAL(BYTE, Status))
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
   DivisorHigh: BYTE ;
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
      Out8(HighBaudRateDiv, DivisorHigh) ;
      Out8(LowBaudRateDiv, DivisorLow) ;
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
         Out8(LineContrReg, VAL(BYTE, Parameters))
      END
   END
END Init ;


PROCEDURE SetUpCommPort ;
VAR
   BitSet: BITSET ;
   ch    : BYTE ;
BEGIN
   (* Disable Interrupts *)
   DLAB(0) ;
   Out8(LineContrReg, 0H) ;
   Out8(IntEnableReg, 0H) ;

   (* clear the controller: *)
   BitSet := VAL(BITSET, In8(LineStatusReg)) ;
   IF DataReadyBit IN BitSet
   THEN
      (* data ready *)
      ch := In8(ReceiverReg)
   END ;

   IF TxHoldingRegEmptyBit IN BitSet
   THEN
      Out8(TransmitReg, ch)
   END ;

   (* We can set the line status to {} see page 1-213 of the IBM-PC *)
   (* technical manual.                                             *)
   Out8(LineStatusReg, 00H)     (* Controller cleared *)

END SetUpCommPort ;



PROCEDURE Read (VAR ch: CHAR) ;
VAR
   BitSet: BITSET ;
BEGIN
(*
   (* Set DTR active *)
   BitSet := VAL(BITSET, In8(ModemContrReg)) ;
   INCL(BitSet, 0) ;
   Out8(ModemContrReg, VAL(BYTE, BitSet)) ;
*)
   (* wait until character has arrived in the receiver register *)
   REPEAT
      BitSet := VAL(BITSET, In8(LineStatusReg))
   UNTIL DataReadyBit IN BitSet ;
   (* ok now read data *)
   ch := VAL(CHAR, In8(ReceiverReg)) ;
(*
   (* Set DTR Non Active *)
   BitSet := VAL(BITSET, In8(ModemContrReg)) ;
   EXCL(BitSet, 0) ;
   Out8(ModemContrReg, VAL(BYTE, BitSet))
*)
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
   (* Set RTS non active - purely as a precaution for compatibility *)
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
   BitSet := VAL(BITSET, In8(LineStatusReg)) ;
   ok := (TxHoldingRegEmptyBit IN BitSet) AND (TxShiftRegEmptyBit IN BitSet) ;
   IF ok
   THEN
      Out8(TransmitReg, ch)
   END ;
   RETURN( ok )
END BusyWrite ;


PROCEDURE Initialize ;
VAR
   Done: BOOLEAN ;
   ch  : CHAR ;
BEGIN
   SetUpCommPort ;
   Init( 9600, 1, 8, None, Done ) ;
   IF NOT Done
   THEN
      HALT
   END ;
   FOR ch := 'a' TO 'z' DO
      Write(ch)
   END ;
   Write(lf) ; Write(cr) ;
   FOR ch := '0' TO '9' DO
      Write(ch)
   END ;
   Write(lf) ; Write(cr) ;
   FOR ch := 'A' TO 'Z' DO
      Write(ch)
   END ;
   Write(lf) ; Write(cr) ;
   Write('>') ; Write(' ') ; 
   LOOP
      Read(ch) ;
      Write(ch)
   END
END Initialize ;


BEGIN
   Initialize
END rtsserial2.
