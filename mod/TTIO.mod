IMPLEMENTATION MODULE TTIO ;


(*
   Sadly by introducing the LEDs this module has become
   more complex. If only the LEDs did not require ack/naks..
*)

FROM SYSTEM IMPORT ADDRESS, WORD,
                   OnOrOff, TurnInterrupts, BITSET, BYTE ;

FROM PortIO IMPORT In8, Out8, In16, Out16 ;
FROM KeyBoardConvert IMPORT ScanToASCII ;
FROM Debug IMPORT DebugString ;


(*
   Import concurrent process control functions from Executive.
*)

FROM Executive IMPORT DESCRIPTOR, SEMAPHORE, InitProcess, InitSemaphore,
                      WaitForIO, Resume, Wait, Signal ;

FROM BufferDevice IMPORT ReadBuffer, WriteBuffer, InitBuffer,
                         Buffer ;


CONST
   StackArea   = 10000  ; (* Allows StackArea BYTES per PROCESS. *)
   MaxLEDBytes =     2  ; (* max bytes that can be outstanding   *)
                          (* at any time. LED control bytes      *)
   MaxNegAcks  =     4 ;  (* how many times do we try resending? *)

CONST
   KbdCtrl   = 61H ;
   KbdPort   = 60H ;
   KbdStatus = 64H ;
   KbdCmd    = 64H ;

VAR
   ToKeyboard      : ARRAY [0..MaxLEDBytes] OF BYTE ;
   OutstandingLEDs : CARDINAL ;   (* no of bytes to be sent     *)
   SentBytes       : CARDINAL ;   (* bytes already sent         *)
   EndIndex        : CARDINAL ;   (* index to the end of buffer *)
   NegativeAckCount: CARDINAL ;   (* number of naks for byte    *)
   OldScroll,
   OldNum,
   OldCaps         : BOOLEAN ;


(*
   ReadDriver - drives the Read, and when an interrupt occurs
                it sends a character to ReadDeliver.

                This procedure should be thought of as a process
                running concurrently with all other processes.
*)

PROCEDURE ReadDriver ;
CONST
   IntNo =       021H ;  (* IRQ 1 *)
VAR
   ch        : CHAR ;
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   SendCommandToKeyboard(0AEH) ;    (* enable keyboard *)
   LOOP
      WaitForIO( IntNo ) ;
      ch := In8(KbdPort) ;

      IF NOT ToDoWithLEDs(ch)
      THEN
         ScanToASCII( ReadDeliver, ch, NoSelection )
      END ;
      CheckLEDs
   END
END ReadDriver ;


(*
   NoSelection - a dummy procedure which may be extended to provide console flipping.
                 See ../../m2mus/mod/TTIO.mod and ../../m2mus/mod/Consoles.mod
                 for an example.
*)

PROCEDURE NoSelection (c: CARDINAL) ;
BEGIN
END NoSelection ;


(*
   CheckLEDs - checks to see whether any LED control info should be
               sent to the keyboard. This LED control stuff is really
               a pain. I wish the hardware would just accept the data..
*)

PROCEDURE CheckLEDs ;
BEGIN
   IF OutstandingLEDs>0
   THEN
      (* yes some bytes to send *)
      WaitUntilKeyboardReady ;
      Out8(KbdPort, ToKeyboard[SentBytes])
   END
END CheckLEDs ;


(*
   CheckAck - checks how to deal with an ack.
              Will depend on whether we are sending bytes
              from the ToKeyboard buffer or not.
*)

PROCEDURE CheckAck ;
BEGIN
   NegativeAckCount := 0 ;   (* since just received an ack *)
   IF OutstandingLEDs>0
   THEN
      (* yes we are sending bytes using the ToKeyboard buffer.
         But we know that the last byte was accepted.. *)
      (* DebugString('  ack found') ; *)
      DEC(OutstandingLEDs) ;
      INC(SentBytes)
   END
END CheckAck ;


(*
   CheckNeg - checks how to deal with a nak.
*)

PROCEDURE CheckNeg ;
BEGIN
   INC(NegativeAckCount) ;
   (* DebugString('  nak found') ; *)
   IF NegativeAckCount=MaxNegAcks
   THEN
      (* something went wrong let's reset the keyboard *)
      DebugString('something wrong with keyboard - we will reset it\n') ;
      WaitUntilKeyboardReady ;
      Out8(KbdPort, BYTE(0F4H)) ;
      OutstandingLEDs := 0 ;  (* and throw away all the LED control data *)
      SentBytes := 0
   ELSE
      (* lets try again *)
      CheckLEDs
   END
END CheckNeg ;


(*
   ToDoWithLEDs - handles all acknowledgements to do with LEDs
*)

PROCEDURE ToDoWithLEDs (ch: CHAR) : BOOLEAN ;
CONST
   Ack   = CHAR(0FAH) ;
   Nak   = CHAR(0FEH) ;
BEGIN
   (* borrowed the acknowledgement idea from linux since some keyboards
      will timeout or ignore change LED commands. *)
   IF OutstandingLEDs>0
   THEN
      (* (Linux comment):

         "0xfa, 0xfe only mean "acknowledge", "resend" for most keyboards
          but they are the key-up scancodes for PF6, PF10 on a FOCUS 9000"

         (Gaius) I just hope we are not using such keyboards for the
                 moment... mark this on a "to do list"
      *)
      IF ch=Ack
      THEN
         CheckAck ;
         RETURN( TRUE )
      ELSIF ch=Nak
      THEN
         CheckNeg ;
         RETURN( TRUE )
      ELSE
         DebugString('unknown keyboard code response\n') ;
         RETURN( FALSE )
      END
   ELSE
      RETURN( FALSE )
   END
END ToDoWithLEDs ;


(*
   InitLEDs - initializes the LED BOOLEANs
*)

PROCEDURE InitLEDs ;
BEGIN
   OutstandingLEDs := 0 ;
   SentBytes := 0 ;
   NegativeAckCount := 0
END InitLEDs ;


(*
   WaitUntilKeyboardReady - polls the keyboard waiting to the ok
                            status bit to be set.
*)

PROCEDURE WaitUntilKeyboardReady ;
CONST
   KbdOk = 1 ;
VAR
   c: CARDINAL ;
   b: BYTE ;
BEGIN
   c := 0 ;
   WHILE c<64000 DO
      b := In8(KbdStatus) ;
      IF NOT (KbdOk IN VAL(BITSET, b))
      THEN
         RETURN
      END ;
      INC(c)
   END ;
   DebugString('keyboard has gone to sleep\n')
END WaitUntilKeyboardReady ;


(*
   SendCommandToKeyboard - sends a 1 byte command to the keyboard.
*)

PROCEDURE SendCommandToKeyboard (b: BYTE) ;
BEGIN
   WaitUntilKeyboardReady ;
   Out8(KbdCmd, b)
END SendCommandToKeyboard ;


(*
   SendDataToKeyboard - sends a character TO the keyboard.
                        In fact it really placs the data into a buffer
                        which is consumed by the device driver.
*)

PROCEDURE SendDataToKeyboard (b: BYTE) ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   (* DebugString('send data to keyboard..') ; *)
   IF OutstandingLEDs=0
   THEN
      SentBytes := 0 ;
      EndIndex := 0
   END ;
   ToKeyboard[EndIndex] := b ;
   EndIndex := (EndIndex+1) MOD MaxLEDBytes ;
   INC(OutstandingLEDs) ;
   IF OutstandingLEDs=1
   THEN
      WaitUntilKeyboardReady ;
      (* DebugString('pump priming LEDs\n') ; *)
      Out8(KbdPort, ToKeyboard[0])
   END ;
   ToOldState := TurnInterrupts(ToOldState)
END SendDataToKeyboard ;


VAR
   MutexLED: SEMAPHORE ;   (* to stop multiple processes altering *)
                           (* the LEDs                            *)

(*
   SwitchLeds - switch the keyboard LEDs to the state defined
                by the BOOLEAN variables. TRUE = ON.
*)

PROCEDURE SwitchLeds (NumLock, CapsLock, ScrollLock: BOOLEAN) ;
VAR
   b: BITSET ;
BEGIN
   (* DebugString('switch LEDs...') ; *)
   Wait(MutexLED) ;
   OldCaps := CapsLock ;
   OldNum := NumLock ;
   OldScroll := ScrollLock ;
   (* encode LED byte *)
   b := {} ;
   IF NumLock
   THEN
      INCL(b, 1)
   END ;
   IF CapsLock
   THEN
      INCL(b, 2)
   END ;
   IF ScrollLock
   THEN
      INCL(b, 0)
   END ;
   SendDataToKeyboard(BYTE(0EDH)) ;
   SendDataToKeyboard(VAL(BYTE, b)) ;
   Signal(MutexLED)
END SwitchLeds ;


(*
   SwitchScroll - switches the scroll LED on or off.
*)

PROCEDURE SwitchScroll (Scroll: BOOLEAN) ;
BEGIN
   IF Scroll#OldScroll
   THEN
      SwitchLeds(OldNum, OldCaps, Scroll)
   END
END SwitchScroll ;


(*
   SwitchNum - switches the Num LED on or off.
*)

PROCEDURE SwitchNum (Num: BOOLEAN) ;
BEGIN
   IF Num#OldNum
   THEN
      SwitchLeds(Num, OldCaps, OldScroll)
   END
END SwitchNum ;


(*
   SwitchCaps - switches the Caps LED on or off.
*)

PROCEDURE SwitchCaps (Caps: BOOLEAN) ;
BEGIN
   IF Caps#OldCaps
   THEN
      SwitchLeds(OldNum, Caps, OldScroll)
   END
END SwitchCaps ;


VAR
   ReadBuf: Buffer ;       (* Dijkstra's bounded buffer         *)
   ReadDes: DESCRIPTOR ;   (* Process Descriptor of ReadDriver  *)


(*
   InitRead - initializes the read process and the read buffer.
*)

PROCEDURE InitRead ;
BEGIN
   DebugString('InitRead\n') ;
   ReadBuf := InitBuffer() ;
   DebugString('ReadBuffer initialized\n') ;
   InitLEDs ;
   MutexLED := InitSemaphore(1, 'MutexLED') ;
   ReadDes := Resume(InitProcess(ReadDriver, StackArea, 'TTioRead')) ;
   SwitchLeds(FALSE, FALSE, FALSE) ; (* note that this can only be called    *)
                                     (* AFTER ReadDriver process has started *)
   DebugString('Read process initialized\n')
END InitRead ;


PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   ReadBuffer(ReadBuf, ch)
END Read ;


(*
   ReadDeliver - places a character into the buffer, ReadBuf.
*)

PROCEDURE ReadDeliver (ch: CHAR) ;
BEGIN
   WriteBuffer(ReadBuf, ch)
END ReadDeliver ;


BEGIN
   InitRead ;
   DebugString('Exit from TTIO\n')
END TTIO.
