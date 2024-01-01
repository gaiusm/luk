IMPLEMENTATION MODULE SYSTEM ;


IMPORT M2RTS ;
IMPORT Scn ;

FROM IRQ IMPORT InitIRQ, DisableIRQ ;
FROM InterruptVector IMPORT InitInterruptVector ;
FROM StdIO IMPORT PushOutput, PopOutput ;
FROM StrIO IMPORT WriteString ;
FROM NumberIO IMPORT WriteCard ;


CONST
   MaxInterruptNo = 255 ;    (* maximum number of interrupt vectors          *)
   InterruptBit   =   9 ;    (* bit 9 of the 386/486 flags indicates ints on *)
   StartIRQ       = 020H;    (* start of vectors that are IRQ 8259 related   *)
   EndIRQ         = StartIRQ+15 ;
   DebugOn        = FALSE ;  (* should we check the interrupt state?         *)

TYPE
   PROCESS = ADDRESS ;
   IOstate = RECORD
                Used           : BOOLEAN ; (* is there a pending IOTRANSFER? *)
                IsIRQ          : BOOLEAN ; (* is this interrupt no. an IRQ?  *)
                AddressOfFirst,
                AddressOfSecond: ADDRESS ;
             END ;

VAR
   IOTransferTo: ARRAY [0..MaxInterruptNo] OF IOstate ;



(*
   TRANSFER - save the current volatile environment into, p1.
              Restore the volatile environment from, p2.
              This looks as if it might have a low overhead.
*)

PROCEDURE TRANSFER (VAR p1: PROCESS; p2: PROCESS) ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   ASM VOLATILE ("pusha ; pushf") ;    (* push all registers *)
   (* remember p1 is a VAR parameter *)
   ASM VOLATILE ("movl %[p1], %%eax ; movl %%esp, (%%eax)" :: [p1] "rm" (p1)) ;  (* p1 := top of stack  *)
   ASM VOLATILE ("movl %[p2], %%eax ; movl %%eax, %%esp" :: [p2] "rm" (p2)) ;    (* top of stack := p2 *)
   ASM VOLATILE ("popf ; popa") ;    (* restore all registers *)
   ToOldState := TurnInterrupts(ToOldState)
END TRANSFER ;


CONST
   ActivationRecordSize = 11 ;


(*
   NEWPROCESS - p is a parameterless procedure, a, is the origin of
                the workspace used for the process stack and containing
                the volatile environment of the process. n, is the amount
                in bytes of this workspace. new, is the new process.
*)

PROCEDURE NEWPROCESS (p: PROC; a: ADDRESS; n: CARDINAL; VAR new: PROCESS) ;
VAR
   ToOldState: OnOrOff ;
   oldesp,
   oldebp    : CARDINAL ;
   r         : CARDINAL ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;

   IF DebugOn
   THEN
      CheckOff
   END ;
   (* need to align workspace *)
   r := CARDINAL(a) MOD TSIZE(WORD) ;
   IF r#0
   THEN
      DEC(n, TSIZE(WORD)) ;
      INC(a, ADDRESS(TSIZE(WORD) - r))
   END ;
   (*
      p: PROC              8(%ebp)
      a: ADDRESS          12(%ebp)
      n: CARDINAL         16(%ebp)
      VAR new: PROCESS   (20(%ebp))

      NOTE:

      To create a new process we will fudge an activation record
      on the new workspace and save the new top of stack in, new.
      This activation record MUST exactly match a call to TRANSFER
      as this is how our new process will come to life.

      a   is the bottom of the stack so we must set %esp to a-n
      p   is the address of the first instruction of the new process.
          So we copy this over the top of our return address on the new
          stack.
      *   stacks start at the top of memory and grow down to lower
          addresses. The esp points to the top element on the stack.
   *)
   (* make, a, point to top of our fudged stack *)
   a := a + ADDRESS(n - ActivationRecordSize * TSIZE(WORD)) ;

   (*
      The activation record for TRANSFER will look like this:

                                                         OFFSET in DWORDS
      |--------------|
      | ip           |                                             16
      |--------------|
      | ebp          |                                             15
      |--------------|
      |              |
      | LOCAL VARS   |        ***** DANGER *****                   +3
      |              |        you must get this value from TRANSFER
      |--------------|
      | ebx          |                                             11
      |--------------|
      | edi          |                                             10
      |--------------|
      | esi          |                                              9
      |--------------|
      | all registers|
      |              |
      |        eax   |                                              8
      |        ecx   |                                              7
      |        edx   |                                              6
      |        ebx   |                                              5
      |        esp'  |                                              4
      |        ebp   |                                              3
      |        esi   |                                              2
      |        edi   |                                              1
      |--------------|
      | eflags       |                                              0
      |--------------|         <-   a

      We will construct this in the simplist way.
   *)

   oldebp := CARDINAL(a) ;   (* TOP of stack - remember stacks grow down *)

   a := PushCardinal(a, 0) ;
   a := PushCardinal(a, CARDINAL(p)) ;                       (* IP  *)
   a := PushCardinal(a, oldebp) ;                            (* ebp'' *)
   oldebp := CARDINAL(a) ;
   a := PushCardinal(a, CARDINAL(ToOldState)) ;   (* LOCAL VARIABLE DANGER *)
   a := PushCardinal(a, CARDINAL(ToOldState)) ;   (* LOCAL VARIABLE DANGER *)
   a := PushCardinal(a, CARDINAL(ToOldState)) ;   (* LOCAL VARIABLE DANGER *)
   (* These three local variables are used to restore the interrupt mask   *)

   a := PushCardinal(a, 0) ;                                 (* ebx *)
   a := PushCardinal(a, 0) ;                                 (* edi *)
   a := PushCardinal(a, 0) ;                                 (* esi *)
   oldesp := CARDINAL(a) ;
   a := PushCardinal(a, 1) ;                                 (* eax *)
   a := PushCardinal(a, 3) ;                                 (* ecx *)
   a := PushCardinal(a, 4) ;                                 (* edx *)
   a := PushCardinal(a, 2) ;                                 (* ebx *)
   a := PushCardinal(a, oldesp) ;                            (* esp` *)
   a := PushCardinal(a, oldebp) ;                            (* ebp` *)
   a := PushCardinal(a, 0) ;                                 (* esi *)
   a := PushCardinal(a, 0) ;                                 (* edi *)
   a := PushCardinal(a, CARDINAL(GetFlags())) ;              (* flags *)

   new := PROCESS(a) ;         (* TOP of stack - remember stacks grow down *)

   ToOldState := TurnInterrupts(ToOldState)
END NEWPROCESS ;


(*
   IOTRANSFER - saves the current volatile environment into, First,
                and restores volatile environment, Second.
                When an interrupt, InterruptNo, is encountered then
                the reverse takes place. (The then current volatile
                environment is shelved onto Second and First is resumed).

                NOTE: that upon interrupt the Second might not be the
                      same process as that before the original call to
                      IOTRANSFER.
*)

PROCEDURE IOTRANSFER (VAR First, Second: PROCESS; InterruptNo: CARDINAL) ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(Off) ;
   (* WriteString('\n inside IOTRANSFER') ; *)
   WITH IOTransferTo[InterruptNo] DO
      IF Used
      THEN
         PushOutput(Scn.Write) ;   (* use debugging write *)
         WriteString('IOTRANSFER already waiting for interrupt') ;
         WriteCard(InterruptNo, 4) ; HALT
      END ;
      Used            := TRUE ;
      AddressOfFirst  := ADR(First) ;
      AddressOfSecond := ADR(Second) ;
      IF IsIRQ
      THEN
         (* WriteString('\n IOTRANSFER - InitIRQ') ; *)
         InitIRQ(InterruptNo-StartIRQ, IOTHandler)
      ELSE
         (* WriteString('\n IOTRANSFER - InitIntVec') ; *)
         InitInterruptVector(InterruptNo, IOTHandler)
      END
   END ;
   (* WriteString('\n IOTRANSFER going for the 1st TRANSFER') ; *)
   TRANSFER(First, Second) ;
   (* WriteString('\n IOTRANSFER after the TRANSFER and all ok') ; *)
   (*
      we are now back running First again
      - it has all happened - lets clear up
   *)
   IF DebugOn
   THEN
      CheckOff
   END ;
   IOTransferTo[InterruptNo].Used := FALSE ;
   ToOldState := TurnInterrupts(ToOldState)
END IOTRANSFER ;


(*
   IOTHandler - handles interrupts related to a pending IOTRANSFER.
*)

PROCEDURE IOTHandler (InterruptNo: CARDINAL) ;
VAR
   Old,
   New: POINTER TO PROCESS ;
BEGIN
   (* WriteString('\n enter IOTHandler') ; *)
   IF InterruptNo<=MaxInterruptNo
   THEN
      WITH IOTransferTo[InterruptNo] DO
         IF Used
         THEN
            (* switch off irq related interrupts until the next IOTRANSFER *)
            IF IsIRQ
            THEN
               DisableIRQ(InterruptNo-StartIRQ)
            END ;
            Used := FALSE ;
            Old  := AddressOfSecond ;
            New  := AddressOfFirst ;
            (* WriteString('\n IOTHandler before 2nd TRANSFER') ; *)
            TRANSFER(Old^, New^)
            (* ;  WriteString('\n IOTHandler after 2nd TRANSFER') ; *)
         ELSE
            PushOutput(Scn.Write) ;   (* use debugging write *)
            WriteString('IOTRANSFER not expecting an interrupt') ;
            WriteCard(InterruptNo, 4) ; HALT
         END
      END
   ELSE
      PushOutput(Scn.Write) ;   (* use debugging write *)
      WriteString('IOTRANSFER received interrupt out of range') ;
      WriteCard(InterruptNo, 4) ; HALT
   END
   (* ; WriteString('\n exit IOTHandler') ; *)
END IOTHandler ;


(*
   CheckOff - 
*)

PROCEDURE CheckOff ;
BEGIN
   IF InterruptBit IN GetFlags()
   THEN
      PushOutput(Scn.Write) ;   (* use debugging write *)
      WriteString('assert failed in SYSTEM.mod: interrupts should be off') ;
      PopOutput ;
      HALT
   END
END CheckOff ;


(*
   PushCardinal - pushes a CARDINAL at address, a, and returns the
                  new stack pointer.
*)

PROCEDURE PushCardinal (a: ADDRESS; c: CARDINAL) : ADDRESS ;
VAR
   PtrToCardinal: POINTER TO CARDINAL ;
BEGIN
   PtrToCardinal := a - ADDRESS(TSIZE(CARDINAL)) ;
   PtrToCardinal^ := c ;
   RETURN( PtrToCardinal )
END PushCardinal ;


(*
   LISTEN - briefly listen for any interrupts.
*)

PROCEDURE LISTEN ;
VAR
   ToOldState: OnOrOff ;
BEGIN
   ToOldState := TurnInterrupts(On) ;
   (* interrupts are now on - now turn them back again *)
   ToOldState := TurnInterrupts(ToOldState) ;
END LISTEN ;


(*
   TurnInterrupts - switches interrupts on or off depending
                    on i. It returns the old value.
*)

PROCEDURE TurnInterrupts (Switch: OnOrOff) : OnOrOff ;
VAR
   Flags: BITSET ;
BEGIN
   Flags := GetFlags() ;
   IF Switch=On
   THEN
      InterruptsOn
   ELSE
      InterruptsOff
   END ;
   IF InterruptBit IN Flags
   THEN
      RETURN( On )
   ELSE
      RETURN( Off )
   END
END TurnInterrupts ;


(*
   InterruptsOn - turn processor interrupts on.
*)

PROCEDURE InterruptsOn ;
BEGIN
   ASM VOLATILE("sti");
END InterruptsOn ;


(*
   InterruptsOff - turn processor interrupts off.
*)

PROCEDURE InterruptsOff ;
BEGIN
   ASM VOLATILE("cli");
END InterruptsOff ;


(*
   GetFlags - returns a BITSET containing the 386/486 flags
*)

PROCEDURE GetFlags () : BITSET ;
VAR
   b: BITSET ;
BEGIN
   ASM VOLATILE("pushf ; popl %%ebx ; movl %%ebx, %[b]" : [b] "=rm" (b) :: "ebx") ;
   RETURN( b )
END GetFlags ;


(*
   IsNoIRQ - returns TRUE if interrupt number, i, is an IRQ.
*)

PROCEDURE IsNoIRQ (i: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (i>=StartIRQ) AND (i<=EndIRQ) )
END IsNoIRQ ;


(*
   Init - initialize SYSTEM data structures. IOTRANSFER process tables.
*)

PROCEDURE Init ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 0 TO MaxInterruptNo DO
      WITH IOTransferTo[i] DO
         Used  := FALSE ;
         IsIRQ := IsNoIRQ(i)
      END
   END
END Init ;


END SYSTEM.
