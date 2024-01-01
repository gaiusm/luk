IMPLEMENTATION MODULE Exceptions ;


FROM MonStrIO IMPORT WriteString, WriteLn, WriteHex ;
FROM InterruptVector IMPORT InitInterruptVector ;


(*
   Init - install a set of 386/486 exception handlers.
*)

PROCEDURE Init ;
BEGIN
   InitInterruptVector( 0, DivideError) ;
   InitInterruptVector( 1, DebugException) ;
   InitInterruptVector( 2, NMIInterrupt) ;
   InitInterruptVector( 3, Breakpoint) ;
   InitInterruptVector(13, GeneralProtection) ;
END Init ;



(*
   DivideError - handle the divide error.
*)

PROCEDURE DivideError (IntNo: CARDINAL) ;
BEGIN
   WriteString('divide by zero error') ;
   DumpRegisters ;
   HALT
END DivideError ;


(*
   DebugException - 
*)

PROCEDURE DebugException (IntNo: CARDINAL) ;
BEGIN
   WriteString('debug exception') ;
   DumpRegisters ;
   HALT
END DebugException ;


(*
   NMIInterrupt - handle the NMI interrupt.
*)

PROCEDURE NMIInterrupt (IntNo: CARDINAL) ;
BEGIN
   WriteString('NMI interrupt - suggest that you check your DRAM chips') ;
   DumpRegisters ;
   HALT
END NMIInterrupt ;


(*
   Breakpoint - handle the breakpoint interrupt
*)

PROCEDURE Breakpoint (IntNo: CARDINAL) ;
BEGIN
   WriteString('breakpoint interrupt - breakpoint encountered...') ;
   DumpRegisters ;
   HALT
END Breakpoint ;


(*
   GeneralProtection - handle general protection
*)

PROCEDURE GeneralProtection (IntNo: CARDINAL) ;
BEGIN
   WriteString('General protection') ;
   DumpRegisters ;
   HALT
END GeneralProtection ;


(*
   DumpRegisters - writes out the set of registers
*)

PROCEDURE DumpRegisters ;
BEGIN
   WriteString('\nRegister dump\n') ;
   WriteString('SS     DS     ES     FS     GS\n') ;
   WriteHex(GetSS(), 4) ; WriteString('   ') ;
   WriteHex(GetDS(), 4) ; WriteString('   ') ;
   WriteHex(GetES(), 4) ; WriteString('   ') ;
   WriteHex(GetFS(), 4) ; WriteString('   ') ;
   WriteHex(GetGS(), 4) ; WriteString('   ') ;
END DumpRegisters ;


(*
   GetSS - returns the SS register
*)

PROCEDURE GetSS () : CARDINAL ;
BEGIN
   (* set hi 16 bits to 0 *)
   ASM VOLATILE('mov  $0, %eax ; mov  %ss, %ax')
END GetSS ;


(*
   GetDS - returns the DS register
*)

PROCEDURE GetDS () : CARDINAL ;
BEGIN
   (* set hi 16 bits to 0 *)
   ASM VOLATILE('mov  $0, %eax ; mov  %ds, %ax')
END GetDS ;


(*
   GetES - returns the ES register
*)

PROCEDURE GetES () : CARDINAL ;
BEGIN
   (* set hi 16 bits to 0 *)
   ASM VOLATILE('mov  $0, %eax ; mov  %es, %ax')
END GetES ;


(*
   GetFS - returns the FS register
*)

PROCEDURE GetFS () : CARDINAL ;
BEGIN
   (* set hi 16 bits to 0 *)
   ASM VOLATILE('mov   $0, %eax ; mov   %fs, %ax')
END GetFS ;


(*
   GetGS - returns the GS register
*)

PROCEDURE GetGS () : CARDINAL ;
BEGIN
   (* set hi 16 bits to 0 *)
   ASM VOLATILE('mov  $0, %eax ; mov  %gs, %ax')
END GetGS ;


END Exceptions.
