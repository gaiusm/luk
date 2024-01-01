IMPLEMENTATION MODULE InterruptVector ;

FROM Descriptors IMPORT IDT, InterruptGate ;
FROM Dump IMPORT DumpUp ;
FROM WordSizes IMPORT SHORTWORD, CardinalToShortWord ;
FROM SYSTEM IMPORT BYTE, ADR ;
FROM MonStrIO IMPORT WriteString, WriteLn, WriteCard, WriteHex ;


CONST
   CodeSegmentSelector =   8 ;
   MaxInterrupt        = 255 ;
   CodeInterruptId     =  20 ;    (* byte 20 of interrupt code contains id   *)
   CodeProcedureId     =  25 ;    (* byte 25 of interrupt code contains proc *)
   MaxCodeBytes        =  39 ;    (* number of bytes in our hand coded ISRs  *)

TYPE
   ISR = RECORD
            Code: IsrCode ;
            Used: BOOLEAN ;
         END ;

   IsrCode = ARRAY [0..MaxCodeBytes] OF BYTE ;

VAR
   InterruptIsr: ARRAY [0..MaxInterrupt] OF ISR ;
   IsrTemplate : IsrCode ;



(*
   ClaimIsr - claims the interrupt vector, Vector, and assigns the
              PROCEDURE, p, to be called when this interrupt fires.
              Vector should be between 0..255.
*)

PROCEDURE ClaimIsr (Vector: CARDINAL; p: ProcISR) : ADDRESS ;
VAR
   PtrToP: POINTER TO ProcISR ;
BEGIN
   WITH InterruptIsr[Vector] DO
      IF Used
      THEN
         WriteString('interrupt service routine code already in use..') ;
         HALT
      ELSE
         Used := TRUE ;
         Code := IsrTemplate ;
         (* now fill in Vector and p *)
         Code[CodeInterruptId] := VAL(BYTE, Vector) ;
         PtrToP := ADR(Code[CodeProcedureId]) ;
         PtrToP^ := p ;

         RETURN( ADR(Code) )
      END
   END
END ClaimIsr ;


(*
   InitInterruptVector - initializes interrupt, VectorNo, to call
                         PROCEDURE, p, when an interrupt occurs.
*)

PROCEDURE InitInterruptVector (VectorNo: CARDINAL; p: ProcISR) ;
VAR
   a: ADDRESS ;
BEGIN
   a := ClaimIsr(VectorNo, p) ;
   InstallIsr(VectorNo, a)
END InitInterruptVector ;


(*
   InstallIsr - installs an interrupt service routine at interrupt vector,
                VectorNo, which will call address, a.
*)

PROCEDURE InstallIsr (VectorNo: CARDINAL; a: ADDRESS) ;
BEGIN
   WITH IDT[VectorNo] DO
      Offset0To15  := CardinalToShortWord(CARDINAL(a) MOD 65536) ;
      Offset16To31 := CardinalToShortWord(CARDINAL(a) DIV 65536) ;
      Segment      := CardinalToShortWord(CodeSegmentSelector) ;
      Flags        := CardinalToShortWord(08E00H)   (* DPL = 0, Present *)
   END
END InstallIsr ;


(*
   KillInterruptVector - removes interrupt, VectorNo.
*)

PROCEDURE KillInterruptVector (VectorNo: CARDINAL) ;
BEGIN
   InterruptIsr[VectorNo].Used := FALSE ;
   InitInterruptVector(VectorNo, UnscheduledInterrupt) ;
   InterruptIsr[VectorNo].Used := FALSE
END KillInterruptVector ;


(*
   UnscheduledInterrupt - issues a warning message saying that interrupt, n,
                          occurred.
*)

PROCEDURE UnscheduledInterrupt (n: CARDINAL) ;
BEGIN
   WriteString('warning interrupt') ; WriteCard(n, 4) ;
   WriteString(' caught when no Modula-2 handler was in place') ; WriteLn ;
(*   HALT ; *)
END UnscheduledInterrupt ;


(*
   Init - sets up IsrTemplate and then initializes all interrupt vectors.
*)

PROCEDURE Init ;
VAR
   i: CARDINAL ;
BEGIN
   IsrTemplate[ 0] := 0FCH ;   (* cld *)
   IsrTemplate[ 1] := 050H ;   (* push eax *)
   IsrTemplate[ 2] := 051H ;   (* push ecx *)
   IsrTemplate[ 3] := 052H ;   (* push edx *)
   IsrTemplate[ 4] := 01EH ;   (* push ds *)
   IsrTemplate[ 5] := 006H ;   (* push es *)
   IsrTemplate[ 6] := 00FH ;   (* push fs *)
   IsrTemplate[ 7] := 0A0H ;
   IsrTemplate[ 8] := 0B8H ;   (* movl 0x00000010, %eax *)
   IsrTemplate[ 9] := 010H ;
   IsrTemplate[10] := 000H ;
   IsrTemplate[11] := 000H ;
   IsrTemplate[12] := 000H ;
   IsrTemplate[13] := 08EH ;   (* mov  ax, ds *)
   IsrTemplate[14] := 0D8H ;
   IsrTemplate[15] := 08EH ;   (* mov  ax, es *)
   IsrTemplate[16] := 0C0H ;
   IsrTemplate[17] := 08EH ;   (* mov  ax, fs *)
   IsrTemplate[18] := 0E0H ;
   IsrTemplate[19] := 068H ;   (* push  interruptnumber *)
   IsrTemplate[20] := 000H ;
   IsrTemplate[21] := 000H ;
   IsrTemplate[22] := 000H ;
   IsrTemplate[23] := 000H ;
   IsrTemplate[24] := 0B8H ;   (* movl procedure, %eax *)
   IsrTemplate[25] := 000H ;
   IsrTemplate[26] := 000H ;
   IsrTemplate[27] := 000H ;
   IsrTemplate[28] := 000H ;
   IsrTemplate[29] := 0FFH ;   (* call  %eax *)
   IsrTemplate[30] := 0D0H ;
   IsrTemplate[31] := 058H ;   (* pop  %eax   // remove parameter *)
   IsrTemplate[32] := 00FH ;   (* pop  %fs *)
   IsrTemplate[33] := 0A1H ;
   IsrTemplate[34] := 007H ;   (* pop  %es *)
   IsrTemplate[35] := 01FH ;   (* pop  %ds *)
   IsrTemplate[36] := 05AH ;   (* pop  %dx *)
   IsrTemplate[37] := 059H ;   (* pop  %cx *)
   IsrTemplate[38] := 058H ;   (* pop  %ax *)
   IsrTemplate[39] := 0CFH ;   (* iret *)
   FOR i := 0 TO MaxInterrupt DO
      KillInterruptVector(i)
   END
END Init ;


END InterruptVector.
