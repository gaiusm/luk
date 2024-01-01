MODULE testctrl ;

FROM DeviceConfiguration IMPORT Parity ;
FROM SerIOA IMPORT Init, Write ;
FROM NumberIO IMPORT WriteBin, WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM SYSTEM IMPORT BITSET ;
FROM TTIO IMPORT Read ;
FROM Kernel IMPORT InitProcess, Resume, PtrToProcDes, ProcType ;
IMPORT SerIOA ;


PROCEDURE Input ;
VAR
   ch  : CHAR ;
   bits: BITSET ;
BEGIN
   LOOP
      SerIOA.Read(ch) ;
      bits := VAL(BITSET, ch) ;
      IF 1 IN bits
      THEN
         WriteString('infared on      ')
      ELSE
         WriteString('infared off     ')
      END ;
      IF 0 IN bits
      THEN
         WriteString('micro switch on ')
      ELSE
         WriteString('micro switch off')
      END ;
      WriteLn
   END
END Input ;


(*
   SetBaud - 
*)

PROCEDURE SetBaud (baud: CARDINAL) ;
VAR
   ok: BOOLEAN ;
BEGIN
   WriteString('initialize com1 with ') ;
   WriteCard(baud, 0) ;
   WriteString(', 1, 8, even, off') ;
   Init(baud, 1, 8, Even, FALSE, ok) ;
   bits := {} ;
   IF NOT ok
   THEN
      WriteString('something went wrong with the com1 initialization') ;
      WriteLn
   END
END SetBaud ;


VAR
   ch  : CHAR ;
   bit : CARDINAL ;
   bits: BITSET ;
   baud: CARDINAL ;
   ip  : PtrToProcDes ;
BEGIN
   baud := 9600 ;
   SetBaud(baud) ;
   InitProcess(ip, Input, 7, 1, 10000, 0, System, NIL, 'motor input') ;
   Resume(ip) ;
   WriteString('test the logic control board') ; WriteLn ;
   WriteString('type in numbers 0..7 to flip bits') ; WriteLn ;
   LOOP
      WriteString('bits: ') ; WriteBin(CARDINAL(bits), 8) ; WriteLn ;
      Write(VAL(CHAR, bits)) ;
      Read(ch) ;
      IF ch='-'
      THEN
         baud := baud DIV 2 ;
         SetBaud(baud)
      ELSIF ch='+'
      THEN
         baud := baud * 2 ;
         SetBaud(baud)
      ELSE
         bit := ORD(ch)-ORD('0') ;
         IF (bit>=0) AND (bit<=7)
         THEN
            IF bit IN bits
            THEN
               EXCL(bits, bit)
            ELSE
               INCL(bits, bit)
            END
         ELSE
            WriteString('use numbers 0..7') ; WriteLn
         END
      END
   END
END testctrl.
