IMPLEMENTATION MODULE SLoad ;


FROM crt0 IMPORT RunProgram ;
FROM libg IMPORT Read, Write ;
FROM SYSTEM IMPORT BYTE, ADDRESS, TurnInterrupts, OnOrOff ;
FROM SysStorage IMPORT ALLOCATE, DEALLOCATE ;
FROM NumberIO IMPORT CardToStr, WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT PushOutput ;
FROM Descriptors IMPORT AfterIDTGDT ;


CONST
   MaxHeader =     4 ;
   MaxFrame  =  1024 ;

VAR
   Header       : ARRAY [0..MaxHeader] OF CHAR ;
   Frame        : ARRAY [0..MaxFrame] OF CHAR ;
   LastSeq      : CARDINAL ;
   TotalChecksum: CARDINAL ;


(*
   Frame contents:

    $                         #
   -1     0     1..MaxFrame-3   1 2
   start  seqno Data          checksum  
*)


(*
   ResetHeader - 
*)

PROCEDURE ResetHeader ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 0 TO MaxHeader DO
      Header[i] := 0C
   END
END ResetHeader ;


(*
   FetchPacket - returns TRUE if no header is found.
                 It waits until a positive frame appears.
*)

PROCEDURE FetchPacket () : BOOLEAN ;
VAR
   ch     : CHAR ;
   i      : CARDINAL ;
   IsLegal: BOOLEAN ;
BEGIN
   REPEAT
      REPEAT
         ch := GetChar() ;
         IF HeaderFound()
         THEN
            RETURN( FALSE )
         END
      UNTIL ch='$' ;
      i := 0 ;
      REPEAT
         Frame[i] := GetChar() ;
         IF HeaderFound()
         THEN
            RETURN( FALSE )
         END ;
         INC(i)
      UNTIL (i>MaxFrame) OR (Frame[i-1]='#') ;
      IF (Frame[i-1]='#') AND (i<MaxFrame)
      THEN
         (* collect checksum *)
         Frame[i] := GetChar() ;
         IF HeaderFound()
         THEN
            RETURN( FALSE )
         END ;
         INC(i) ;
         Frame[i] := GetChar() ;
         IF HeaderFound()
         THEN
            RETURN( FALSE )
         END ;
         INC(i)
      END ;
      IF i<=MaxFrame
      THEN
         Frame[i] := '$'
      END ;
      IsLegal := LegalCheckSum() ;
      IF IsLegal AND CorrectSeqNo()
      THEN
         WritePositive
      ELSE
         IF NOT IsLegal
         THEN
            Write('C')
         ELSE
            WriteNegative
         END
      END
   UNTIL IsLegal AND CorrectSeqNo() ;
   LastSeq := 1-LastSeq ;
   RETURN( TRUE )
END FetchPacket ;


(*
   WritePositive - 
*)

PROCEDURE WritePositive ;
BEGIN
   Write(ToChar(1-LastSeq))
END WritePositive ;


(*
   WriteNegative - 
*)

PROCEDURE WriteNegative ;
BEGIN
   Write(ToChar(LastSeq))
END WriteNegative ;


(*
   CorrectSeqNo - returns, TRUE, if the seqno is in sequence.
*)

PROCEDURE CorrectSeqNo () : BOOLEAN ;
BEGIN
   RETURN( ToNum(Frame[0])=1-LastSeq )
END CorrectSeqNo ;


(*
   LegalCheckSum - 
*)

PROCEDURE LegalCheckSum () : BOOLEAN ;
VAR
   c, i: CARDINAL ;
BEGIN
   c := 0 ;
   i := 0 ;
   WHILE (i<=MaxFrame) AND (Frame[i]#'#') DO
      INC(c, VAL(CARDINAL, Frame[i])) ;
      INC(i)
   END ;
   IF (Frame[i]='#') AND (i<MaxFrame)
   THEN
      INC(i) ;
      IF i<MaxFrame
      THEN
         RETURN( (ToNum(Frame[i])=(c MOD 010H) MOD 010H) AND (ToNum(Frame[i+1])=(c DIV 010H) MOD 010H) )
      END
   END ;
   RETURN( FALSE )
END LegalCheckSum ;


(*
   AddToTotalChecksum - 
*)

PROCEDURE AddToTotalChecksum (b: BYTE) ;
BEGIN
   TotalChecksum := (TotalChecksum + VAL(CARDINAL, b)) MOD 0100000H ;
END AddToTotalChecksum ;


(*
   FetchByte - returns TRUE if a byte can be extracted from the frame.
*)

PROCEDURE FetchByte (i: CARDINAL; a: ADDRESS) : BOOLEAN ;
VAR
   j: CARDINAL ;
   p: POINTER TO BYTE ;
BEGIN
   j := 1 ;
   i := i*2 ;
   WHILE (j<MaxFrame) AND (Frame[j]#'#') DO
      IF (i=0) AND (Frame[j+1]#'#')
      THEN
         p := a ;
         p^ := VAL(BYTE, ToNum(Frame[j]) + ToNum(Frame[j+1]) * 010H) ;
         AddToTotalChecksum(p^) ;
         RETURN( TRUE )
      ELSE
         DEC(i) ;
         INC(j)
      END
   END ;
   RETURN( FALSE )
END FetchByte ;


(*
   ToNum - 
*)

PROCEDURE ToNum (ch: CHAR) : CARDINAL ;
BEGIN
   RETURN( ORD(ch-'A') MOD 010H )
END ToNum ;


(*
   ToChar - 
*)

PROCEDURE ToChar (c: CARDINAL) : CHAR ;
BEGIN
   RETURN( VAL(CHAR, c MOD 010H)+'A' )
END ToChar ;


(*
   LoadProgram - loads in a new realtime system and runs it.
*)

PROCEDURE LoadProgram ;
VAR
   a   : ADDRESS ;
   s   : CARDINAL ;
   ch  : CHAR ;
   back: OnOrOff ;
BEGIN
   LOOP
      ResetHeader ;
      back := TurnInterrupts(On) ;
      LastSeq := 0 ;
      TotalChecksum := 0 ;
      REPEAT
         ch := GetChar() ;
      UNTIL HeaderFound() ;
      IF CreateSpace(a, s)
      THEN
         IF LoadText(a, s)
         THEN
            back := TurnInterrupts(Off) ;
            PushOutput(Write) ;
            WriteLn ; WriteString('program length ') ; WriteCard(s, 6) ; WriteString(' bytes') ; WriteLn ;
            WriteString('go...') ;
            RunProgram(a, s, AfterIDTGDT)
         END ;
         DEALLOCATE(a, s)
      END
   END
END LoadProgram ;


(*
   GetChar - returns the current character received and it places it into the header buffer.
*)

PROCEDURE GetChar () : CHAR ;
VAR
   ch: CHAR ;
BEGIN
   Read(ch) ;
   Header[0] := Header[1] ;
   Header[1] := Header[2] ;
   Header[2] := Header[3] ;
   Header[3] := ch ;
   RETURN( ch )
END GetChar ;


(*
   HeaderFound - 
*)

PROCEDURE HeaderFound () : BOOLEAN ;
BEGIN
   RETURN( (Header[0]='a') AND (Header[1]='b') AND (Header[2]='c') AND (Header[3]='d') )
END HeaderFound ;


(*
   CreateSpace - loads in the size of the image and initializes space for this image.
*)

PROCEDURE CreateSpace (VAR a: ADDRESS; VAR s: CARDINAL) : BOOLEAN ;
BEGIN
   Write('+') ;  (* found header - continue *)
   IF FetchPacket()
   THEN
      s := ExtractSize() ;
      ALLOCATE(a, s) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END CreateSpace ;


(*
   ExtractSize - returns the size of the program to be downloaded.
*)

PROCEDURE ExtractSize () : CARDINAL ;
BEGIN
   RETURN( 
          ToNum(Frame[1]) * 0000001H +
          ToNum(Frame[2]) * 0000010H +
          ToNum(Frame[3]) * 0000100H +
          ToNum(Frame[4]) * 0001000H +
          ToNum(Frame[5]) * 0010000H +
          ToNum(Frame[6]) * 0100000H
         )
END ExtractSize ;


(*
   IsSameCrt0 - returns TRUE if the downloaded program has the same crt0 and
                Descriptor.S as we are currently using.
                This is in essence a version check
                we need to be sure that they are the same as we are about
                to perform an overlay.. after the segment descriptors.
*)

PROCEDURE IsSameCrt0 (a: ADDRESS) : BOOLEAN ;
VAR
   old, new: POINTER TO BYTE ;
   i       : CARDINAL ;
   p       : PROC ;
BEGIN
   p := AfterIDTGDT ;
   i := 0 ;
   old := ADDRESS(p) ;
   new := a + ADDRESS(p) ;
   WHILE i<8 DO
      IF old^=new^
      THEN
         INC(i)
      ELSE
         RETURN( FALSE )
      END
   END ;
   RETURN( TRUE )
END IsSameCrt0 ;


(*
   LoadText - 
*)

PROCEDURE LoadText (a: ADDRESS; s: CARDINAL) : BOOLEAN ;
VAR
   p   : POINTER TO BYTE ;
   i, j: CARDINAL ;
BEGIN
   i := 0 ;
   p := a ;
   WHILE i<s DO
      IF FetchPacket()
      THEN
         j := 0 ;
         WHILE FetchByte(j, p) DO
            INC(p) ;
            INC(j) ;
            INC(i)
         END
      ELSE
         RETURN( FALSE )
      END
   END ;
   i := 0 ;
   j := 0 ;
   p := a ;
   WHILE i<s DO
      j := (j + VAL(CARDINAL, p^)) MOD 0100000H ;
      INC(p) ;
      INC(i)
   END ;
   IF j=TotalChecksum
   THEN
      j := TotalChecksum ;
      IF FetchPacket()
      THEN
         IF ExtractSize()=j
         THEN
            IF IsSameCrt0(a)
            THEN
               Write('p') ;
               RETURN( TRUE )
            ELSE
               Write('v')
            END
         ELSE
            Write('f')
         END
      ELSE
         Write('t')
      END
   ELSE
      Write('X')
   END ;
   RETURN( FALSE )
END LoadText ;


END SLoad.
