IMPLEMENTATION MODULE InstrDB ;


FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM Storage IMPORT ALLOCATE ;
FROM NameKey IMPORT MakeKey, GetKey, WriteKey ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM StrLib IMPORT StrLen, StrCopy, StrEqual ;
FROM Args IMPORT GetArg ;
FROM libc IMPORT getenv ;
FROM ASCII IMPORT nul ;


CONST
   Any   = 'X' ;

TYPE
   PtrToEntry = POINTER TO Entry ;
   Entry      = RECORD
                   Next     : PtrToEntry ;
                   Match    : Name ;
                   OpcodeDef: Name ;
                   OpcodeLen: CARDINAL ;
                   NameTemp : Name ;
                END ;

VAR
   HeadOper,
   HeadInst: PtrToEntry ;
   Check,
   Debug   : BOOLEAN ;


(*
   PutInstruction - places an instruction in the database.
*)

PROCEDURE PutInstruction (OpcodeDefinition, NameTemplate: Name) ;
VAR
   a, b : ARRAY [0..MaxInstructionLength] OF CHAR ;
   m    : CARDINAL ;
   Inst : PtrToEntry ;
BEGIN
   GetKey(OpcodeDefinition, a) ;
   CreateMatchString(a, b) ;
   IF Debug
   THEN
      WriteString('opcode definition ') ; WriteString(a) ; WriteLn ;
      WriteString('match string      ') ; WriteString(b) ; WriteLn ; WriteLn
   END ;
   IF Check AND (FindInstr(HeadInst, b)#NIL)
   THEN
      WriteString('Duplicate instruction found') ; WriteLn ;
      WriteString('     ') ; WriteKey(NameTemplate) ;
      WriteString('   ') ; WriteKey(OpcodeDefinition) ; WriteLn ;
      WriteString('Clashes with: ') ;
      IF GetInstruction(b, OpcodeDefinition, NameTemplate)
      THEN
      END ;
      WriteKey(NameTemplate) ;
      WriteString('   ') ; WriteKey(OpcodeDefinition) ; WriteLn ;
      HALT
   ELSE
      NEW(Inst) ;
      WITH Inst^ DO
         Next := HeadInst ;
         Match := MakeKey(b) ;
         OpcodeDef := OpcodeDefinition ;
         NameTemp := NameTemplate ;
         (* OpcodeLen := CalculateLength(OpcodeDefinition) *)
      END ;
      HeadInst := Inst
   END
END PutInstruction ;


(*
   GetInstruction - attempts to find an instruction using the MatchString.
                    It returns TRUE if an instruction was found and
                    fills in OpcodeDefinition and NameTemplate.
*)

PROCEDURE GetInstruction (MatchString: ARRAY OF CHAR;
                          VAR OpcodeDefinition,
                              NameTemplate: Name) : BOOLEAN ;
VAR
   Inst: PtrToEntry ;
BEGIN
   Inst := FindInstr(HeadInst, MatchString) ;
   IF Inst=NIL
   THEN
      RETURN( FALSE )
   ELSE
      WITH Inst^ DO
         OpcodeDefinition := OpcodeDef ;
         NameTemplate := NameTemp
      END ;
      RETURN( TRUE )
   END
END GetInstruction ;


(*
   PutOperand - places an operand in the database.
*)

PROCEDURE PutOperand (OperandDefinition, NameTemplate: Name) ;
VAR
   a, b : ARRAY [0..MaxInstructionLength] OF CHAR ;
   m    : CARDINAL ;
   Inst : PtrToEntry ;
BEGIN
   GetKey(OperandDefinition, a) ;
   CreateMatchString(a, b) ;
   IF Debug
   THEN
      WriteString('operand definition ') ; WriteString(a) ; WriteLn ;
      WriteString('match string       ') ; WriteString(b) ; WriteLn ; WriteLn
   END ;
   IF FindInstr(HeadOper, b)#NIL
   THEN
      WriteString('Duplicate operand found') ; WriteLn ;
      WriteString('     ') ; WriteKey(NameTemplate) ; WriteLn ;
      HALT
   ELSE
      NEW(Inst) ;
      WITH Inst^ DO
         Next := HeadOper ;
         Match := MakeKey(b) ;
         OpcodeDef := OperandDefinition ;
         NameTemp := NameTemplate ;
         (* OpcodeLen := CalculateLength(OpcodeDefinition) *)
      END ;
      HeadOper := Inst
   END
END PutOperand ;


(*
   GetOperand - attempts to find an instruction using the MatchString.
                It returns TRUE if an instruction was found and
                fills in OpcodeDefinition and NameTemplate.
*)

PROCEDURE GetOperand (MatchString: ARRAY OF CHAR;
                      VAR OperandDefinition,
                          NameTemplate: Name) : BOOLEAN ;
VAR
   Inst: PtrToEntry ;
BEGIN
   Inst := FindInstr(HeadOper, MatchString) ;
   IF Inst=NIL
   THEN
      RETURN( FALSE )
   ELSE
      WITH Inst^ DO
         OperandDefinition := OpcodeDef ;
         NameTemplate := NameTemp
      END ;
      RETURN( TRUE )
   END
END GetOperand ;


(*
   FindInstr - returns a pointer to an entry if a match was found.
*)

PROCEDURE FindInstr (Head: PtrToEntry;
                     MatchString: ARRAY OF CHAR) : PtrToEntry ;
VAR
   Inst: PtrToEntry ;
   a   : ARRAY [0..MaxInstructionLength] OF CHAR ;
BEGIN
   Inst := Head ;
   WHILE Inst#NIL DO
      GetKey(Inst^.Match, a) ;
      IF IsMatch(a, MatchString)
      THEN
         RETURN( Inst )
      ELSE
         Inst := Inst^.Next
      END
   END ;
   RETURN( NIL )
END FindInstr ;


(*
   MakeMatchString - creates a matchstring using the OpcodeDefinition.
*)

PROCEDURE MakeMatchString (OpcodeDefinition: Name) : Name ;
VAR
   Match,
   OpDefn: ARRAY [0..MaxInstructionLength] OF CHAR ;
BEGIN
   GetKey(OpcodeDefinition, OpDefn) ;
   CreateMatchString(OpDefn, Match) ;
   RETURN( MakeKey(Match) )
END MakeMatchString ;


(*
   CreateMatchString - creates a match string from OpcodeDefinition.
*)

PROCEDURE CreateMatchString (OpcodeDefinition: ARRAY OF CHAR;
                             VAR Match: ARRAY OF CHAR) ;
VAR
   i, j,
   Hm, Ho: CARDINAL ;
BEGIN
   Hm := HIGH(Match) ;
   Ho := StrLen(OpcodeDefinition) ;
   i := 0 ;
   j := 0 ;
   WHILE (i<Ho) AND (j<Hm) AND (OpcodeDefinition[i]#nul) DO
      IF OpcodeDefinition[i]='<'
      THEN
         INC(i) ;
         CreateByte(OpcodeDefinition, Match, Hm, Ho, i, j)
      ELSE
         INC(i)
      END
   END ;
   IF j<Hm
   THEN
      Match[j] := nul
   END
END CreateMatchString ;


(*
   CreateByte - assigns a match byte to array b.
*)

PROCEDURE CreateByte (a: ARRAY OF CHAR;
                      VAR b: ARRAY OF CHAR; Ha, Hb: CARDINAL;
                      VAR i, j: CARDINAL) ;
VAR
   NameBits,
   NoOfBits: CARDINAL ;
BEGIN
   NoOfBits := 0 ;
   WHILE (i<Ha) AND (j<Hb) AND (a[i]#nul) AND (a[i]#'>') DO
      IF IsBinary(a[i])
      THEN
         b[j] := a[i] ;
         INC(NoOfBits) ;
         INC(i) ;
         INC(j)
      ELSIF a[i]=':'
      THEN
         INC(i) ;
         IF IsDigit(a[i])
         THEN
            NameBits := ORD(a[i])-ORD('0') ;
            INC(i) ;   (* move over digit *)
            INC(NoOfBits, NameBits) ;
            WHILE (NameBits>0) AND (j<Hb) DO
               b[j] := Any ;
               INC(j) ;
               DEC(NameBits)
            END
         ELSE
            WriteString('expecting digit after : in OpcodeDefinition ') ;
            WriteString(a) ; WriteLn
         END
      ELSIF a[i]=Any
      THEN
         b[j] := Any ;
         INC(NoOfBits) ;
         INC(j) ;
         INC(i)
      ELSE
         INC(i)   (* must be a symbolic name *)
      END
   END ;
   IF (i<Ha) AND (a[i]#'>')
   THEN
      WriteString('expecting > at end of byte definition of: ') ;
      WriteString(a) ; WriteLn
   END ;
   IF j<Hb
   THEN
      b[j] := nul
   END ;
   IF NoOfBits#8
   THEN
      WriteString('byte definition does not have 8 bits,') ;
      WriteCard(NoOfBits, 2) ; WriteString(' found: ') ;
      WriteString(b) ; WriteLn
   END
END CreateByte ;


(*
   IsMatch - returns TRUE if the strings a and b match.
*)

PROCEDURE IsMatch (a, b: ARRAY OF CHAR) : BOOLEAN ;
VAR
   i, high: CARDINAL ;
BEGIN
   IF Debug
   THEN
      WriteString('is there a match between') ; WriteLn ;
      WriteString(a) ; WriteLn ;
      WriteString(b) ; WriteLn
   END ;
   high := StrLen(a) ;
   IF high=StrLen(b)
   THEN
      i := 0 ;
      WHILE (i<high) AND (a[i]#nul) AND (b[i]#nul) DO
         IF IsEqualMatch(a[i], b[i])
         THEN
            INC(i)
         ELSE
            IF Debug
            THEN
               WriteString('no') ; WriteLn
            END ;
            RETURN( FALSE )
         END
      END ;
      IF Debug
      THEN
         WriteString('yes') ; WriteLn
      END ;
      RETURN( TRUE )
   ELSE
      IF Debug
      THEN
         WriteString('no') ; WriteLn
      END ;
      RETURN( FALSE )
   END
END IsMatch ;


(*
   IsEqualMatch - returns TRUE if the characters a and b are the
                  same or if either is an Any character.
*)

PROCEDURE IsEqualMatch (a, b: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (a=b) OR (a=Any) OR (b=Any) )
END IsEqualMatch ;


(*
   IsBinary - returns true if ch is a binary digit.
*)

PROCEDURE IsBinary (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch >= '0') AND (ch <= '1') )
END IsBinary ;


(*
   IsDigit - returns true if ch is a digit.
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch >= '0') AND (ch <= '9') )
END IsDigit ;


(*
   ParseArguments - parses arguments relevant to this module.
*)

PROCEDURE ParseArguments ;
VAR
   i: CARDINAL ;
   a: ARRAY [0..80] OF CHAR ;
BEGIN
   i := 0 ;
   WHILE GetArg(a, i) DO
      IF StrEqual(a, '-check')
      THEN
         Check := TRUE
      END ;
      INC(i)
   END
END ParseArguments ;


(*
   Init - initialize the instruction list.
*)

PROCEDURE Init ;
VAR
   Env: ARRAY [0..10] OF CHAR ;
BEGIN
   HeadInst := NIL ;
   HeadOper := NIL ;
   StrCopy('M2DEBUG', Env) ;
   Debug := getenv(ADR(Env))#NIL ;
   Check := FALSE ;
   ParseArguments
END Init ;


BEGIN
   Init
END InstrDB.
