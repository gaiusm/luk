IMPLEMENTATION MODULE Translate;


FROM SYSTEM IMPORT ADR ;
FROM Args IMPORT GetArg ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM NumberIO IMPORT WriteHex, BinToStr, StrToCard, WriteCard, StrToBin ;
FROM StrLib IMPORT StrLen, StrCopy, StrConCat, StrEqual, IsSubString ;
FROM InstrDB IMPORT MaxInstructionBytes, MaxInstructionLength,
                    PutInstruction, GetInstruction, MakeMatchString,
                    PutOperand, GetOperand ;
FROM AOUT IMPORT IsProcessor16Bit ;
FROM VarDB IMPORT InitVarDB, GetValue, PutValue, IsValue ;
FROM NameKey IMPORT Name, WriteKey, GetKey, MakeKey, NulName ;
FROM ASCII IMPORT nul ;


CONST
   MaxStack        = 20 ;
   Quote           = '"' ;
   Begin           = '<' ;
   End             = '>' ;
   BitField        = ':' ;
   MaxBytesPerLine = 4 ;
   DefaultDbit     = 0 ;

VAR
   InstChar           : ARRAY [0..MaxInstructionLength] OF CHAR ;
   InstByte           : ARRAY [0..MaxInstructionBytes+MaxBytesPerLine]
                        OF BYTE ;
   NoOfDataBytes,
   NoOfBytes          : CARDINAL ;
   NoOfMemoryOperands,
   MemoryOperandOffset: CARDINAL ;
   IndirectGetByte    : ProcGet ;
   Stack              : ARRAY [1..MaxStack] OF BYTE ;
   StackPtr           : CARDINAL ;
   Dbit               : CARDINAL ;
   Debug              : BOOLEAN ;


(*
   GetNBytes - reads n bytes if available and returns the number read.
*)

PROCEDURE GetNBytes (n: CARDINAL) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE (i<n) AND LocalGetByte(InstByte[i]) DO
      INC(i)
   END ;
   RETURN( i )
END GetNBytes ;


(*
   TranslateInstruction - translates the sequence of bytes into an
                          80386 instruction. It returns the number of
                          bytes the instruction uses.
*)

PROCEDURE TranslateInstruction (IgnoreInstructions: BOOLEAN;
                                CurrentAddress: CARDINAL;
                                GetByte: ProcGet) : CARDINAL ;
VAR
   Found            : BOOLEAN ;
   b                : BYTE ;
   OperandDefinition: ARRAY [0..MaxInstructionBytes] OF CHAR ;
   OpcodeDefinition ,
   OperandName      ,
   OpcodeName       : Name ;
BEGIN
   InitVarDB ;
   IndirectGetByte := GetByte ;
   NoOfBytes := 0 ;
   NoOfDataBytes := 0 ;       (* No data bytes found yet *)
   NoOfMemoryOperands := 0 ;  (* no memory operands yet  *)
   MemoryOperandOffset := 0 ;
   IF IgnoreInstructions
   THEN
      NoOfBytes := GetNBytes(MaxBytesPerLine) ;
      DisplayBytes(CurrentAddress, 0, NoOfBytes)
   ELSE
      REPEAT
         Found := FindInstruction(OpcodeDefinition, OpcodeName) ;
         IF NOT Found
         THEN
            PushBackUpTo(NoOfDataBytes) ;
            IF LocalGetByte(b)
            THEN
            END ;
            INC(NoOfDataBytes)
         END
      UNTIL Found OR (NoOfDataBytes=MaxBytesPerLine) ;
      IF Found
      THEN
         (* firstly display any data bytes found before instruction *)
         DisplayBytes(CurrentAddress, 0, NoOfDataBytes) ;
         InitVarDB ;
         (* now display instruction bytes and name *)
         ParseOpcodeDefinition(OpcodeDefinition) ;
         BuildOperand(OperandDefinition, OperandName) ;
         ReadAppropriateNumberOfBytes(OpcodeDefinition,
                                      OperandName, OpcodeName) ;
         DisplayFirstBytes(CurrentAddress, NoOfDataBytes,
                           Min(NoOfDataBytes+MaxBytesPerLine, NoOfBytes)) ;
         WriteSpaces(MaxInstructionBytes-
                     Min(NoOfBytes-NoOfDataBytes, MaxBytesPerLine)) ;
         NoOfMemoryOperands := 0 ;  (* reset count for next pass  *)
         MemoryOperandOffset := NoOfBytesInOpcode(OpcodeDefinition) +
                                NoOfDataBytes ;
         GenerateName(CurrentAddress, OpcodeName, OperandName) ;
         WriteLn ;
         INC(CurrentAddress, Min(MaxBytesPerLine, NoOfBytes-NoOfDataBytes)) ;
         (*
            now display any remaining bytes that are used in the instruction.
            This occurs if the instruction format uses more than
            MaxBytesPerLine bytes.
         *)
         DisplayBytes(CurrentAddress, MaxBytesPerLine, NoOfBytes)
      ELSE
         DisplayBytes(CurrentAddress, 0, Min(NoOfDataBytes, MaxBytesPerLine)) ;
         IF NoOfDataBytes>0
         THEN
            PushBackUpTo(NoOfDataBytes)
         END
      END
   END ;
   RETURN( NoOfBytes )
END TranslateInstruction ;


(*
   OperandKey - returns the current operand key.
*)

PROCEDURE OperandKey () : Name ;
VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   a[0] := '%' ;
   a[1] := CHR(ORD('0')+NoOfMemoryOperands) ;
   RETURN( MakeKey(a) )
END OperandKey ;


(*
   BuildOperand - builds an operand name definition and looks this name
                  up in the operand database.
*)

PROCEDURE BuildOperand (VAR OperandDefinition: ARRAY OF CHAR;
                        VAR OperandName: Name) ;
VAR
   o    : Name ;
   Start: CARDINAL ;
BEGIN
   Start := 0 ;
   AddBits(OperandDefinition, AddressBitSize(), Start, 1) ;
   Start := 1 ;
   AddBits(OperandDefinition, DataBitSize(), Start, 1) ;
   Start := 2 ;
   IF IsValue(MakeKey('mod'))
   THEN
      AddBits(OperandDefinition, GetValue(MakeKey('mod')), Start, 2) ;
      INC(Start, 2)
   END ;
   IF IsValue(MakeKey('r/m'))
   THEN
      AddBits(OperandDefinition, GetValue(MakeKey('r/m')), Start, 3) ;
      INC(Start, 3)
   END ;
   IF IsValue(MakeKey('sreg3')) OR IsValue(MakeKey('sreg2'))
   THEN
      (*
         if we are using the segment registers then the addressing
         mode is the same as the w bit = 1    ie 16 or 32 bits
      *)
      AddBits(OperandDefinition, 1, Start, 1) ;
      INC(Start, 1)
   ELSIF IsValue(MakeKey('w'))
   THEN
      AddBits(OperandDefinition, GetValue(MakeKey('w')), Start, 1) ;
      INC(Start, 1)
   ELSE
      (* no w bit therefore place zero here *)
      AddBits(OperandDefinition, GetValue(MakeKey('w')), Start, 1) ;
      INC(Start, 1)
   END ;
   IF NOT GetOperand(OperandDefinition, o, OperandName)
   THEN
      OperandName := NulName
      ; Dummy
   END
END BuildOperand ;


(*
   Dummy - 
*)

PROCEDURE Dummy ;
BEGIN
   
END Dummy ;


(*
   AddBits - adds n bits encoding value, v, starting at indice, i, to
             string, a.
*)

PROCEDURE AddBits (VAR a: ARRAY OF CHAR; v: CARDINAL; i, n: CARDINAL) ;
VAR
   vi, vh,
   High  : CARDINAL ;
   StrOfV: ARRAY [0..64] OF CHAR ;
BEGIN
   High := HIGH(a) ;
   BinToStr(v, 0, StrOfV) ;
   (* pad with zeros *)
   vh := StrLen(StrOfV) ;
   vi := 0 ;
   WHILE (vh<n) AND (i<=High) DO
      a[i] := '0' ;
      INC(i) ;
      INC(vh)
   END ;
   (* now the rest of the number *)
   vh := StrLen(StrOfV) ;
   vi := 0 ;
   WHILE (vi<vh) AND (i<=High) AND (n>0) DO
      a[i] := StrOfV[vi] ;
      INC(i) ;
      INC(vi) ;
      DEC(n)
   END ;
   IF i<=High
   THEN
      a[i] := nul
   END
END AddBits ;


(*
   ReadAppropriateNumberOfBytes - continues to read the appropriate number of
                                  bytes to satisfy the opcode definition.
*)

PROCEDURE ReadAppropriateNumberOfBytes (OpDef: Name;
                                        OperandName, OpcodeName: Name) ;
VAR
   n: CARDINAL ;
   b: BYTE ;
BEGIN
   n := OperandMemorySize(OperandName, OpcodeName)+NoOfBytesInOpcode(OpDef) ;
   WHILE (NoOfBytes-NoOfDataBytes<n) AND LocalGetByte(b) DO
   END
END ReadAppropriateNumberOfBytes ;


(*
   OperandSize - returns the operand size in bytes.
*)

PROCEDURE OperandSize () : CARDINAL ;
VAR
   n: CARDINAL ;
BEGIN
   IF IsValue(MakeKey('w'))
   THEN
      n := GetValue(MakeKey('w')) ;
      IF n=1
      THEN
         IF DataBitSize()=0
         THEN
            n := 2 DIV (GetValue(MakeKey('s'))+1)
         ELSE
            n := 4 DIV (GetValue(MakeKey('s'))+1)
         END
      ELSE
         n := 1
      END
   ELSE
      IF IsValue(MakeKey('OPERANDSIZE'))
      THEN
         IF DataBitSize()=0
         THEN
            n := 2 DIV (GetValue(MakeKey('s'))+1)
         ELSE
            n := 4 DIV (GetValue(MakeKey('s'))+1)
         END
      ELSE
         n := 1
      END
   END ;
   RETURN( n )
END OperandSize ;


(*
   OperandMemorySize - returns the number of bytes in memory which
                       contain the operand.
*)

PROCEDURE OperandMemorySize (OperandName, OpcodeName: Name) : CARDINAL ;
VAR
   Op1, Op2   : CARDINAL ;
   NameOperand,
   NameOpcode : ARRAY [0..MaxInstructionLength] OF CHAR ;
BEGIN
   GetKey(OperandName, NameOperand) ;
   GetKey(OpcodeName, NameOpcode) ;
   Op1 := DetermineSize(NameOpcode) ;
   IF Op1>0
   THEN
      INC(NoOfMemoryOperands) ;
      PutValue(OperandKey(), Op1)
   END ;
   Op2 := DetermineSize(NameOperand) ;
   IF Op2>0
   THEN
      INC(NoOfMemoryOperands) ;
      PutValue(OperandKey(), Op2)
   END ;
   RETURN( Op1 + Op2 )
END OperandMemorySize ;


(*
   DetermineSize - returns the size of memory references contained in, a.
*)

PROCEDURE DetermineSize (a: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   IF IsSubString(a, '"d8"')
   THEN
      RETURN( 1 )
   ELSIF IsSubString(a, '"d16"')
   THEN
      RETURN( 2 )
   ELSIF IsSubString(a, '"d32"')
   THEN
      RETURN( 4 )
   ELSIF IsSubString(a, 'disp8')
   THEN
      RETURN( 1 )
   ELSIF IsSubString(a, 'disp16/32')
   THEN
      IF AddressBitSize()=0
      THEN
         RETURN( 2 )
      ELSE
         RETURN( 4 )
      END
   ELSIF IsSubString(a, 'seg/offset')
   THEN
      RETURN( 4 )
   ELSIF IsSubString(a, '"imm"')
   THEN
      RETURN( OperandSize() )
   ELSIF IsSubString(a, '"imm8"')
   THEN
      RETURN( 1 )
   ELSIF IsSubString(a, '"imm16/32"')
   THEN
      IF DataBitSize()=0
      THEN
         RETURN( 2 )
      ELSE
         RETURN( 4 )
      END
   END ;
   RETURN( 0 )
END DetermineSize ;


(*
   NoOfBytesInOpcode - returns the number of bytes contained within the opcode
                       definition, OpDef.
*)

PROCEDURE NoOfBytesInOpcode (OpDef: Name) : CARDINAL ;
VAR
   OpcodeDefinition: ARRAY [0..MaxInstructionLength] OF CHAR ;
   i, n            : CARDINAL ;
   Bytes           : CARDINAL ;
BEGIN
   GetKey(OpDef, OpcodeDefinition) ;
   i := 0 ;
   Bytes := 0 ;
   n := StrLen(OpcodeDefinition) ;
   WHILE i<n DO
      IF OpcodeDefinition[i]=Begin
      THEN
         INC(Bytes)
      END ;
      INC(i)
   END ;
   RETURN( Bytes )
END NoOfBytesInOpcode ;


(*
   PushBackUpTo - called when an instruction has not been found
                  and we have dumped the first MaxBytesPerLine
                  numbers as data. The rest of the bytes may
                  form a future instruction - thus we push
                  them back onto the input stream ready for
                  the next time we are called.
*)

PROCEDURE PushBackUpTo (n: CARDINAL) ;
BEGIN
   WHILE n<NoOfBytes DO
      LocalPutByte(InstByte[NoOfBytes-1])
   END
END PushBackUpTo ;
         
         
(*
   GenerateName - displays the instruction name and interprets the
                  variables.
*)

PROCEDURE GenerateName (CurrentAddress: CARDINAL;
                        OpcodeName, OperandName: Name) ;
VAR
   Name: ARRAY [0..MaxInstructionLength] OF CHAR ;
   High,
   i   : CARDINAL ;
BEGIN
   GetKey(OpcodeName, Name) ;
   i := 0 ;
   High := StrLen(Name) ;
   WHILE i<High DO
      IF Name[i]=Quote
      THEN
         GenerateVariable(CurrentAddress, Name, i, High, OperandName) ;
         IF Name[i]#Quote
         THEN
            WriteString('error in name template - missing quote: ') ;
            WriteString(Name) ; WriteLn ;
            WriteString('at position') ; WriteCard(i, 4) ; WriteLn
         END
      ELSIF Name[i]='\'
      THEN
         INC(i) ;
         IF Name[i]='n'
         THEN
            INC(i) ;
            WriteLn
         END
      ELSE
         Write(Name[i])
      END ;
      INC(i)
   END
END GenerateName ;


(*
   GenerateVariable - extracts the variable from string, a, and
                      calls the interpret variable procedure.
*)

PROCEDURE GenerateVariable (CurrentAddress: CARDINAL;
                            a: ARRAY OF CHAR;
                            VAR i: CARDINAL; High: CARDINAL;
                            OperandName: Name) ;
VAR
   j: CARDINAL ;
   v: ARRAY [0..MaxInstructionLength] OF CHAR ;
BEGIN
   j := 0 ;
   INC(i) ;
   WHILE (i+j<High) AND (j<HIGH(v)) AND (a[i+j]#Quote) DO
      v[j] := a[i+j] ;
      INC(j)
   END ;
   IF j<HIGH(v)
   THEN
      v[j] := nul
   END ;
   INC(i, j) ;
   InterpretVariable(CurrentAddress, v, OperandName)
END GenerateVariable ;


(*
   InterpretVariable - interprets the variable, a.
*)

PROCEDURE InterpretVariable (CurrentAddress: CARDINAL;
                             a: ARRAY OF CHAR; Operand: Name) ;
BEGIN
   IF StrEqual(a, 'reg')
   THEN
      ProcessReg(a)
   ELSIF StrEqual(a, 'sreg3')
   THEN
      ProcessSReg3(a)
   ELSIF StrEqual(a, 'sreg2')
   THEN
      ProcessSReg2(a)
   ELSIF StrEqual(a, 'eax')
   THEN
      WriteReg(0, GetValue(MakeKey('w')), DataBitSize())
   ELSIF StrEqual(a, 'jecx')
   THEN
      Write('j') ; WriteReg(1, 0, AddressBitSize())
   ELSIF StrEqual(a, 'imm')
   THEN
      ProcessImmediate(OperandSize())
   ELSIF StrEqual(a, 'imm8')
   THEN
      ProcessImmediate(1)
   ELSIF StrEqual(a, 'imm16/32')
   THEN
      IF DataBitSize()=0
      THEN
         ProcessImmediate(2)
      ELSE
         ProcessImmediate(4)
      END
   ELSIF StrEqual(a, 'w')
   THEN
      ProcessW
   ELSIF StrEqual(a, 'd8')
   THEN
      ProcessMemoryBytes(1)
   ELSIF StrEqual(a, 'd16')
   THEN
      ProcessMemoryBytes(2)
   ELSIF StrEqual(a, 'd32')
   THEN
      ProcessMemoryBytes(4)
   ELSIF StrEqual(a, 'disp8')
   THEN
      ProcessDisplacement(CurrentAddress)
   ELSIF StrEqual(a, 'disp16/32')
   THEN
      ProcessDisplacement(CurrentAddress)
   ELSIF StrEqual(a, 'seg/offset')
   THEN
      ProcessSegOffset
   ELSIF StrEqual(a, '%1')
   THEN
      WriteString('%1 is out of date check instruction/opcode definitions')
   ELSIF StrEqual(a, '%')
   THEN
      IF Operand=NulName
      THEN
         WriteString('????')
      ELSE
         GenerateName(CurrentAddress, Operand, NulName)
      END
   END
END InterpretVariable ;


(*
   ProcessDisplacement - writes the displacement contained
                         in, n, bytes.
*)

PROCEDURE ProcessDisplacement (CurrentAddress: CARDINAL) ;
VAR
   n, i, j, d: CARDINAL ;
BEGIN
   INC(NoOfMemoryOperands) ;
   IF IsValue(OperandKey())
   THEN
      n := GetValue(OperandKey()) ;
      i := MemoryOperandOffset ;
      j := i+n-1 ;
      INC(MemoryOperandOffset, n) ;
      d := 0 ;
      WHILE j>=i DO
         d := d * 0100H + VAL(CARDINAL, InstByte[j]) ;
         WriteHex(VAL(CARDINAL, InstByte[j]), 2) ;
         DEC(j)
      END ;
      WriteString('  (') ;
      IF (n=1) AND (d>80H)
      THEN
         WriteHex(CurrentAddress+d-0100H+NoOfBytes-NoOfDataBytes, 8)
      ELSIF (n=2) AND (d>=8000H)
      THEN
         WriteHex(CurrentAddress+d-010000H+NoOfBytes-NoOfDataBytes, 8)
      ELSE
         WriteHex(CurrentAddress+d+NoOfBytes-NoOfDataBytes, 8)
      END ;
      Write(')')
   ELSE
      WriteString('Memory operand is unavailable "') ;
      WriteKey(OperandKey()) ; Write('"') ; WriteLn ; HALT
   END
END ProcessDisplacement ;


(*
   ProcessSegOffset - writes the segment offset address
*)

PROCEDURE ProcessSegOffset ;
VAR
   i, v,
   s, o: CARDINAL ;
BEGIN
   INC(NoOfMemoryOperands) ;
   IF IsValue(OperandKey())
   THEN
      v := GetValue(OperandKey()) ;
      i := MemoryOperandOffset ;
      IF v#4
      THEN
         WriteString('program error segment/offset expected 4 bytes')
      END ;
      WriteHex(VAL(CARDINAL, InstByte[i+3]), 2) ;
      WriteHex(VAL(CARDINAL, InstByte[i+2]), 2) ;
      Write(':') ;
      WriteHex(VAL(CARDINAL, InstByte[i+1]), 2) ;
      WriteHex(VAL(CARDINAL, InstByte[i+0]), 2) ;
      s := VAL(CARDINAL, InstByte[i+2]) +
           VAL(CARDINAL, InstByte[i+3]) * 0100H ;
      o := VAL(CARDINAL, InstByte[i+0]) +
           VAL(CARDINAL, InstByte[i+1]) * 0100H ;
      WriteString('  (') ;
      WriteHex(s*010H+o, 8) ;
      Write(')')
   ELSE
      WriteString('Memory operand is unavailable "') ;
      WriteKey(OperandKey()) ; Write('"') ; WriteLn ; HALT
   END
END ProcessSegOffset ;


(*
   ProcessW - generates a "b" or "w or "l" character for the
              quantity of data being operated upon.
*)

PROCEDURE ProcessW ;
BEGIN
   IF DataBitSize()=0
   THEN
      IF GetValue(MakeKey('w'))=0
      THEN
         Write('b')
      ELSE
         Write('w')
      END
   ELSE
      IF GetValue(MakeKey('w'))=0
      THEN
         Write('b')
      ELSE
         Write('l')
      END
   END
END ProcessW ;


(*
   DataBitSize - returns 1 for a data bit size of 32 bits and
                 0 for a data bit size of 16 bits.
*)

PROCEDURE DataBitSize () : CARDINAL ;
BEGIN
   IF IsProcessor16Bit()
   THEN
      RETURN( Dbit-GetValue(MakeKey('OPERANDSIZE')) )
   ELSE
      RETURN( 1-(Dbit-GetValue(MakeKey('OPERANDSIZE'))) )
   END
END DataBitSize ;


(*
   AddressBitSize - returns 1 for an address bit size of 32 bits and
                    0 for an address bit size of 16 bits.
*)

PROCEDURE AddressBitSize () : CARDINAL ;
BEGIN
   IF IsProcessor16Bit()
   THEN
      RETURN( Dbit-GetValue(MakeKey('ADDRESSSIZE')) )
   ELSE
      RETURN( 1-(Dbit-GetValue(MakeKey('ADDRESSSIZE'))) )
   END
END AddressBitSize ;


(*
   ProcessMemoryBytes - displays, n, memory bytes for the current operand.
*)

PROCEDURE ProcessMemoryBytes (n: CARDINAL) ;
VAR
   v, i, j: CARDINAL ;
BEGIN
   INC(NoOfMemoryOperands) ;
   IF IsValue(OperandKey())
   THEN
      v := GetValue(OperandKey()) ;
      i := MemoryOperandOffset ;
      j := i+v-1 ;
      IF v#n
      THEN
         WriteString('no of bytes in operand is inconsistant - program error') ; WriteCard(v, 4) ; WriteCard(n, 4) ; WriteLn ;
      END ;
      INC(MemoryOperandOffset, n) ;
      WHILE j>=i DO
         WriteHex(VAL(CARDINAL, InstByte[j]), 2) ;
         DEC(j)
      END
   ELSE
      WriteString('Memory operand is unavailable "') ;
      WriteKey(OperandKey()) ; Write('"') ; WriteLn ; HALT
   END
END ProcessMemoryBytes ;


(*
   ProcessImmediate - displays, n, memory bytes for the current operand.
*)

PROCEDURE ProcessImmediate (n: CARDINAL) ;
VAR
   v, i, j: CARDINAL ;
BEGIN
   INC(NoOfMemoryOperands) ;
   IF IsValue(OperandKey())
   THEN
      v := GetValue(OperandKey()) ;
      i := MemoryOperandOffset ;
      j := i+v-1 ;
      IF v#n
      THEN
         WriteString('no of bytes in operand is inconsistant - program error') ; WriteCard(v, 4) ; WriteCard(n, 4) ; WriteLn ;
      END ;
      INC(MemoryOperandOffset, n) ;
      IF (VAL(CARDINAL, InstByte[j])>07FH) AND (GetValue(MakeKey('s'))=1)
      THEN
         (* sign extend *)
         InstByte[j] := VAL(BYTE, VAL(CARDINAL, InstByte[j])-080H) ;
         WriteString('80') ;
         IF n=2
         THEN
            WriteString('0000')
         END
      END ;
      WHILE j>=i DO
         WriteHex(VAL(CARDINAL, InstByte[j]), 2) ;
         DEC(j)
      END
   ELSE
      WriteString('Memory operand is unavailable "') ;
      WriteKey(OperandKey()) ; Write('"') ; WriteLn ; HALT
   END
END ProcessImmediate ;


(*
   ProcessImm - displays, n, memory bytes.
                Used when there is no "mod" "r/m" variables in operand.
*)

PROCEDURE ProcessImm (OpcodeDef: Name) ;
VAR
   n, i: CARDINAL ;
BEGIN
   i := NoOfBytesInOpcode(OpcodeDef) ;
   n := i+OperandSize() ;
   WHILE n>=i DO
      WriteHex(VAL(CARDINAL, InstByte[n]), 2) ;
      DEC(n)
   END
END ProcessImm ;


(*
   WriteReg - writes a register with its size dependant upon the
             "w" variable and the "OPERANDSIZE" flag.
*)

PROCEDURE WriteReg (reg: CARDINAL; w: CARDINAL;
                    DataBits32: CARDINAL) ;
BEGIN
   IF IsValue(MakeKey('w'))
   THEN
      (* w variable really exists in this instruction *)
      IF DataBits32=0
      THEN
         (* 16 bit data *)
         IF w=0
         THEN
            WriteSubString('alcldlblahchdhbh', reg*2, 2)
         ELSE
            WriteSubString('axcxdxbxspbpsidi', reg*2, 2)
         END
      ELSE
         (* 32 bit data *)
         IF w=0
         THEN
            WriteSubString('alcldlblahchdhbh', reg*2, 2)
         ELSE
            WriteSubString('eaxecxedxebxespebpesiedi', reg*3, 3)
         END
      END
   ELSE
      (* w variable does not exist in this instruction *)
      IF DataBits32=0
      THEN
         (* 16 bit data *)
         WriteSubString('axcxdxbxspbpsidi', reg*2, 2)
      ELSE
         (* 32 bit data *)
         WriteSubString('eaxecxedxebxespebpesiedi', reg*3, 3)
      END
   END
END WriteReg ;


(*
   ProcessReg - displays the correct register name.
*)

PROCEDURE ProcessReg (a: ARRAY OF CHAR) ;
VAR
   w, op32: CARDINAL ;
BEGIN
   IF IsValue(MakeKey(a))
   THEN
      IF IsValue(MakeKey('w'))
      THEN
         w := GetValue(MakeKey('w')) ;
      ELSE
         w := 0
      END ;
      op32 := DataBitSize() ;
      WriteReg(GetValue(MakeKey(a)), w, op32)
   ELSE
      WriteString(' register name reg found but no value found ')
   END
END ProcessReg ;
   

(*
   ProcessSReg3 - displays the correct register name.
*)

PROCEDURE ProcessSReg3 (a: ARRAY OF CHAR) ;
BEGIN
   IF IsValue(MakeKey(a))
   THEN
      WriteSubString('escsssdsfsgs????', GetValue(MakeKey(a))*2, 2)
   ELSE
      WriteString('register name sreg3 found but no value found') ; WriteLn
   END
END ProcessSReg3 ;


(*
   ProcessSReg2 - displays the correct register name.
*)

PROCEDURE ProcessSReg2 (a: ARRAY OF CHAR) ;
BEGIN
   IF IsValue(MakeKey(a))
   THEN
      WriteSubString('escsssds', GetValue(MakeKey(a))*2, 2)
   ELSE
      WriteString('register name sreg2 found but no value found') ; WriteLn
   END
END ProcessSReg2 ;


(*
   CheckPreInstruction - checks to see whether the first 2 bytes
                         are operand size overrides.
*)

PROCEDURE CheckPreInstruction ;
VAR
   b    : BYTE ;
   Found: BOOLEAN ;
BEGIN
   Found := TRUE ;
   WHILE Found AND LocalGetByte(b) DO
      CASE b OF

      066H:  PutValue(MakeKey('OPERANDSIZE'), 1) |
      067H:  PutValue(MakeKey('ADDRESSSIZE'), 1)

      ELSE
         LocalPutByte(b) ;
         Found := FALSE
      END
   END
END CheckPreInstruction ;
   

(*
   ParseOpcodeDefinition - parse the opcode definition and foreach
                           variable found record the value of the
                           bits in the InstChar.
*)

PROCEDURE ParseOpcodeDefinition (OpDef: Name) ;
VAR
   OpcodeDefinition: ARRAY [0..MaxInstructionLength] OF CHAR ;
   OpI, InstI      : CARDINAL ;
   Bits            : CARDINAL ;
BEGIN
   GetKey(OpDef, OpcodeDefinition) ;
   Bits := 0 ;
   InstI := 0 ;
   OpI := 0 ;
   WHILE (OpI<HIGH(OpcodeDefinition)) AND (OpcodeDefinition[OpI]#nul) DO
      IF IsDigit(OpcodeDefinition[OpI])
      THEN
         ParseDigit(OpcodeDefinition, OpI, InstI) ;
         INC(Bits)
      ELSIF OpcodeDefinition[OpI]=Quote
      THEN
         INC(Bits, ParseVariable(OpcodeDefinition, OpI, InstI))
      ELSIF OpcodeDefinition[OpI]=Begin
      THEN
         Bits := 0 ;
         INC(OpI)
      ELSIF OpcodeDefinition[OpI]=End
      THEN
         IF Bits#8
         THEN
            WriteString('error incorrect number of bits in definition') ;
            WriteLn ;
            WriteString('      found') ; WriteCard(Bits, 2) ;
            WriteString(' in ') ; WriteString(OpcodeDefinition) ; WriteLn
         END ;
         Bits := 0 ;
         INC(OpI)
      ELSE
         WriteString('unrecognised character in definition: ') ;
         WriteString(OpcodeDefinition) ; WriteLn ;
         WriteString('             character ') ;
         Write(OpcodeDefinition[OpI]) ;
         WriteString(' character no. ') ; WriteCard(OpI, 3) ; WriteLn
      END
   END
END ParseOpcodeDefinition ;


(*
   ParseVariable - parse the variable inside the OpcodeDefinition
                   and store the variable value.
*)

PROCEDURE ParseVariable (OpcodeDefinition: ARRAY OF CHAR;
                         VAR OpI, InstI: CARDINAL) : CARDINAL ;
VAR
   Val,
   Bits : CARDINAL ;
   Value,
   Name : ARRAY [0..MaxInstructionLength] OF CHAR ;
   i    : CARDINAL ;
BEGIN
   Bits := 0 ;
   INC(OpI) ;
   i := 0 ;
   WHILE (OpI<HIGH(OpcodeDefinition)) AND (OpcodeDefinition[OpI]#nul) AND
         (OpcodeDefinition[OpI]#BitField) DO
      Name[i] := OpcodeDefinition[OpI] ;
      INC(i) ;
      INC(OpI)
   END ;
   IF i<HIGH(Name)
   THEN
      Name[i] := nul
   END ;
   IF (OpI<HIGH(OpcodeDefinition)) AND (OpcodeDefinition[OpI]=BitField)
   THEN
      INC(OpI) ;
      IF IsDigit(OpcodeDefinition[OpI])
      THEN
         Bits := ORD(OpcodeDefinition[OpI]) - ORD('0') ;
         i := 0 ;
         WHILE (InstI+i<HIGH(InstChar)) AND (i<Bits) DO
            Value[i] := InstChar[InstI+i] ;
            INC(i)
         END ;
         INC(OpI) ;
         Value[i] := nul ;
         StrToBin(Value, Val) ;
         PutValue(MakeKey(Name), Val)
      ELSE
         WriteString('expecting digit after bit field delimiter') ; WriteLn ;
         WriteString(OpcodeDefinition) ; WriteLn ;
         HALT
      END ;
      IF OpcodeDefinition[OpI]=Quote
      THEN
         INC(OpI)
      ELSE
         WriteString('expecting quote after variable and bitfield') ; WriteLn ;
         WriteString(OpcodeDefinition) ; WriteLn ;
         HALT
      END
   ELSE
      WriteString('missing bitfield from opcode definition: ') ;
      WriteString(OpcodeDefinition) ; WriteLn ;
      HALT
   END ;
   INC(InstI, Bits) ;
   RETURN( Bits )
END ParseVariable ;


(*
   ParseDigit - checks to see that the OpcodeDefinition matches the
                InstChar.
*)

PROCEDURE ParseDigit (OpcodeDefinition: ARRAY OF CHAR;
                      VAR OpI, InstI: CARDINAL) ;
BEGIN
   IF OpcodeDefinition[OpI]=InstChar[InstI]
   THEN
      INC(OpI) ;
      INC(InstI)
   ELSE
      WriteString('error found between definition and instruction') ;
      WriteLn ;
      WriteString('      definition: ') ; WriteString(OpcodeDefinition) ;
      WriteLn ;
      HALT
   END
END ParseDigit ;


(*
   FindInstruction - fills in the OpcodeDefinition and
                     NameTemplate which matches the next
                     sequence of bytes.
*)

PROCEDURE FindInstruction (VAR OpcodeDefinition,
                               NameTemplate: Name) : BOOLEAN ;
VAR
   i       : CARDINAL ;
   b       : BYTE ;
   Found   : BOOLEAN ;
   a,
   BinByte : ARRAY [0..7] OF CHAR ;
BEGIN
   CheckPreInstruction ;
   StrCopy('', InstChar) ;
   Found := FALSE ;
   WHILE (NOT Found) AND
         (NoOfBytes-NoOfDataBytes<MaxInstructionBytes) AND
         LocalGetByte(b) DO
      BinToStr(VAL(CARDINAL, b), 0, a) ;
      (* need to add leading 0 to this array *)
      i := 0 ;
      WHILE i<=7-StrLen(a) DO
         BinByte[i] := '0' ;
         INC(i)
      END ;
      IF i<HIGH(BinByte)
      THEN
         BinByte[i] := nul
      END ;
      StrConCat(BinByte, a, BinByte) ;                 
      StrConCat(InstChar, BinByte, InstChar) ;
      IF Debug
      THEN
         WriteString('building inst ') ; WriteString(InstChar) ; WriteLn
      END ;
      Found := GetInstruction(InstChar, OpcodeDefinition, NameTemplate)
   END ;
   RETURN( Found )
END FindInstruction ;


(*
   LocalGetByte - attempts to get a byte from the caller and
                  it stores the byte in a local array for further use.
                  It searches the local stack first.
*)

PROCEDURE LocalGetByte (VAR b: BYTE) : BOOLEAN ;
BEGIN
   IF StackPtr>0
   THEN
      b := Stack[StackPtr] ;
      DEC(StackPtr) ;
      InstByte[NoOfBytes] := b ;
      INC( NoOfBytes ) ;
      RETURN( TRUE )
   ELSIF IndirectGetByte(b)
   THEN
      InstByte[NoOfBytes] := b ;
      INC( NoOfBytes ) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END LocalGetByte ;


(*
   LocalPutByte - places the byte onto an internal stack.
*)

PROCEDURE LocalPutByte (b: BYTE) ;
BEGIN
   IF StackPtr<MaxStack
   THEN
      INC(StackPtr) ;
      Stack[StackPtr] := b ;
      (* make sure that we dont continually add to the InstByte *)
      IF NoOfBytes>0
      THEN
         DEC(NoOfBytes)
      END
   END
END LocalPutByte ;
   
   
(*
   DisplayAscii - displays the instruction in ASCII format.
*)

PROCEDURE DisplayAscii (From, To: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := From TO To DO
      IF IsAscii(CHAR(InstByte[i]))
      THEN
         Write(CHAR(InstByte[i]))
      ELSE
         Write('.')
      END
   END
END DisplayAscii ;


(*
   WriteSpaces - writes out n spaces.
*)

PROCEDURE WriteSpaces (n: CARDINAL) ;
BEGIN
   WHILE n#0 DO
      Write(' ') ;
      DEC(n)
   END
END WriteSpaces ;


(*
   DisplayFirstBytes - displays the first line of bytes in InstByte.
*)

PROCEDURE DisplayFirstBytes (CurrentAddress: CARDINAL;
                             Start, End: CARDINAL) ;
VAR
   Count,
   c    : CARDINAL ;
BEGIN
   Count := Min(End-Start, MaxBytesPerLine) ;
   IF Count#0
   THEN
      WriteHex(CurrentAddress, 8) ; WriteString('   ') ;
      c := Start ;
      WHILE c<End DO
         Write(' ') ;
         WriteHex(VAL(CARDINAL, InstByte[c]), 2) ;
         INC(c)
      END ;
      WriteSpaces((MaxBytesPerLine-Count)*3) ;
      WriteString('   ') ;
      DisplayAscii(Start, End-1)
   END
END DisplayFirstBytes ;


(*
   DisplayBytes - displays bytes with a newline every MaxBytesPerLine.
*)

PROCEDURE DisplayBytes (VAR CurrentAddress: CARDINAL;
                        Start, End: CARDINAL) ;
VAR
   Count,
   i, c : CARDINAL ;
BEGIN
   IF End>Start
   THEN
      c := Start ;
      WHILE c<End DO
         Count := Min(End-Start, MaxBytesPerLine) ;
         i := 0 ;
         WriteHex(CurrentAddress, 8) ; WriteString('   ') ;
         WHILE i<Count DO
            Write(' ') ;
            WriteHex(VAL(CARDINAL, InstByte[i+Start]), 2) ;
            INC(i)
         END ;
         WriteSpaces((MaxBytesPerLine-Count)*3) ;
         WriteString('   ') ;
         DisplayAscii(Start, Start+Count-1) ;
         INC(Start, Count) ;
         INC(c, Count) ;
         INC(CurrentAddress, Count) ;
         WriteLn
      END
   END
END DisplayBytes ;


(*
   BuildInstructionTable - builds the instruction data structure.
*)

PROCEDURE BuildInstructionTable ;
BEGIN
   (* Prefix bytes *)
   AddInstr('<11110000>', 'lock') ;
   AddInstr('<00101110>', 'seg cs') ;
   AddInstr('<00111110>', 'seg ds') ;
   AddInstr('<00100110>', 'seg es') ;
   AddInstr('<01100100>', 'seg fs') ;
   AddInstr('<01100101>', 'seg gs') ;
   AddInstr('<00110110>', 'seg ss') ;

   (* MOV *)
   AddInstr('<1000100"w:1"><"mod:2""reg:3""r/m:3">', 'mov  "%", "reg"') ;
   AddInstr('<1000101"w:1"><"mod:2""reg:3""r/m:3">', 'mov  "reg", "%"') ;
   AddInstr('<1100011"w:1"><"mod:2""reg:3""r/m:3">', 'mov."w"  "%", #"imm"') ;
   AddInstr('<1011"w:1""reg:3">', 'mov  "reg", #"imm"') ;
   AddInstr('<1010000"w:1">', 'mov  "eax", "imm"') ;
   AddInstr('<1010001"w:1">', 'mov  "imm", "eax"') ;
   AddInstr('<10001110><"mod:2""sreg3:3""r/m:3">', 'mov  "sreg3", "%"') ;
   AddInstr('<10001100><"mod:2""sreg3:3""r/m:3">', 'mov  "%", "sreg3"') ;

   (* MOVSX *)
   AddInstr('<00001111><1011111"w:1"><"mod:2""reg:3""r/m:3">', 'movsx  "reg", "%"') ;

   (* MOVZX *)
   AddInstr('<00001111><1011011"w:1"><"mod:2""reg:3""r/m:3">', 'movzx  "reg", "%"') ;

   (* PUSH *)
   AddInstr('<11111111><"mod:2"110"r/m:3">', 'push  "%"') ;
   AddInstr('<01010"reg:3">', 'push  "reg"') ;
   AddInstr('<000"sreg2:2"110>', 'push  "sreg2"') ;
   AddInstr('<00001111><10100000>', 'push  fs') ;
   AddInstr('<00001111><10101000>', 'push  gs') ;
   AddInstr('<01101000>', 'push  #"imm16/32"') ;
   AddInstr('<01101010>', 'push  #"imm8"') ;
   AddInstr('<01100000>', 'pushall') ;

   (* POP *)
   AddInstr('<10001111><"mod:2"000"r/m:3">', 'pop  "%"') ;
   AddInstr('<01011"reg:3">', 'pop  "reg"') ;
   AddInstr('<00000111>', 'pop  es') ;
   AddInstr('<00010111>', 'pop  ss') ;
   AddInstr('<00011111>', 'pop  ds') ;
   AddInstr('<00001111><10100001>', 'pop  fs') ;
   AddInstr('<00001111><10101001>', 'pop  gs') ;
   AddInstr('<01100001>', 'popall') ;

   (* IN *)
   AddInstr('<11100100>', 'in  al, "d8"') ;
   AddInstr('<11100101>', 'in  "eax", "d8"') ;
   AddInstr('<11101100>', 'in  al, dx') ;
   AddInstr('<11101101>', 'in  "eax", dx') ;

   (* OUT *)
   AddInstr('<11100110>', 'out  "d8", al') ;
   AddInstr('<11100111>', 'out  "d8", "eax"') ;
   AddInstr('<11101110>', 'out  dx, al') ;
   AddInstr('<11101111>', 'out  dx, "eax"') ;

   (* SEGMENT CONTROL *)
   AddInstr('<10001101><"mod:2""reg:3""r/m:3">', 'lea  "reg", "%"') ;
   AddInstr('<11000101><"mod:2""reg:3""r/m:3">', 'lds  "reg", "%"') ;
   AddInstr('<11000100><"mod:2""reg:3""r/m:3">', 'les  "reg", "%"') ;
   AddInstr('<00001111><10110100><"mod:2""reg:3""r/m:3">', 'lfs  "reg", "%"') ;
   AddInstr('<00001111><10110101><"mod:2""reg:3""r/m:3">', 'lgs  "reg", "%"') ;
   AddInstr('<00001111><10110010><"mod:2""reg:3""r/m:3">', 'lss  "reg", "%"') ;

   (* FLAG Control *)
   AddInstr('<11111000>', 'clc') ;
   AddInstr('<11111100>', 'cld    // clear direction') ;
   AddInstr('<11111010>', 'cli    // ints off') ;
   AddInstr('<00001111><00000110>', 'clts') ;
   AddInstr('<11110101>', 'cmc') ;
   AddInstr('<10011111>', 'lahf   // load ah from flags') ;
   AddInstr('<10011101>', 'popf') ;
   AddInstr('<10011100>', 'pushf') ;
   AddInstr('<10011110>', 'sahf   // store ah into flags') ;
   AddInstr('<11111001>', 'stc    // set carry') ;
   AddInstr('<11111101>', 'std    // set direction') ;
   AddInstr('<11111011>', 'sti    // ints on') ;

   (* ADD *)
   AddInstr('<0000000"w:1"><"mod:2""reg:3""r/m:3">', 'add  "%", "reg"') ;
   AddInstr('<0000001"w:1"><"mod:2""reg:3""r/m:3">', 'add  "reg", "%"') ;
   AddInstr('<100000"s:1""w:1"><"mod:2"000"r/m:3">', 'add  "%", #"s""imm"') ;
   AddInstr('<0000010"w:1">', 'add  "eax", #"imm"') ;

   (* ADC *)
   AddInstr('<0001000"w:1"><"mod:2""reg:3""r/m:3">', 'adc  "%", "reg"') ;
   AddInstr('<0001001"w:1"><"mod:2""reg:3""r/m:3">', 'adc  "reg", "%"') ;
   AddInstr('<100000"s:1""w:1"><"mod:2"010"r/m:3">', 'adc  "%", #"s""imm"') ;
   AddInstr('<00010100>', 'adc  "eax", #"imm"') ;

   (* INC *)
   AddInstr('<1111111"w:1"><"mod:2"000"r/m:3">', 'inc  "%"') ;
   AddInstr('<01000"reg:3">', 'inc  "reg"') ;

   (* DEC *)
   AddInstr('<1111111"w:1"><"mod:2"001"r/m:3">', 'dec  "%"') ;
   AddInstr('<01001"reg:3">', 'dec  "reg"') ;

   (* SUB *)
   AddInstr('<0010100"w:1"><"mod:2""reg:3""r/m:3">', 'sub  "%", "reg"') ;
   AddInstr('<0010101"w:1"><"mod:2""reg:3""r/m:3">', 'sub  "reg", "%"') ;
   AddInstr('<100000"s:1""w:1"><"mod:2"101"r/m:3">', 'sub  "%", #"s""imm"') ;
   AddInstr('<0010110"w:1">', 'sub  "eax", #"imm"') ;

   (* SBB *)
   AddInstr('<0001100"w:1"><"mod:2""reg:3""r/m:3">', 'sbb  "%", "reg"') ;
   AddInstr('<0001101"w:1"><"mod:2""reg:3""r/m:3">', 'sbb  "reg", "%"') ;
   AddInstr('<100000"s:1""w:1"><"mod:2"011"r/m:3">', 'sbb  "%", #"s""imm"') ;
   AddInstr('<00011100>', 'sbb  "eax", #"imm"') ;

   (* IMUL *)
   AddInstr('<1111011"w:1"><"mod:2"100"r/m:3">', 'imul  "eax", "%"') ;
   AddInstr('<00001111><10101111><"mod:2""reg:3""r/m:3">', 'imul  "reg", "%"') ;
   AddInstr('<011010"s:1"1><"mod:2""reg:3""r/m:3">', 'imul  "reg", "imm8", "%"') ;

   (* DIV *)
   AddInstr('<1111011"w:1"><"mod:2"110"r/m:3">', 'div  "eax", "%"') ;
   AddInstr('<1111011"w:1"><"mod:2"111"r/m:3">', 'idiv  "eax", "%"') ;

   (* CMP *)
   AddInstr('<0011100"w:1"><"mod:2""reg:3""r/m:3">', 'cmp  "%", "reg"') ;
   AddInstr('<0011101"w:1"><"mod:2""reg:3""r/m:3">', 'cmp  "reg", "%"') ;
   AddInstr('<100000"s:1""w:1"><"mod:2"111"r/m:3">', 'cmp  "%", #"s""imm"') ;
   AddInstr('<00111100>', 'cmp  "eax", #"imm"') ;

   (* AND *)
   AddInstr('<0010000"w:1"><"mod:2""reg:3""r/m:3">', 'and  "%", "reg"') ;
   AddInstr('<0010001"w:1"><"mod:2""reg:3""r/m:3">', 'and  "reg", "%"') ;
   AddInstr('<1000000"w:1"><"mod:2"100"r/m:3">', 'and  "%", #"imm"') ;
   AddInstr('<00100100>', 'and  "eax", "imm"') ;

   (* TEST *)
   AddInstr('<1000010"w:1"><"mod:2""reg:3""r/m:3">', 'test  "%", "reg"') ;
   AddInstr('<1111011"w:1"><"mod:2"000"r/m:3">', 'test  "%", #"imm"') ;
   AddInstr('<10101000>', 'test  "eax", "imm"') ;

   (* OR *)
   AddInstr('<0000100"w:1"><"mod:2""reg:3""r/m:3">', 'or  "%", "reg"') ;
   AddInstr('<0000101"w:1"><"mod:2""reg:3""r/m:3">', 'or  "reg", "%"') ;
   AddInstr('<1000000"w:1"><"mod:2"001"r/m:3">', 'or  "%", #"imm"') ;
   AddInstr('<00001100>', 'or  "eax", "imm"') ;

   (* XOR *)
   AddInstr('<0011000"w:1"><"mod:2""reg:3""r/m:3">', 'xor  "%", "reg"') ;
   AddInstr('<0011001"w:1"><"mod:2""reg:3""r/m:3">', 'xor  "reg", "%"') ;
   AddInstr('<100000"s:1""w:1"><"mod:2"110"r/m:3">', 'xor  "%", #"imm"') ;
   AddInstr('<00110100>', 'xor  "eax", "imm"') ;

   (* NOT *)
   AddInstr('<1111011"w:1"><"mod:2"010"r/m:3">', 'not  "%"') ;

   (* NEG *)
   AddInstr('<1111011"w:1"><"mod:2"011"r/m:3">', 'neg  "%"') ;

   (* INT *)
   AddInstr('<11001101>', 'int  #"d8"') ;

   (* IRET *)
   AddInstr('<11001111>', 'iret') ;

   (* HALT *)
   AddInstr('<11110100>', 'halt') ;

   (* NOP *)
   AddInstr('<10010000>', 'nop') ;

   (* REPEATED string manipulation *)
   AddInstr('<11110011><1010011"w:1">', 'repe cmps."w"   // find non match') ;
   AddInstr('<11110010><1010011"w:1">', 'repne cmps."w"  // find match') ;
   AddInstr('<11110010><0110110"w:1">', 'rep ins."w"') ;
   AddInstr('<11110010><1010110"w:1">', 'rep lods."w"') ;
   AddInstr('<11110011><1010010"w:1">', 'rep movs."w"') ;
   AddInstr('<11110010><0110111"w:1">', 'rep outs."w"') ;
   AddInstr('<11110011><1010111"w:1">', 'repe scas."w"') ;
   AddInstr('<11110010><1010111"w:1">', 'repne scas."w"') ;
   AddInstr('<11110010><1010101"w:1">', 'rep stos."w"') ;

   (* Prefixed by cs *)
   AddInstr('<11110011><00101110><1010011"w:1">', 'repe cs: cmps."w"') ;
   AddInstr('<11110010><00101110><1010011"w:1">', 'repne cs: cmps."w"') ;
   AddInstr('<11110010><00101110><0110110"w:1">', 'rep cs: ins."w"') ;
   AddInstr('<11110010><00101110><1010110"w:1">', 'rep cs: lods."w"') ;
   AddInstr('<11110011><00101110><1010010"w:1">', 'rep cs: movs."w"') ;
   AddInstr('<11110010><00101110><0110111"w:1">', 'rep cs: outs."w"') ;
   AddInstr('<11110011><00101110><1010111"w:1">', 'repe cs: scas."w"') ;
   AddInstr('<11110010><00101110><1010111"w:1">', 'repne cs: scas."w"') ;
   AddInstr('<11110010><00101110><1010101"w:1">', 'rep cs: stos."w"') ;

   (* Prefixed by ds *)
   AddInstr('<11110011><00111110><1010011"w:1">', 'repe ds: cmps."w"') ;
   AddInstr('<11110010><00111110><1010011"w:1">', 'repne ds: cmps."w"') ;
   AddInstr('<11110010><00111110><0110110"w:1">', 'rep ds: ins."w"') ;
   AddInstr('<11110010><00111110><1010110"w:1">', 'rep ds: lods."w"') ;
   AddInstr('<11110011><00111110><1010010"w:1">', 'rep ds: movs."w"') ;
   AddInstr('<11110010><00111110><0110111"w:1">', 'rep ds: outs."w"') ;
   AddInstr('<11110011><00111110><1010111"w:1">', 'repe ds: scas."w"') ;
   AddInstr('<11110010><00111110><1010111"w:1">', 'repne ds: scas."w"') ;
   AddInstr('<11110010><00111110><1010101"w:1">', 'rep ds: stos."w"') ;

   (* Prefixed by es *)
   AddInstr('<11110011><00100110><1010011"w:1">', 'repe es: cmps."w"') ;
   AddInstr('<11110010><00100110><1010011"w:1">', 'repne es: cmps."w"') ;
   AddInstr('<11110010><00100110><0110110"w:1">', 'rep es: ins."w"') ;
   AddInstr('<11110010><00100110><1010110"w:1">', 'rep es: lods."w"') ;
   AddInstr('<11110011><00100110><1010010"w:1">', 'rep es: movs."w"') ;
   AddInstr('<11110010><00100110><0110111"w:1">', 'rep es: outs."w"') ;
   AddInstr('<11110011><00100110><1010111"w:1">', 'repe es: scas."w"') ;
   AddInstr('<11110010><00100110><1010111"w:1">', 'repne es: scas."w"') ;
   AddInstr('<11110010><00100110><1010101"w:1">', 'rep es: stos."w"') ;

   (* Prefixed by fs *)
   AddInstr('<11110011><01100100><1010011"w:1">', 'repe fs: cmps."w"') ;
   AddInstr('<11110010><01100100><1010011"w:1">', 'repne fs: cmps."w"') ;
   AddInstr('<11110010><01100100><0110110"w:1">', 'rep fs: ins."w"') ;
   AddInstr('<11110010><01100100><1010110"w:1">', 'rep fs: lods."w"') ;
   AddInstr('<11110011><01100100><1010010"w:1">', 'rep fs: movs."w"') ;
   AddInstr('<11110010><01100100><0110111"w:1">', 'rep fs: outs."w"') ;
   AddInstr('<11110011><01100100><1010111"w:1">', 'repe fs: scas."w"') ;
   AddInstr('<11110010><01100100><1010111"w:1">', 'repne fs: scas."w"') ;
   AddInstr('<11110010><01100100><1010101"w:1">', 'rep fs: stos."w"') ;

   (* Prefixed by gs *)
   AddInstr('<11110011><01100101><1010011"w:1">', 'repe gs: cmps."w"') ;
   AddInstr('<11110010><01100101><1010011"w:1">', 'repne gs: cmps."w"') ;
   AddInstr('<11110010><01100101><0110110"w:1">', 'rep gs: ins."w"') ;
   AddInstr('<11110010><01100101><1010110"w:1">', 'rep gs: lods."w"') ;
   AddInstr('<11110011><01100101><1010010"w:1">', 'rep gs: movs."w"') ;
   AddInstr('<11110010><01100101><0110111"w:1">', 'rep gs: outs."w"') ;
   AddInstr('<11110011><01100101><1010111"w:1">', 'repe gs: scas."w"') ;
   AddInstr('<11110010><01100101><1010111"w:1">', 'repne gs: scas."w"') ;
   AddInstr('<11110010><01100101><1010101"w:1">', 'rep gs: stos."w"') ;

   (* Prefixed by ss *)
   AddInstr('<11110011><00110110><1010011"w:1">', 'repe ss: cmps."w"') ;
   AddInstr('<11110010><00110110><1010011"w:1">', 'repne ss: cmps."w"') ;
   AddInstr('<11110010><00110110><0110110"w:1">', 'rep ss: ins."w"') ;
   AddInstr('<11110010><00110110><1010110"w:1">', 'rep ss: lods."w"') ;
   AddInstr('<11110011><00110110><1010010"w:1">', 'rep ss: movs."w"') ;
   AddInstr('<11110010><00110110><0110111"w:1">', 'rep ss: outs."w"') ;
   AddInstr('<11110011><00110110><1010111"w:1">', 'repe ss: scas."w"') ;
   AddInstr('<11110010><00110110><1010111"w:1">', 'repne ss: scas."w"') ;
   AddInstr('<11110010><00110110><1010101"w:1">', 'rep ss: stos."w"') ;

   (* CALL *)
   AddInstr('<11101000>', 'call  "disp16/32"') ;
   AddInstr('<11111111><"mod:2"010"r/m:3">', 'call  ["%"]') ;

   (* RET *)
   AddInstr('<11000011>', 'ret  near\n') ;
   AddInstr('<11000010>', 'ret  near, sp+d16\n') ;
   AddInstr('<11001011>', 'ret  far\n') ;
   AddInstr('<11001010>', 'ret  far, sp+16\n') ;

   (* LEAVE *)
   AddInstr('<11001001>', 'leave') ;

   (* JMP *)
   AddInstr('<11101011>', 'jmp  near "disp8"') ;
   AddInstr('<11101001>', 'jmp  near "disp16/32"') ;
   AddInstr('<11111111><"mod:2"100"r/m:3">', 'jmp  near ["%"]') ;
   AddInstr('<11101010>', 'jmp  far "seg/offset"') ;
   AddInstr('<11111111><"mod:2"101"r/m:3">', 'jmp  far ["%"]') ;

   (* CONDITIONAL JMPs *)
   AddInstr('<01110000>', 'jo  "disp8"') ;
   AddInstr('<00001111><10000000>', 'jo  "disp16/32"') ;
   AddInstr('<01110001>', 'jno  "disp8"') ;
   AddInstr('<00001111><10000001>', 'jno  "disp16/32"') ;
   AddInstr('<01110010>', 'jb  "disp8"') ;
   AddInstr('<00001111><10000010>', 'jb  "disp16/32"') ;
   AddInstr('<01110011>', 'jnb  "disp8"') ;
   AddInstr('<00001111><10000011>', 'jnb  "disp16/32"') ;
   AddInstr('<01110100>', 'je  "disp8"') ;
   AddInstr('<00001111><10000100>', 'je  "disp16/32"') ;
   AddInstr('<01110101>', 'jne  "disp8"') ;
   AddInstr('<00001111><10000101>', 'jne  "disp16/32"') ;
   AddInstr('<01110110>', 'jbe  "disp8"') ;
   AddInstr('<00001111><10000110>', 'jbe  "disp16/32"') ;
   AddInstr('<01110111>', 'jnbe  "disp8"') ;
   AddInstr('<00001111><10000111>', 'jnbe  "disp16/32"') ;
   AddInstr('<01111000>', 'js  "disp8"') ;
   AddInstr('<00001111><10001000>', 'js  "disp16/32"') ;
   AddInstr('<01111001>', 'jns  "disp8"') ;
   AddInstr('<00001111><10001001>', 'jns  "disp16/32"') ;
   AddInstr('<01111010>', 'jpe  "disp8" // jump if parity even') ;
   AddInstr('<00001111><10001010>', 'jpe  "disp16/32" // jump if parity even') ;
   AddInstr('<01111011>', 'jpo  "disp8" // jump if parity odd') ;
   AddInstr('<00001111><10001011>', 'jpo  "disp16/32 // jump if parity odd"') ;
   AddInstr('<01111100>', 'jl  "disp8"') ;
   AddInstr('<00001111><10001100>', 'jl  "disp16/32"') ;
   AddInstr('<01111101>', 'jnl  "disp8"') ;
   AddInstr('<00001111><10001101>', 'jnl  "disp16/32"') ;
   AddInstr('<01111110>', 'jle  "disp8"') ;
   AddInstr('<00001111><10001110>', 'jle  "disp16/32"') ;
   AddInstr('<01111111>', 'jg  "disp8"') ;
   AddInstr('<00001111><10001111>', 'jg  "disp16/32"') ;
   AddInstr('<11100011>', '"jecx"z  "disp8"') ;
   AddInstr('<11100010>', 'while cx>0 do  "disp8"') ;
   AddInstr('<11100001>', 'while equal do  "disp8"') ;
   AddInstr('<11100000>', 'while not equal do  "disp8"') ;

   (* LOGIC *)
   AddInstr('<1101000"w:1"><"mod:2"000"r/m:3">', 'rol  "%", #1') ;
   AddInstr('<1101001"w:1"><"mod:2"000"r/m:3">', 'rol  "%", cl') ;
   AddInstr('<1100000"w:1"><"mod:2"000"r/m:3">', 'rol  "%", #"imm8"') ;

   AddInstr('<1101000"w:1"><"mod:2"001"r/m:3">', 'ror  "%", #1') ;
   AddInstr('<1101001"w:1"><"mod:2"001"r/m:3">', 'ror  "%", cl') ;
   AddInstr('<1100000"w:1"><"mod:2"001"r/m:3">', 'ror  "%", #"imm8"') ;

   AddInstr('<1101000"w:1"><"mod:2"100"r/m:3">', 'sal  "%", #1') ;
   AddInstr('<1101001"w:1"><"mod:2"100"r/m:3">', 'sal  "%", cl') ;
   AddInstr('<1100000"w:1"><"mod:2"100"r/m:3">', 'sal  "%", #"imm8"') ;

   AddInstr('<1101000"w:1"><"mod:2"111"r/m:3">', 'sar  "%", #1') ;
   AddInstr('<1101001"w:1"><"mod:2"111"r/m:3">', 'sar  "%", cl') ;
   AddInstr('<1100000"w:1"><"mod:2"111"r/m:3">', 'sar  "%", #"imm8"') ;

   AddInstr('<1101000"w:1"><"mod:2"010"r/m:3">', 'rcl  "%", #1') ;
   AddInstr('<1101001"w:1"><"mod:2"010"r/m:3">', 'rcl  "%", cl') ;
   AddInstr('<1100000"w:1"><"mod:2"010"r/m:3">', 'rcl  "%", #"imm8"') ;

   AddInstr('<1101000"w:1"><"mod:2"011"r/m:3">', 'rcr  "%", #1') ;
   AddInstr('<1101001"w:1"><"mod:2"011"r/m:3">', 'rcr  "%", cl') ;
   AddInstr('<1100000"w:1"><"mod:2"011"r/m:3">', 'rcr  "%", #"imm8"') ;

   AddInstr('<00001111><10100100><"mod:2""reg:3""r/m:3">', 'shld  "%", "reg", #"imm8"') ;
   AddInstr('<00001111><10100101><"mod:2""reg:3""r/m:3">', 'shld  "%", "reg", cl') ;
   AddInstr('<00001111><10101100><"mod:2""reg:3""r/m:3">', 'shrd  "%", "reg", #"imm8"') ;
   AddInstr('<00001111><10101101><"mod:2""reg:3""r/m:3">', 'shrd  "%", "reg", cl') ;

   (* Protection control *)
   AddInstr('<01100011><"mod:2""reg:3""r/m:3">', 'arpl  "reg", "%"') ;
   AddInstr('<00001111><00000010><"mod:2""reg:3""r/m:3">', 'lar  "reg", "%"') ;
   AddInstr('<00001111><00000001><"mod:2"010"r/m:3">', 'lgdt  "%"') ;
   AddInstr('<00001111><00000001><"mod:2"011"r/m:3">', 'lidt  "%"') ;
   AddInstr('<00001111><00000000><"mod:2"010"r/m:3">', 'lidt  "%"') ;
   AddInstr('<00001111><00000001><"mod:2"110"r/m:3">', 'lmsw  "%"') ;
   AddInstr('<00001111><00000011><"mod:2""reg:3""r/m:3">', 'lsl  "reg", "%"') ;
   AddInstr('<00001111><00000000><"mod:2"011"r/m:3">', 'ltr  "%"') ; (* 011 ?? *)
   AddInstr('<00001111><00000001><"mod:2"000"r/m:3">', 'sgdt  "%"') ;
   AddInstr('<00001111><00000001><"mod:2"001"r/m:3">', 'sidt  "%"') ;
   AddInstr('<00001111><00000000><"mod:2"000"r/m:3">', 'sldt  "%"') ;
   AddInstr('<00001111><00000001><"mod:2"100"r/m:3">', 'smsw  "%"') ;
   AddInstr('<00001111><00000000><"mod:2"001"r/m:3">', 'str  "%"') ;
   AddInstr('<00001111><00000000><"mod:2"100"r/m:3">', 'verr  "%"') ;
   AddInstr('<00001111><00000000><"mod:2"101"r/m:3">', 'verw  "%"') ;

END BuildInstructionTable ;


(*
   AddInstr - adds an instruction entry into the opcode table.
*)

PROCEDURE AddInstr (OpcodeDefinition, NameTemplate: ARRAY OF CHAR) ;
BEGIN
   PutInstruction(MakeKey(OpcodeDefinition), MakeKey(NameTemplate))
END AddInstr ;


(*
   BuildOperandTable - builds the operand data structure.
*)

PROCEDURE BuildOperandTable ;
BEGIN
   (* see page E-19 of the 386SX programmers reference manual *)

   (*
             +-------- address 32 bits = 1      16 bits = 0
             |+------- data 32 bits = 1         16 bits = 0
             ||
             ||  R/M
             ||M 
             || O   +- W bit
             ||D    |
             vvvvvvvv
   *)
   
   AddOper('<0X00000X>', 'ds:[bx+si]') ;
   AddOper('<0X00001X>', 'ds:[bx+di]') ;
   AddOper('<0X00010X>', 'ss:[bp+si]') ;
   AddOper('<0X00011X>', 'ss:[bp+di]') ;
   AddOper('<0X00100X>', 'ds:[si]') ;
   AddOper('<0X00101X>', 'ds:[di]') ;
   AddOper('<0X00110X>', 'ds:"d16"') ;
   AddOper('<0X00111X>', 'ds:[bx]') ;

   AddOper('<0X01000X>', 'ds:[bx+si+"d8"]') ;
   AddOper('<0X01001X>', 'ds:[bx+di+"d8"]') ;
   AddOper('<0X01010X>', 'ss:[bp+si+"d8"]') ;
   AddOper('<0X01011X>', 'ss:[bp+di+"d8"]') ;
   AddOper('<0X01100X>', 'ds:[si+"d8"]') ;
   AddOper('<0X01101X>', 'ds:[di+"d8"]') ;
   AddOper('<0X01110X>', 'ss:[bp+"d8"]') ;
   AddOper('<0X01111X>', 'ds:[bx+"d8"]') ;

   AddOper('<0X10000X>', 'ds:[bx+si+"d16"]') ;
   AddOper('<0X10001X>', 'ds:[bx+di+"d16"]') ;
   AddOper('<0X10010X>', 'ss:[bp+si+"d16"]') ;
   AddOper('<0X10011X>', 'ss:[bp+di+"d16"]') ;
   AddOper('<0X10100X>', 'ds:[si+"d16"]') ;
   AddOper('<0X10101X>', 'ds:[di+"d16"]') ;
   AddOper('<0X10110X>', 'ss:[bp+"d16"]') ;
   AddOper('<0X10111X>', 'ds:[bx+"d16"]') ;

   AddOper('<00110000>', 'al') ; AddOper('<00110001>', 'ax') ;
   AddOper('<00110010>', 'cl') ; AddOper('<00110011>', 'cx') ;
   AddOper('<00110100>', 'dl') ; AddOper('<00110101>', 'dx') ;
   AddOper('<00110110>', 'bl') ; AddOper('<00110111>', 'bx') ;
   AddOper('<00111000>', 'ah') ; AddOper('<00111001>', 'sp') ;
   AddOper('<00111010>', 'ch') ; AddOper('<00111011>', 'bp') ;
   AddOper('<00111100>', 'dh') ; AddOper('<00111101>', 'si') ;
   AddOper('<00111110>', 'bh') ; AddOper('<00111111>', 'di') ;

   AddOper('<01110000>', 'al') ; AddOper('<01110001>', 'eax') ;
   AddOper('<01110010>', 'cl') ; AddOper('<01110011>', 'ecx') ;
   AddOper('<01110100>', 'dl') ; AddOper('<01110101>', 'edx') ;
   AddOper('<01110110>', 'bl') ; AddOper('<01110111>', 'ebx') ;
   AddOper('<01111000>', 'ah') ; AddOper('<01111001>', 'esp') ;
   AddOper('<01111010>', 'ch') ; AddOper('<01111011>', 'ebp') ;
   AddOper('<01111100>', 'dh') ; AddOper('<01111101>', 'esi') ;
   AddOper('<01111110>', 'bh') ; AddOper('<01111111>', 'edi') ;

   (* finished page E-19 *)

   (* Starting page E-20 *)

   AddOper('<1X00000X>', 'ds:[eax]') ;
   AddOper('<1X00001X>', 'ds:[ecx]') ;
   AddOper('<1X00010X>', 'ds:[edx]') ;
   AddOper('<1X00011X>', 'ds:[ebx]') ;
   AddOper('<1X00100X>', 's-i-b') ;
   AddOper('<1X00101X>', 'ds:"d32"') ;
   AddOper('<1X00110X>', 'ds:[esi]') ;
   AddOper('<1X00111X>', 'ds:[edi]') ;

   AddOper('<1X01000X>', 'ds:[eax+"d8"]') ;
   AddOper('<1X01001X>', 'ds:[ecx+"d8"]') ;
   AddOper('<1X01010X>', 'ds:[edx+"d8"]') ;
   AddOper('<1X01011X>', 'ds:[ebx+"d8"]') ;
   AddOper('<1X01100X>', 's-i-b') ;
   AddOper('<1X01101X>', 'ss:[ebp+"d8"]') ;
   AddOper('<1X01110X>', 'ds:[esi+"d8"]') ;
   AddOper('<1X01111X>', 'ds:[edi+"d8"]') ;

   AddOper('<1X10000X>', 'ds:[eax+"d32"]') ;
   AddOper('<1X10001X>', 'ds:[ecx+"d32"]') ;
   AddOper('<1X10010X>', 'ds:[edx+"d32"]') ;
   AddOper('<1X10011X>', 'ds:[ebx+"d32"]') ;
   AddOper('<1X10100X>', 's-i-b') ;
   AddOper('<1X10101X>', 'ss:[ebp+"d32"]') ;
   AddOper('<1X10110X>', 'ds:[esi+"d32"]') ;
   AddOper('<1X10111X>', 'ds:[edi+"d32"]') ;

   AddOper('<10110000>', 'al') ;
   AddOper('<10110010>', 'cl') ;
   AddOper('<10110100>', 'dl') ;
   AddOper('<10110110>', 'bl') ;
   AddOper('<10111000>', 'ah') ;
   AddOper('<10111010>', 'ch') ;
   AddOper('<10111100>', 'dh') ;
   AddOper('<10111110>', 'bh') ;

   AddOper('<10110001>', 'ax') ;
   AddOper('<10110011>', 'cx') ;
   AddOper('<10110101>', 'dx') ;
   AddOper('<10110111>', 'bx') ;
   AddOper('<10111001>', 'sp') ;
   AddOper('<10111011>', 'bp') ;
   AddOper('<10111101>', 'si') ;
   AddOper('<10111111>', 'di') ;

   AddOper('<11110000>', 'al') ;
   AddOper('<11110010>', 'cl') ;
   AddOper('<11110100>', 'dl') ;
   AddOper('<11110110>', 'bl') ;
   AddOper('<11111000>', 'ah') ;
   AddOper('<11111010>', 'ch') ;
   AddOper('<11111100>', 'dh') ;
   AddOper('<11111110>', 'bh') ;

   AddOper('<11110001>', 'eax') ;
   AddOper('<11110011>', 'ecx') ;
   AddOper('<11110101>', 'edx') ;
   AddOper('<11110111>', 'ebx') ;
   AddOper('<11111001>', 'esp') ;
   AddOper('<11111011>', 'ebp') ;
   AddOper('<11111101>', 'esi') ;
   AddOper('<11111111>', 'edi') ;
END BuildOperandTable ;


(*
   AddOper - adds an instruction entry into the opcode table.
*)

PROCEDURE AddOper (Definition, Name: ARRAY OF CHAR) ;
BEGIN
   PutOperand(MakeKey(Definition), MakeKey(Name))
END AddOper ;


(*
   WriteSubString - writes a subsection of string, a, from, Begin, with
                    length, Length.
*)

PROCEDURE WriteSubString (a: ARRAY OF CHAR; Begin, Length: CARDINAL) ;
VAR
   i,
   High: CARDINAL ;
BEGIN
   High := StrLen(a) ;
   i := 0 ;
   WHILE (Begin+i<High) AND (i<Length) DO
      Write(a[Begin+i]) ;
      INC(i)
   END
END WriteSubString ;


(*
   Min - returns the minimum of two CARDINAL values.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;
   

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
   IsAscii - returns true if ch is a printable character.
*)

PROCEDURE IsAscii (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch >= ' ') AND (ch <= 'z') )
END IsAscii ;


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
      IF StrEqual(a, '-D')
      THEN
         Dbit := 1-Dbit
      ELSIF StrEqual(a, '-d')
      THEN
         Debug := TRUE
      END ;
      INC(i)
   END
END ParseArguments ;


(*
   Init - builds the instruction table data structures.
*)

PROCEDURE Init ;
BEGIN
   BuildInstructionTable ;
   BuildOperandTable ;
   StackPtr := 0 ;
   Dbit := DefaultDbit ;
   ParseArguments
END Init ;


BEGIN
   Init
END Translate.
