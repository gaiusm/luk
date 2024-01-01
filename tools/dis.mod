MODULE dis ;


FROM SYSTEM IMPORT ADR, TSIZE, SIZE, BYTE ;
FROM Args IMPORT GetArg, Narg ;
FROM NumberIO IMPORT WriteCard, WriteHex, StrToCard ;
FROM StrLib IMPORT StrEqual, StrCopy ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT StrToHex ;
FROM Translate IMPORT ProcGet, TranslateInstruction ;
FROM ASCII IMPORT nul ;

FROM WordSizes IMPORT  SHORTWORD, LONGWORD,
                       ShortWordToCardinal, LongWordToCardinal,
                       WriteLong, WriteShort ;

FROM AOUT IMPORT Examine,
                 GetByteFromText, GetByteFromData,
                 GetTextPhysicalAddress, GetDataPhysicalAddress,
                 GetTextVirtualAddress, GetDataVirtualAddress,
                 GetDataPhysicalSize, GetDataVirtualSize,
                 GetTextPhysicalSize, GetTextVirtualSize ;

CONST
   MaxLine    = 1024 ;


VAR
   InputName         : ARRAY [0..MaxLine] OF CHAR ;
   IgnoreInstructions,
   Verbose           : BOOLEAN ;
   Debug             : BOOLEAN ;
   OriginAddress     : CARDINAL ;


(*
   ScanSegments - scans all the segments.
*)

PROCEDURE ScanSegments ;
VAR
   GetByte: ProcGet ;
   n, i, j: CARDINAL ;
BEGIN
   GetByte := GetByteFromText ;
   n := GetTextVirtualSize() ;
   i := 0 ;
   WHILE i<n DO
      j := i ;
      IF OriginAddress=0
      THEN
         INC(i, TranslateInstruction(IgnoreInstructions, GetTextVirtualAddress()+i, GetByte))
      ELSE
         INC(i, TranslateInstruction(IgnoreInstructions, OriginAddress+i, GetByte))
      END ;
      IF j=i
      THEN
         WriteString('TranslateInstruction returned 0 bytes - why?') ; WriteLn
      END
   END
END ScanSegments ;


(*
   Init - parses arguments and then scans the a.out file.
*)

PROCEDURE Init ;
BEGIN
   IgnoreInstructions := FALSE ;
   Verbose := FALSE ;
   Debug := FALSE ;
   OriginAddress := 0 ;
   ParseArguments ;
   DoForeachFile
END Init ;


(*
   DoForeachFile - foreach file on the argument list scan it segments
      	       	   and write text and data to the output file.
*)

PROCEDURE DoForeachFile ;
VAR
   i: CARDINAL ;
BEGIN
   i := GetNthFile(1, InputName) ;
   WHILE i#0 DO
      IF Examine(InputName)
      THEN
         WriteString('File        : ') ; WriteString(InputName) ; WriteLn ;
         WriteLn ;
         ScanSegments
      ELSE
         WriteString('unable to find ') ; WriteString(InputName) ; WriteLn
      END ;
      i := GetNthFile(i+1, InputName)
   END
END DoForeachFile ;


(*
   GetNthFile - returns the index of the current file name.
*)

PROCEDURE GetNthFile (i: CARDINAL; VAR a: ARRAY OF CHAR) : CARDINAL ;
VAR
   Argc: CARDINAL ;
BEGIN
   Argc := Narg() ;
   WHILE i<Argc DO
      IF GetArg(a, i)
      THEN
         IF StrEqual(a, '-H')
         THEN
            INC(i)
         ELSIF NOT (StrEqual(a, '-v') OR StrEqual(a, '-d') OR
                    StrEqual(a, '-h') OR StrEqual(a, '-i') OR
                    StrEqual(a, '-D'))
         THEN
            RETURN( i )
         END
      ELSE
      	 HALT  (* Should never reach here *)
      END ;
      INC(i)
   END ;
   RETURN( 0 )   (* No more arguments left *)
END GetNthFile ;


(*
   ParseArguments - parses the command line arguments.
*)

PROCEDURE ParseArguments ;
VAR
   i: CARDINAL ;
   a: ARRAY [0..MaxLine] OF CHAR ;
BEGIN
   i := 1 ;
   WHILE GetArg(a, i) DO
      IF StrEqual(a, '-v')
      THEN
      	 Verbose := TRUE
      ELSIF StrEqual(a, '-d')
      THEN
      	 Debug := TRUE
      ELSIF StrEqual(a, '-i')
      THEN
         IgnoreInstructions := TRUE
      ELSIF StrEqual(a, '-h')
      THEN
         WriteString('Usage: dis       [-v][-d][-h][-i] { <file> }') ;
         WriteString('       -v        verbose') ; WriteLn ;
         WriteString('       -d        debug') ; WriteLn ;
         WriteString('       -i        ignore instructions - just generate hex, ascii and addresses') ; WriteLn ;
         WriteString('       -D        flip the default D bit (default = 16 bits) to 32 bits') ; WriteLn ;
         WriteString('       -H #      hex start address') ; WriteLn ;
         WriteString('       -check    perform internal checks on instructions') ; WriteLn ;
         WriteLn
      ELSIF StrEqual(a, '-H') AND GetArg(a, i+1)
      THEN
         INC(i) ;
         StrToHex(a, OriginAddress)
      END ;
      INC(i)
   END
END ParseArguments ;


(*
   IsDigit - returns true if ch is a digit.
*)

PROCEDURE IsDigit (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( (ch >= '0') AND (ch <='9') )
END IsDigit ;


BEGIN
   Init
END dis.
