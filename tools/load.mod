MODULE load ;


FROM SYSTEM IMPORT ADR, TSIZE, SIZE, BYTE ;
FROM ASCII IMPORT nul ;
FROM Args IMPORT GetArg, Narg ;
FROM NumberIO IMPORT WriteCard, WriteHex, StrToCard ;
FROM StrLib IMPORT StrEqual, StrCopy, StrLen ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM Debug IMPORT Halt ;

FROM FIO IMPORT OpenForRandom, File, Close,
                SetPositionFromBeginning, FindPosition,
      	       	WriteNBytes, OpenToRead, ReadChar,
                IsNoError, Exists ;

FROM AOUT IMPORT Examine,
                 GetByteFromText, GetByteFromData,
                 GetTextPhysicalAddress, GetDataPhysicalAddress,
                 GetTextVirtualAddress, GetDataVirtualAddress,
                 GetDataPhysicalSize, GetDataVirtualSize,
                 GetTextPhysicalSize, GetTextVirtualSize,
                 GetBssVirtualSize, DebugFile ;

FROM WordSizes IMPORT SHORTWORD ;


CONST
   KiloByte             =        1024   ;   (* 1 K                          *)
   MegaByte             = KiloByte * KiloByte ;  (* 1 M                     *)
   ClickSize            =          16   ;   (* A 8086 segment register has  *)
                                            (* granuality 16b               *)
   DefaultStack         = 1 * MegaByte  ;   (* stack for realtime system    *)
   SectorSize           =         512   ;   (* Sector size of a floppy disk *)
   DefaultMaxSecondSize =          15-1 ;   (* boot.S max second size load  *)
   DisplayBuild         =         TRUE  ;   (* display the build info       *)
   MaxOSSize            = (640-64) * KiloByte ;  (* max OS (base mem + boot) *)

   MaxCheckSum          = 0100H ;

   (* offsets in the first sector *)
   StackSizeOffset      = 512-14 ;
   DebugGDBOffset       = 512-12 ;
   OSStartOffset        = 512-10 ;
   OSSizeOffset         = 512-8 ;
   SecondDataOffset     = 512-6 ;
   SecondSizeOffset     = 512-4 ;
   SignatureOffset      = 512-2 ;

   DebugThird           = FALSE ;

CONST
   MaxLine    = 1024 ;



VAR
   fo          : File ;
   InputName,
   OutputName  : ARRAY [0..MaxLine] OF CHAR ;
   SeperateID  : BOOLEAN ;   (* Seperate Instruction and Data memory model    *)
   Verbose     : BOOLEAN ;
   Debug       : BOOLEAN ;   (* debug this load program (not RTS as below)    *)
   FileOffset  : CARDINAL ;
   SectorNo    : CARDINAL ;
   FileCount   : CARDINAL ;
   SecondSize  : CARDINAL ;  (* offset 510    I+D of secondary boot (sectors) *)
   SecondData  : CARDINAL ;  (* offset 508      D of secondary boot (sectors) *)
   OSSize      : CARDINAL ;  (* offset 506    OS size               (sectors) *)
   OSStart     : CARDINAL ;  (* offset 504    OS start track on floppy of OS. *)
   RunTimeGDB  : CARDINAL ;  (* offset 502    runtime debugging? ie -g        *)
   TextFirst   : BOOLEAN ;
   DataAsBss   : BOOLEAN ;   (* Treat data as bss providing it is all 0       *)
   IsMagic     : BOOLEAN ;   (* Are the following files magic? or raw?        *)
   HeaderValue : CARDINAL ;  (* header offset for binary files.               *)
   StackSize   : CARDINAL ;  (* how much stack is required for main of the OS *)
   needChecksum: BOOLEAN ;   (* should we emit the checksum byte at the end   *)
                             (* of the data segment.                          *)


(*
   SkipBinaryHeader - skips an optional binary header.
*)

PROCEDURE SkipBinaryHeader (fi: File) ;
VAR
   Buffer: BYTE ;
BEGIN
   WHILE HeaderValue>0 DO
      DEC(HeaderValue) ;
      Buffer := BYTE(ReadChar(fi))
   END
END SkipBinaryHeader ;


(*
   Max -
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   checkEmitChecksum -
*)

PROCEDURE checkEmitChecksum (current, last: CARDINAL; checkSum: CARDINAL; byte: BYTE) : BYTE ;
VAR
   i: CARDINAL ;
BEGIN
   IF needChecksum
   THEN
      IF current+1=last
      THEN
         IF byte#BYTE(0)
         THEN
            Halt('the byte where the checksum is to be stored should be zero',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;
         WriteLn ;
         WriteString('checksum byte') ; WriteLn ;
         i := 0 ;
         WriteString('  ') ;
         WHILE i<current MOD 16 DO
            WriteString('   ') ;
            INC(i)
         END ;
         WriteString('->') ;
         byte := VAL(BYTE, checkSum)
      END
   END ;
   RETURN( byte )
END checkEmitChecksum ;


(*
   ScanBinaryFile - treats the scans the input file as a binary file and
                    writes all the contents to the output file.
*)

PROCEDURE ScanBinaryFile (name: ARRAY OF CHAR) : CARDINAL ;
VAR
   fi      : File ;
   i       : CARDINAL ;
   checkSum: CARDINAL ;
   Buffer  : BYTE ;
BEGIN
   IF Exists(name)
   THEN
      WriteString('raw file found: ') ; WriteString(name) ; WriteLn
   END ;
   IF Verbose
   THEN
      WriteString('Text and Data...') ; WriteLn
   END ;
   checkSum := 0 ;
   i := 0 ;
   fi := OpenToRead(name) ;
   IF IsNoError(fi)
   THEN
      SkipBinaryHeader(fi) ;
      REPEAT
         Buffer := BYTE(ReadChar(fi)) ;
         IF IsNoError(fi)
         THEN
            IF WriteNBytes(fo, SIZE(Buffer), ADR(Buffer)) # SIZE(Buffer)
            THEN
               Halt('failed to write the correct number of bytes',
                 __FILE__, __FUNCTION__, __LINE__)
            END ;
            IF Verbose
            THEN
               IF ((FileOffset+i) MOD 16) = 0
               THEN
                  WriteLn ;
                  WriteHex(FileOffset+i, 4)
               END ;
               Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2) ;
            END ;
            INC(checkSum, VAL(CARDINAL, Buffer)) ;
            checkSum := checkSum MOD MaxCheckSum ;
            INC(i)
         END
      UNTIL NOT IsNoError(fi) ;
      Close(fi)
   ELSE
      WriteString('hmm an error occurred on: ') ; WriteString(name) ; WriteLn ;
      HALT(1)
   END ;
   INC(FileOffset, i) ;
   IF SeperateID
   THEN
      FileOffset := PadToNextSector(FileOffset)
   ELSE
      FileOffset := PadToNextClick(FileOffset)
   END ;
   IF Verbose
   THEN
      WriteLn
   END ;
   RETURN( checkSum )
END ScanBinaryFile ;


(*
   collectText -
*)

PROCEDURE collectText (fo: File; i, lastAddress, checkSum: CARDINAL) : CARDINAL ;
VAR
   ok    : BOOLEAN ;
   buffer: BYTE ;
BEGIN
   ok := GetByteFromText(buffer) ;
   IF NOT ok
   THEN
      Halt('failed to read expected byte from section',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   IF DataAsBss
   THEN
      buffer := checkEmitChecksum(i, lastAddress, checkSum, buffer)
   END ;
   IF WriteNBytes(fo, SIZE(buffer), ADR(buffer)) # SIZE(buffer)
   THEN
      Halt('failed to write the correct number of bytes',
           __FILE__, __FUNCTION__, __LINE__)
   END ;

   IF Verbose
   THEN
      IF ((FileOffset+i) MOD 16) = 0
      THEN
         WriteLn ;
         WriteHex(FileOffset+i, 4)
      END ;
      Write(' ') ; WriteHex(VAL(CARDINAL, buffer), 2) ;
   END ;
   INC(checkSum, VAL(CARDINAL, buffer)) ;
   checkSum := checkSum MOD MaxCheckSum ;
   RETURN checkSum
END collectText ;


(*
   stop -
*)

PROCEDURE stop ;
BEGIN
END stop ;


(*
   ScanTextSegment - scans the input file for the text segment and
                     writes the text to the output file.
*)

PROCEDURE ScanTextSegment (checkSum: CARDINAL) : CARDINAL ;
VAR
   LastAddress,
   VirtSize,
   PhysSize: CARDINAL ;
   i       : CARDINAL ;
   ok      : BOOLEAN ;
BEGIN
   IF Verbose
   THEN
      WriteString('Text...') ; WriteLn
   END ;
   PhysSize := GetTextPhysicalSize() ;
   VirtSize := GetTextVirtualSize() ;
   IF PhysSize#VirtSize
   THEN
      stop
   END ;
   LastAddress := Max(PhysSize, VirtSize) ;
   ok := TRUE ;
   i := 0 ;
   WHILE (i<PhysSize) AND ok DO
      checkSum := collectText(fo, i, LastAddress, checkSum) ;
      INC(i)
   END ;
   WHILE (i<VirtSize) AND ok DO
      checkSum := collectText(fo, i, LastAddress, checkSum) ;
      INC(i)
   END ;
   INC(FileOffset, i) ;
   IF FileCount<3
   THEN
      IF SeperateID
      THEN
         Halt('not expecting separate ID within the first two boot stages',
           __FILE__, __FUNCTION__, __LINE__)
      ELSE
         FileOffset := PadToNextClick(FileOffset)
      END
   END ;
   IF Verbose
   THEN
      WriteLn
   END ;
   RETURN checkSum
END ScanTextSegment ;


(*
   collectData -
*)

PROCEDURE collectData (fo: File; i, lastAddress, checkSum: CARDINAL) : CARDINAL ;
VAR
   ok    : BOOLEAN ;
   buffer: BYTE ;
BEGIN
   ok := GetByteFromData(buffer) ;
   IF DataAsBss
   THEN
      (* here we allow data bytes of zero to be omitted from the image, however _all_
         data bytes _must_ be zero for this optimization to take place.
      *)
      IF VAL(CARDINAL, buffer)#0
      THEN
         WriteString('byte at ') ;
         WriteHex(FileOffset+i, 4) ;
         WriteString(' is ') ; WriteHex(VAL(CARDINAL, buffer), 2) ;
         WriteString('expecting data to be zero as -Mi-d=bss flag used') ;
         WriteLn ;
         HALT(1)
      END
   ELSE
      IF NOT ok
      THEN
         Halt('failed to read data byte',
              __FILE__, __FUNCTION__, __LINE__)
      END ;
      buffer := checkEmitChecksum(i, lastAddress, checkSum, buffer) ;
      IF WriteNBytes(fo, SIZE(buffer), ADR(buffer)) # SIZE(buffer)
      THEN
         Halt('failed to write the correct number of bytes',
           __FILE__, __FUNCTION__, __LINE__)
      END ;
      IF Verbose
      THEN
         IF ((i+FileOffset) MOD 16) = 0
         THEN
            WriteLn ;
            WriteHex(FileOffset+i, 4)
         END ;
         Write(' ') ; WriteHex(VAL(CARDINAL, buffer), 2)
      END ;
      INC(checkSum, VAL(CARDINAL, buffer)) ;
      checkSum := checkSum MOD MaxCheckSum ;
   END ;
   RETURN checkSum
END collectData ;


(*
   handleVerbose - emits a Data message depending upon the -verbose
                   option.
*)

PROCEDURE handleVerbose (i: CARDINAL) ;
BEGIN
   IF Verbose
   THEN
      IF DataAsBss
      THEN
         WriteString('Data section is treated as bss...') ; WriteLn
      ELSE
         WriteString('Data...') ; WriteLn ;
         i := 16-(FileOffset MOD 16) ;
         IF i#16
         THEN
            WriteHex(FileOffset-i, 4) ;
            WHILE (i MOD 16)#0 DO
               WriteString('   ') ;
               INC(i)
            END
         END
      END
   END ;
END handleVerbose ;


(*
   ScanDataSegment - scans the input file for the data segment and
                     writes the data to the output file.
*)

PROCEDURE ScanDataSegment (checkSum: CARDINAL) : CARDINAL ;
VAR
   LastAddress,
   PhysSize,
   VirtSize  : CARDINAL;
   i         : CARDINAL ;
   Buffer    : BYTE ;
   ok        : BOOLEAN ;
BEGIN
(*
   IF FileCount=3
   THEN
      FileOffset := PadToStart(FileOffset, GetDataVirtualAddress())
   ELS...
*)
   IF FileCount<3
   THEN
      IF SeperateID
      THEN
         FileOffset := PadToNextSector(FileOffset)
      ELSE
         FileOffset := PadToNextClick(FileOffset)
      END
   END ;
   handleVerbose(i) ;
   IF FileCount=2
   THEN
      SecondData := FileOffset
   END ;
   PhysSize := GetDataPhysicalSize() ;
   VirtSize := GetDataVirtualSize() ;
   LastAddress := Max(PhysSize, VirtSize) ;
   ok := TRUE ;
   i := 0 ;
   WHILE (i<PhysSize) AND ok DO
      checkSum := collectData(fo, i, LastAddress, checkSum) ;
      INC(i)
   END ;
   Buffer := 0 ;
   WHILE (i<VirtSize) AND ok DO
      checkSum := collectData(fo, i, LastAddress, checkSum) ;
      INC(i)
   END ;
   IF NOT DataAsBss
   THEN
      INC(FileOffset, i)
   END ;
   IF SeperateID
   THEN
      FileOffset := PadToNextSector(FileOffset)
   ELSE
      FileOffset := PadToNextClick(FileOffset)
   END ;
   IF Verbose
   THEN
      WriteLn
   END ;
   RETURN( checkSum )
END ScanDataSegment ;


(*
   Init - parses arguments and then scans the a.out file.
*)

PROCEDURE Init ;
BEGIN
   StrCopy('', OutputName) ;
   Verbose := FALSE ;
   Debug := FALSE ;
   RunTimeGDB := 0 ;
   IsMagic := TRUE ;
   HeaderValue := 0 ;
   ParseArguments ;
   StackSize := DefaultStack ;
   IF StrEqual(OutputName, '')
   THEN
      WriteString('Usage: load [-g] -o <outputfile> [-v] { [-Mi+d | -Mi-d | -Md+i | -Md-i | -Mi=d | -checksum | -raw | -binary | -magic ] [ -header decimal# ] [ -stack #[M|K]] filei }') ;
      WriteLn
   ELSE
      DoForeachFile
   END
END Init ;


(*
   DoForeachFile - foreach file on the argument list scan it segments
      	       	   and write text and data to the output file.
*)

PROCEDURE DoForeachFile ;
VAR
   i          : CARDINAL ;
   CurSector  ,
   StartSector,
   EndSector  : CARDINAL ;
   Buffer     : BYTE ;
   checkSum   : CARDINAL ;
BEGIN
   fo := OpenForRandom(OutputName, TRUE, TRUE) ;
   SectorNo := 0 ;
   FileCount := 1 ;
   CurSector := 0 ;
   i := GetNthFile(1, InputName) ;
   WHILE i#0 DO
      FileOffset := 0 ;
      Buffer := 0 ;
      WHILE CurSector<SectorNo*SectorSize DO
      	 CurSector := PadToNextSector(CurSector) ;
      	 IF CurSector<SectorNo*SectorSize
      	 THEN
            IF WriteNBytes(fo, 1, ADR(Buffer)) # 1
            THEN
               Halt('failed to write the correct number of bytes',
                    __FILE__, __FUNCTION__, __LINE__)
            END ;
      	    INC(CurSector)
      	 END
      END ;
      IF FileCount=3
      THEN
      	 OSStart := CurSector
      END ;
      StartSector := CurSector ;
      IF Verbose
      THEN
      	 WriteString('File         : ') ; WriteString(InputName) ; WriteLn ;
      	 WriteString('Start sector :') ; WriteCard(StartSector DIV SectorSize, 4) ;
      	 WriteLn
      END ;
      checkSum := 0 ;
      IF IsMagic
      THEN
         IF Examine(InputName)
         THEN
            IF DebugThird AND (FileCount=3)
            THEN
               DebugFile
            END ;

            IF TextFirst
            THEN
               checkSum := ScanTextSegment(checkSum) ;
               checkSum := ScanDataSegment(checkSum)
            ELSE
               checkSum := ScanDataSegment(checkSum) ;
               checkSum := ScanTextSegment(checkSum)
            END
         ELSE
            WriteString('load failed to find binary file: ') ; WriteString(InputName) ; WriteLn ;
            HALT(1)
         END
      ELSE
         checkSum := ScanBinaryFile(InputName)
      END ;
      FileOffset := PadToNextSector(FileOffset) ;
      CurSector := CurSector + FileOffset ;
      EndSector := CurSector ;
      IF Verbose
      THEN
      	 WriteString('No of sectors:') ;
      	 WriteCard((EndSector-StartSector) DIV SectorSize, 4) ;
      	 WriteLn
      END ;
      IF (FileCount=1) AND (((EndSector-StartSector) DIV SectorSize) > 1)
      THEN
         WriteString('error - bootsector program exceeds 512 bytes') ;
         WriteLn ;
         HALT(1)
      ELSIF FileCount=2
      THEN
      	 WriteString('EndSector    :') ; WriteCard(EndSector, 4) ; WriteLn ;
      	 WriteString('StartSector  :') ; WriteCard(StartSector, 4) ; WriteLn ;
      	 SecondSize := EndSector-StartSector
      ELSIF FileCount=3
      THEN
      	 OSSize := EndSector-StartSector
      END ;
      IF needChecksum
      THEN
         WriteString('checksum byte for stage  ') ; WriteString(InputName) ; WriteString('  : ') ;
         WriteHex(checkSum, 2) ; WriteLn
      END ;
      i := GetNthFile(i+1, InputName) ;
      INC(FileCount)
   END ;
   WriteBuildInfo ;
   Close(fo)
END DoForeachFile ;


(*
   FixValue - place two bytes, sw, at position, o, in file, fo.
*)

PROCEDURE FixValue (o: LONGINT; sw: SHORTWORD) ;
BEGIN
   SetPositionFromBeginning(fo, o) ;
   IF WriteNBytes(fo, SIZE(sw), ADR(sw)) # SIZE(sw)
   THEN
      Halt('failed to write the correct number of bytes',
           __FILE__, __FUNCTION__, __LINE__)
   END
END FixValue ;


(*
   WriteBuildInfo - write out build information in correct places.
*)

PROCEDURE WriteBuildInfo ;
VAR
   OldPos: LONGINT ;
   sw    : SHORTWORD ;
BEGIN
   OldPos := FindPosition(fo) ;
   sw.ByteLo := VAL( BYTE, (StackSize DIV KiloByte) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (StackSize DIV KiloByte) DIV 0100H ) ;
   FixValue(StackSizeOffset, sw) ;

   sw.ByteLo := VAL( BYTE, RunTimeGDB MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, RunTimeGDB DIV 0100H ) ;
   FixValue(DebugGDBOffset, sw) ;

   sw.ByteLo := VAL( BYTE, (OSStart DIV SectorSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (OSStart DIV SectorSize) DIV 0100H ) ;
   FixValue(OSStartOffset, sw) ;

   sw.ByteLo := VAL( BYTE, (OSSize DIV SectorSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (OSSize DIV SectorSize) DIV 0100H ) ;
   FixValue(OSSizeOffset, sw) ;

   sw.ByteLo := VAL( BYTE, (SecondData DIV ClickSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (SecondData DIV ClickSize) DIV 0100H ) ;
   FixValue(SecondDataOffset, sw) ;

   sw.ByteLo := VAL( BYTE, (SecondSize DIV SectorSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (SecondSize DIV SectorSize) DIV 0100H ) ;
   FixValue(SecondSizeOffset, sw) ;

   sw.ByteLo := BYTE(055H) ;
   sw.ByteHi := BYTE(0AAH) ;
   FixValue(SignatureOffset, sw) ;

   SetPositionFromBeginning(fo, OldPos) ;   (* restore position *)

   IF DisplayBuild
   THEN
      WriteString('OS    Start: ') ; WriteCard(OSStart, 8) ;
      WriteString(' bytes ( Sector') ;
      WriteCard(OSStart DIV SectorSize, 4) ; WriteString(' )') ; WriteLn ;
      WriteString('OS     Size: ') ; WriteCard(OSSize, 8) ;
      WriteString(' bytes ( Sector') ;
      WriteCard(OSSize DIV SectorSize, 4) ; WriteString(' )') ; WriteLn ;
      WriteString('Second data: ') ; WriteCard(SecondData, 8) ;
      WriteString(' bytes ( Clicks') ; WriteCard(SecondData DIV 16, 4) ;
      WriteString(' )') ; WriteLn ;
      WriteString('Second size: ') ; WriteCard(SecondSize, 8) ;
      WriteString(' bytes ( Sector') ;
      WriteCard(SecondSize DIV SectorSize, 4) ; WriteString(' )') ; WriteLn ;
      WriteString('Stack size : ') ; WriteCard(StackSize, 8) ; WriteLn ;
      WriteString('GDB Debugging is ') ;
      IF RunTimeGDB=0
      THEN
         WriteString('off')
      ELSIF RunTimeGDB=1
      THEN
         WriteString('on')
      ELSE
         WriteCard(RunTimeGDB, 0)
      END ;
      WriteLn
   END ;
   IF SecondSize DIV SectorSize>DefaultMaxSecondSize
   THEN
      WriteString('error second size is too large for boot.S to load') ;
      WriteLn ;
      HALT(1)
   ELSE
      WriteLn ;
      WriteString('The secondary bootstage may be') ;
      WriteCard(DefaultMaxSecondSize * SectorSize - SecondSize, 8) ;
      WriteString(' bytes larger if required') ; WriteLn ;
      IF GetTextPhysicalSize() +
         GetDataPhysicalSize() +
         GetBssVirtualSize()   > MaxOSSize
      THEN
         WriteString('The operating system is ** too large ** by ') ;
         WriteCard((GetTextPhysicalSize() +
                   GetDataPhysicalSize()  +
                   GetBssVirtualSize())   - MaxOSSize, 8) ;
         WriteString(' bytes') ;
         WriteLn ;
         WriteString('This will *never* work - *NOT BUILDING A BOOTDISK*') ; WriteLn ;
         HALT(1)
      ELSE
         WriteString('Total operating system (text+data+bss) = ') ;
         WriteCard(GetTextPhysicalSize() +
                   GetDataPhysicalSize() +
                   GetBssVirtualSize(), 8) ; WriteLn ;
         WriteString('The operating system    may be') ;
         WriteCard(MaxOSSize - (GetTextPhysicalSize() +
                                GetDataPhysicalSize() +
                                GetBssVirtualSize()), 8) ;
         WriteString(' bytes larger if required') ; WriteLn
      END
   END
END WriteBuildInfo ;


(*
   PadToNextSector - writes nuls out until the beginning of the
                     next physical sector.
*)

PROCEDURE PadToNextSector (i: CARDINAL) : CARDINAL ;
VAR
   Buffer: BYTE ;
   j     : CARDINAL ;
BEGIN
   IF (i MOD SectorSize) # 0
   THEN
      IF Verbose
      THEN
         WriteLn ;
         WriteString('Pad to next sector') ;
         WriteLn ;
         WriteHex((i DIV 16) * 16, 4) ;
         j := 0 ;
         WHILE (i MOD 16) # j DO
            WriteString('   ') ;
            INC(j)
         END
      END ;
      Buffer := 0 ;
      WHILE (i MOD SectorSize) # 0 DO
         IF WriteNBytes(fo, 1, ADR(Buffer)) # 1
         THEN
            Halt('failed to write the correct number of bytes',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;

      	 IF Verbose
      	 THEN
            IF (i MOD 16) = 0
            THEN
      	       WriteLn ;
      	       WriteHex((i DIV 16) * 16, 4)
            END ;
            Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2)
      	 END ;
      	 INC(i)
      END ;
      IF Verbose
      THEN
         WriteLn
      END
   END ;
   RETURN( i )
END PadToNextSector ;


(*
   PadToNextClick - writes nuls out until the beginning of the
                    next click. It returns the new file offset.
*)

PROCEDURE PadToNextClick (i: CARDINAL) : CARDINAL ;
VAR
   Buffer: BYTE ;
   j     : CARDINAL ;
BEGIN
   IF (i MOD ClickSize) # 0
   THEN
      IF Verbose
      THEN
         WriteLn ;
         WriteString('Pad to next click') ;
         WriteLn ;
         WriteHex((i DIV 16) * 16, 4) ;
         j := 0 ;
         WHILE (i MOD 16) # j DO
            WriteString('   ') ;
            INC(j)
         END
      END ;
      Buffer := 0 ;
      WHILE (i MOD ClickSize) # 0 DO
         IF WriteNBytes(fo, 1, ADR(Buffer)) # 1
         THEN
            Halt('failed to write the correct number of bytes',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;

      	 IF Verbose
      	 THEN
            IF (i MOD 16) = 0
            THEN
      	       WriteLn ;
      	       WriteHex((i DIV 16) * 16, 4)
            END ;
            Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2)
      	 END ;
      	 INC(i)
      END ;
      IF Verbose
      THEN
         WriteLn
      END
   END ;
   RETURN( i )
END PadToNextClick ;


(*
   PadToStart -
*)

PROCEDURE PadToStart (i, Start: CARDINAL) : CARDINAL ;
VAR
   Buffer: BYTE ;
   j     : CARDINAL ;
BEGIN
   IF i<Start
   THEN
      IF Verbose
      THEN
         WriteLn ;
         WriteString('Pad to start..') ; WriteHex(Start, 8) ;
         WriteLn ;
         WriteHex((i DIV 16) * 16, 4) ;
         j := 0 ;
         WHILE (i MOD 16) # j DO
            WriteString('   ') ;
            INC(j)
         END
      END ;
      Buffer := 0 ;
      WHILE i<Start DO
         IF WriteNBytes(fo, 1, ADR(Buffer)) # 1
         THEN
            Halt('failed to write the correct number of bytes',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;
      	 IF Verbose
      	 THEN
            IF (i MOD 16) = 0
            THEN
      	       WriteLn ;
      	       WriteHex((i DIV 16) * 16, 4)
            END ;
            Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2)
      	 END ;
      	 INC(i)
      END ;
      IF Verbose
      THEN
         WriteLn
      END
   END ;
   RETURN( i )
END PadToStart ;


(*
   GetNthFile - returns the index of the current file name.
*)

PROCEDURE GetNthFile (i: CARDINAL; VAR a: ARRAY OF CHAR) : CARDINAL ;
VAR
   Argc: CARDINAL ;
BEGIN
   needChecksum := FALSE ;
   Argc := Narg() ;
   WHILE i<Argc DO
      IF GetArg(a, i)
      THEN
         IF StrEqual(a, '-o')
         THEN
            INC(i)
         ELSIF StrEqual(a, '-Mi+d')
         THEN
            SeperateID := FALSE ;
            TextFirst := TRUE ;
            DataAsBss := FALSE
         ELSIF StrEqual(a, '-Md+i')
         THEN
            SeperateID := FALSE ;
            TextFirst := FALSE ;
            DataAsBss := FALSE
         ELSIF StrEqual(a, '-Mi-d')
         THEN
            SeperateID := TRUE ;
            TextFirst := TRUE ;
            DataAsBss := FALSE
         ELSIF StrEqual(a, '-Md-i')
         THEN
            SeperateID := TRUE ;
      	    TextFirst := FALSE ;
            DataAsBss := FALSE
         ELSIF StrEqual(a, '-Mi-d=bss')
         THEN
            TextFirst := TRUE ;
            DataAsBss := TRUE
      	 ELSIF StrEqual(a, '-v') OR StrEqual(a, '-d') OR StrEqual(a, '-g')
         THEN
       	 ELSIF (a[0]='-') AND IsDigit(a[1])
      	 THEN
      	    a[0] := ' ' ;
      	    StrToCard(a, SectorNo)
(*
         ELSIF StrEqual(a, '-checksum')
         THEN
            (* checksum does not yet work *)
            needChecksum := TRUE
*)
         ELSIF StrEqual(a, '-raw') OR StrEqual(a, '-binary')
         THEN
            IsMagic := FALSE
         ELSIF StrEqual(a, '-magic')
         THEN
            IsMagic := TRUE
         ELSIF StrEqual(a, '-header') AND GetArg(a, i+1)
         THEN
            INC(i) ;
            StrToCard(a, HeaderValue)
         ELSIF StrEqual(a, '-stack') AND GetArg(a, i+1)
         THEN
            INC(i) ;
            IF IsDigit(a[0])
            THEN
               IF a[StrLen(a)-1]='M'
               THEN
                  a[StrLen(a)-1] := nul ;
                  StrToCard(a, StackSize) ;
                  StackSize := StackSize * MegaByte
               ELSIF a[StrLen(a)-1]='K'
               THEN
                  a[StrLen(a)-1] := nul ;
                  StrToCard(a, StackSize) ;
                  StackSize := StackSize * KiloByte
               ELSE
                  WriteString('-stack option needs number followed by K or M to signify units') ; WriteLn ;
                  HALT(1)
               END ;
               IF StackSize=0
               THEN
                  WriteString('a stack size of 0 is illegal') ; WriteLn ;
                  HALT(1)
               END
            ELSE
               WriteString('argument following -stack must be number[M|K]') ; WriteLn ;
               HALT(1)
            END
      	 ELSE
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
      IF StrEqual(a, '-o')
      THEN
      	 INC(i) ;
      	 IF NOT GetArg(OutputName, i)
      	 THEN
      	    WriteString('expecting output file name after -o option') ;
      	    WriteLn ;
      	    HALT
      	 END
      ELSIF StrEqual(a, '-v')
      THEN
      	 Verbose := TRUE
      ELSIF StrEqual(a, '-g')
      THEN
         RunTimeGDB := 1
         (*
            TODO: values greater than this might be useful sometime?
            ie comm2 etc

            Currently treat as a BOOLEAN   0=FALSE   1=TRUE
         *)
      ELSIF StrEqual(a, '-d')
      THEN
      	 Debug := TRUE
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
END load.
