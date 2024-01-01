MODULE load ;


FROM SYSTEM IMPORT ADR, TSIZE, SIZE, BYTE ;
FROM Args IMPORT GetArg, Narg ;
FROM NumberIO IMPORT WriteCard, WriteHex, StrToCard ;
FROM StrLib IMPORT StrEqual, StrCopy ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM FIO IMPORT OpenForRandom, File, ReadNBytes, Close, ReadChar,
                SetPosition, FindPosition, EndType,
      	       	WriteNBytes, OpenToWrite ;


TYPE
   SHORTWORD = RECORD
                  ByteLo, ByteHi: BYTE ;
      	       END ;

   LONGWORD = RECORD
                 WordLo: SHORTWORD ;
                 WordHi: SHORTWORD ;
      	      END ;

CONST
   XenixMagic = 00206H ;   (* indicates x.out header       *)
   SectorSize =   512  ;   (* Sector size of a floppy disk *)

TYPE
   (* executable header   x.out header *)
   xexec = RECORD
      	      Xmagic    : SHORTWORD ;   (* magic number *)
              Xext      : SHORTWORD ;   (* size of header extension *)
      	      Xtext     : LONGWORD ;    (* size of text segment (s) *)
      	      Xdata     : LONGWORD ;    (* size of initialized data (s) *)
              Xbss      : LONGWORD ;    (* size of uninitialized data (s) *)
              Xsyms     : LONGWORD ;    (* size of symbol table (s) *)
              Xreloc    : LONGWORD ;    (* relocation table length (s) *)
              Xentry    : LONGWORD ;    (* entry point, machine dependent *)
              Xcpu      : BYTE ;        (* cpu type & byte/word order *)
              Xrelsym   : BYTE ;        (* relocation & symbol format (u) *)
              Xrenv     : SHORTWORD ;   (* run-time environment *)
      	   END ;

   (* executable extension header *)
   xext = RECORD
             xetrsize  : LONGWORD ;   (* Unused *)
      	     xedrsize  : LONGWORD ;   (* Unused *)
      	     xetbase   : LONGWORD ;   (* Unused *)
      	     xedbase   : LONGWORD ;   (* Unused *)
             xestksize : LONGWORD ;   (* stack size, if XEFS set *)
      	     xesegpos  : LONGWORD ;   (* segment table position  *)
             xesegsize : LONGWORD ;   (* segment table size      *)
             xemdtpos  : LONGWORD ;   (* machine dependent table position *)
      	     xemdtsize : LONGWORD ;   (* machine dependent table size *)
      	     xemdttype : BYTE ;       (* machine dependent table type *)
             xepagesize: BYTE ;       (* file pagesize, in multiples of 512 *)
             xeostype  : BYTE ;       (* operating system type *)
             xeosvers  : BYTE ;       (* operating system version *)
             xeeseg    : SHORTWORD ;  (* entry segment, machine dependent *)
             xesres    : SHORTWORD ;  (* reserved *)
      	  END ;


CONST
   SegmentNull   = 0 ;   (* unused segment *)
   SegmentText   = 1 ;   (* text segment   *)
   SegmentData   = 2 ;   (* data segment   *)
   SegmentSyms   = 3 ;   (* symbol table segment *)
   SegmentReloc  = 4 ;   (* relocation segment *)
   SegmentString = 5 ;   (* segment table's string table segment *)
   SegmentGroups = 6 ;   (* group definitions segment *)

TYPE
   (* x.out segment table entry *)
   xseg = RECORD
             xstype  : SHORTWORD ;      (* segment type *)
             xsattr  : SHORTWORD ;      (* segment attributes *)
             xsseg   : SHORTWORD ;      (* segment number *)
             xsalign : BYTE ;           (* log base 2 of alignment *)
             xscres  : BYTE ;           (* unused *)
             xsfilpos: LONGWORD ;       (* file position *)
             xspsize : LONGWORD ;       (* physical size (in file) *)
             xsvsize : LONGWORD ;       (* virtual size (in core) *)
             xsrbase : LONGWORD ;       (* relocation base address/offset *)
             xsnoff  : SHORTWORD ;      (* segment name string table offset *)
             xssres  : SHORTWORD ;      (* unused *)
             xslres  : LONGWORD ;       (* unused *)
          END ;


CONST
   MaxLine    = 1024 ;
   ClickSize  =   16 ;      (* A 8086 segment register has granuality 16b    *)


VAR
   fi, fo    : File ;
   x         : xexec ;
   e         : xext ;
   InputName,
   OutputName: ARRAY [0..MaxLine] OF CHAR ;
   SeperateID: BOOLEAN ;   (* Seperate Instruction and Data memory model *)
   Verbose   : BOOLEAN ;
   Debug     : BOOLEAN ;
   FileOffset: CARDINAL ;
   SectorNo  : CARDINAL ;
   FileCount : CARDINAL ;
   SecondSize: CARDINAL ;   (* offset 254    I+D of secondary boot (sectors) *)
   SecondData: CARDINAL ;   (* offset 252      D of secondary boot (sectors) *)
   OSSize    : CARDINAL ;   (* offset 250    OS size               (sectors) *)
   OSStart   : CARDINAL ;   (* offset 248    OS start track on floppy of OS. *)
   TextFirst : BOOLEAN ;
   PassNo    : CARDINAL ;   (* Pass number over files *)



(*
   ScanXexec - fills in the exec header from a binary.
*)

PROCEDURE ScanXexec ;
BEGIN
   ReadNBytes( fi, TSIZE(xexec), ADR(x) ) ;
   IF Debug
   THEN
      WriteString('magic       ') ; WriteShort(x.Xmagic) ;
      IF ShortWordToCardinal(x.Xmagic)=XenixMagic
      THEN
      	 WriteString('  xenix magic number')
      END ;
      WriteLn ;
      WriteString('text length ') ; WriteLong(x.Xtext) ; WriteLn ;
      WriteString('bss  length ') ; WriteLong(x.Xbss) ; WriteLn ;
      WriteString('data length ') ; WriteLong(x.Xdata) ; WriteLn ;
      WriteString('entry (pc)  ') ; WriteLong(x.Xentry) ; WriteLn ;
      WriteString('extension   ') ; WriteShort(x.Xext) ; WriteLn ;
      WriteString('cpu type = ') ; WriteHex(VAL(CARDINAL, x.Xcpu), 2) ; WriteLn
   END ;
   IF ShortWordToCardinal(x.Xext)>0
   THEN
      ReadNBytes( fi, TSIZE(xext), ADR(e) ) ;
      IF Debug
      THEN
      	 WriteString('Extended header') ; WriteLn ;
      	 WriteString('entry segment      ') ; WriteShort(e.xeeseg) ; WriteLn ;
      	 WriteString('page size          ') ;
      	 WriteCard(VAL(CARDINAL, e.xepagesize) * 512, 0) ; WriteLn ;
      	 WriteString('segment table pos  ') ; WriteLong(e.xesegpos) ; WriteLn ;
      	 WriteString('segment table size ') ; WriteLong(e.xesegsize) ; WriteLn ;
      	 WriteString('header + ext = ') ; WriteCard(TSIZE(xexec) + TSIZE(xext), 4) ; WriteLn
      END ;
      SetPosition(fi, e.xesegpos, FromStart)
   END
END ScanXexec ;


(*
   ScanTextSegment - scans the input file for the text segment and
                     writes the text to the output file.
*)

PROCEDURE ScanTextSegment (FilePos: CARDINAL; PhysSize, VirtSize: CARDINAL;
      	       	     	   RelocationBase: CARDINAL) ;
VAR
   OldPos: CARDINAL ;
   i     : CARDINAL ;
   Buffer: BYTE ;
BEGIN
   IF (NOT SeperateID) OR (
                           (SeperateID AND (
                                            ((PassNo=1) AND TextFirst) OR
                                            ((PassNo=2) AND (NOT TextFirst))
                                           )
                           )
                          )
   THEN
      IF Verbose
      THEN
         WriteString('Text...') ; WriteLn
      END ;
      OldPos := FindPosition(fi) ;
      SetPosition(fi, FilePos, FromStart) ;
      i := 0 ;
      WHILE i<PhysSize DO
         ReadNBytes(fi, 1, ADR(Buffer)) ;
         IF Verbose
         THEN
            IF (i MOD 16) = 0
            THEN
               WriteLn ;
      	       WriteHex(RelocationBase+i, 4)
      	    END ;
      	    Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2) ;
         END ;
         WriteNBytes(fo, 1, ADR(Buffer)) ;
         INC(i)
      END ;
      Buffer := 0 ;
      WHILE i<VirtSize DO
         IF Verbose
         THEN
            IF (i MOD 16) = 0
            THEN
               WriteLn ;
      	       WriteHex(RelocationBase+i, 4)
      	    END ;
      	    Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2) ;
         END ;
         WriteNBytes(fo, 1, ADR(Buffer)) ;
         INC(i)
      END ;
      INC(FileOffset, i) ;
      IF SeperateID
      THEN
         FileOffset := PadToNextSector(FileOffset)
      END ;
      SetPosition(fi, OldPos, FromStart) ;
      IF Verbose
      THEN
         WriteLn
      END
   END
END ScanTextSegment ;


(*
   ScanDataSegment - scans the input file for the data segment and
                     writes the data to the output file.
*)

PROCEDURE ScanDataSegment (FilePos: CARDINAL; PhysSize, VirtSize: CARDINAL;
                           RelocationBase: CARDINAL) ;
VAR
   OldPos    : CARDINAL ;
   i         : CARDINAL ;
   DataSector: CARDINAL ;
   Buffer    : BYTE ;
BEGIN
   IF (NOT SeperateID) OR (
                           (SeperateID AND (
                                            ((PassNo=2) AND TextFirst) OR
                                            ((PassNo=1) AND (NOT TextFirst))
                                           )
                           )
                          )
   THEN
      IF SeperateID
      THEN
      	 FileOffset := PadToNextSector(FileOffset)
      END ;
      IF Verbose
      THEN
         WriteString('Data...') ; WriteLn
      END ;
      OldPos := FindPosition(fi) ;
      SetPosition(fi, FilePos, FromStart) ;
      DataSector := FileOffset ;
      IF FileCount=2
      THEN
         SecondData := FileOffset
      END ;
      i := 0 ;
      WHILE i<PhysSize DO
         IF Verbose
         THEN
            IF (i MOD 16) = 0
            THEN
               WriteLn ;
               WriteHex(RelocationBase+i, 4)
       	    END
         END ;
         ReadNBytes(fi, 1, ADR(Buffer)) ;
         IF Verbose
         THEN
            Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2)
         END ;
         WriteNBytes(fo, 1, ADR(Buffer)) ;
         INC(i)
      END ;
      Buffer := 0 ;
      WHILE i<VirtSize DO
         IF Verbose
         THEN
            IF (i MOD 16) = 0
            THEN
      	       WriteLn ;
      	       WriteHex(RelocationBase+i, 4)
            END ;
            Write(' ') ; WriteHex(VAL(CARDINAL, Buffer), 2)
         END ;
         WriteNBytes(fo, 1, ADR(Buffer)) ;
         INC(i)
      END ;
      INC(FileOffset, i) ;
      SetPosition(fi, OldPos, FromStart) ;
      IF Verbose
      THEN
         WriteLn
      END
   END
END ScanDataSegment ;


(*
   ScanSegment - scans a segment.
*)

PROCEDURE ScanSegment ;
VAR
   s: xseg ;
BEGIN
   ReadNBytes( fi, TSIZE(xseg), ADR(s) ) ;
   IF Debug
   THEN
      WriteString('segment type ') ; WriteShort(s.xstype)
   END ;
   CASE ShortWordToCardinal(s.xstype) OF

   SegmentNull:  WriteString('null segment') |
   SegmentText:  ScanTextSegment(LongWordToCardinal(s.xsfilpos),
      	       	     	      	 LongWordToCardinal(s.xspsize),
      	       	     	      	 LongWordToCardinal(s.xsvsize),
      	       	     	      	 LongWordToCardinal(s.xsrbase)) |
   SegmentData:  ScanDataSegment(LongWordToCardinal(s.xsfilpos),
      	       	     	      	 LongWordToCardinal(s.xspsize),
      	       	     	      	 LongWordToCardinal(s.xsvsize),
      	       	     	      	 LongWordToCardinal(s.xsrbase))
(*
   SegmentSyms:  WriteString('syms segment') |
   SegmentReloc: WriteString('reloc segment') |
   SegmentString: WriteString('string segment') |
   SegmentGroups: WriteString('groups segment')
*)

   ELSE
   END ;
   IF Debug
   THEN
      WriteLn ;
      WriteString('segment no.     ') ; WriteShort(s.xsseg) ; WriteLn ;
      WriteString('file pos        ') ; WriteLong(s.xsfilpos) ; WriteLn ;
      WriteString('phys size       ') ; WriteLong(s.xspsize) ; WriteLn ;
      WriteString('virt size       ') ; WriteLong(s.xsvsize) ; WriteLn ;
      WriteString('relocation base ') ; WriteLong(s.xsrbase) ; WriteLn
   END
END ScanSegment ;


(*
   ScanSegments - scans all the segments.
*)

PROCEDURE ScanSegments ;
VAR
   n, i: CARDINAL ;
BEGIN
   n := LongWordToCardinal(e.xesegsize) DIV TSIZE(xseg) ;
   i := 1 ;
   WHILE i<=n DO
      ScanSegment ;
      INC(i)
   END
END ScanSegments ;


(*
   ShortWordToCardinal - converts a short word into a cardinal.
*)

PROCEDURE ShortWordToCardinal (s: SHORTWORD) : CARDINAL ;
BEGIN
   RETURN( VAL(CARDINAL, s.ByteHi) * 0100H + VAL(CARDINAL, s.ByteLo) )
END ShortWordToCardinal ;


(*
   LongWordToCardinal - converts a short word into a cardinal.
*)

PROCEDURE LongWordToCardinal (l: LONGWORD) : CARDINAL ;
BEGIN
   RETURN(
           VAL(CARDINAL, l.WordLo.ByteHi) * 0100H +
           VAL(CARDINAL, l.WordLo.ByteLo)
      	 )
END LongWordToCardinal ;


(*
   WriteLong - writes the long out in hex characters.
*)

PROCEDURE WriteLong (l: LONGWORD) ;
BEGIN
   WriteHex(VAL(INTEGER, l.WordHi.ByteHi) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, l.WordHi.ByteLo) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, l.WordLo.ByteHi) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, l.WordLo.ByteLo) MOD 0100H, 2) ;
   WriteCard(VAL(INTEGER, l.WordLo.ByteHi) * 0100H +
      	     VAL(INTEGER, l.WordLo.ByteLo), 6) ;
END WriteLong ;


(*
   WriteShort - writes the short out in hex characters.
*)

PROCEDURE WriteShort (s: SHORTWORD) ;
BEGIN
   WriteHex(VAL(INTEGER, s.ByteHi) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, s.ByteLo) MOD 0100H, 2) ;
   WriteCard(VAL(INTEGER, s.ByteHi) * 0100H +
      	     VAL(INTEGER, s.ByteLo), 6) ;
END WriteShort ;


(*
   Init - parses arguments and then scans the a.out file.
*)

PROCEDURE Init ;
BEGIN
   StrCopy('', OutputName) ;
   Verbose := FALSE ;
   Debug := FALSE ;
   ParseArguments ;
   IF StrEqual(OutputName, '')
   THEN
      WriteString('Usage: load -o <outputfile> [-v] { [-Mi+d | -Mi-d] filei }') ;
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
BEGIN
   fo := OpenForRandom(OutputName, TRUE) ;
   SectorNo := 0 ;
   FileCount := 1 ;
   CurSector := 0 ;
   i := GetNthFile(1, InputName) ;
   WHILE i#0 DO
      Buffer := 0 ;
      WHILE CurSector<SectorNo*SectorSize DO
      	 CurSector := PadToNextSector(CurSector) ;
      	 IF CurSector<SectorNo*SectorSize
      	 THEN
      	    WriteNBytes(fo, 1, ADR(Buffer)) ;
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
      	 WriteString('File        : ') ; WriteString(InputName) ; WriteLn ;
      	 WriteString('Start sector:') ; WriteCard(StartSector DIV SectorSize, 4) ;
      	 WriteLn
      END ;
      FileOffset := 0 ;
      PassNo := 1 ;
      fi := OpenForRandom(InputName, FALSE) ;
      ScanXexec ;
      ScanSegments ;
      Close(fi) ;
      IF SeperateID
      THEN
         PassNo := 2 ;
         fi := OpenForRandom(InputName, FALSE) ;
         ScanXexec ;
         ScanSegments ;
         Close(fi)
      END ;
      FileOffset := PadToNextSector(FileOffset) ;
      CurSector := CurSector + FileOffset ;
      EndSector := CurSector ;
      IF Verbose
      THEN
      	 WriteString('No of sectors:') ;
      	 WriteCard((EndSector-StartSector) DIV SectorSize, 8) ;
      	 WriteLn
      END ;
      IF FileCount=2
      THEN
      	 WriteString('EndSector = ') ; WriteCard(EndSector, 4) ; WriteLn ;
      	 WriteString('StartSector = ') ; WriteCard(StartSector, 4) ; WriteLn ;
      	 SecondSize := EndSector-StartSector
      ELSIF FileCount=3
      THEN
      	 OSSize := EndSector-StartSector
      END ;
      i := GetNthFile(i+1, InputName) ;
      INC(FileCount)
   END ;
   WriteBuildInfo ;
   Close(fo)
END DoForeachFile ;


(*
   WriteBuildInfo - write out build information in correct places.
*)

PROCEDURE WriteBuildInfo ;
VAR
   OldPos: CARDINAL ;
   sw    : SHORTWORD ;
BEGIN
   OldPos := FindPosition(fo) ;
   sw.ByteLo := VAL( BYTE, (OSStart DIV SectorSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (OSStart DIV SectorSize) DIV 0100H ) ;
   SetPosition(fo, 248, FromStart) ;
   WriteNBytes(fo, SIZE(sw), ADR(sw)) ;

   sw.ByteLo := VAL( BYTE, (OSSize DIV SectorSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (OSSize DIV SectorSize) DIV 0100H ) ;
   SetPosition(fo, 250, FromStart) ;
   WriteNBytes(fo, SIZE(sw), ADR(sw)) ;

   sw.ByteLo := VAL( BYTE, (SecondData DIV ClickSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (SecondData DIV ClickSize) DIV 0100H ) ;
   SetPosition(fo, 252, FromStart) ;
   WriteNBytes(fo, SIZE(sw), ADR(sw)) ;

   sw.ByteLo := VAL( BYTE, (SecondSize DIV SectorSize) MOD 0100H ) ;
   sw.ByteHi := VAL( BYTE, (SecondSize DIV SectorSize) DIV 0100H ) ;
   SetPosition(fo, 254, FromStart) ;
   WriteNBytes(fo, SIZE(sw), ADR(sw)) ;

   SetPosition(fo, OldPos, FromStart) ;
   IF Verbose
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
      WriteLn ;
      WriteHex((i DIV 16) * 16, 4) ;
      j := 0 ;
      WHILE (i MOD 16) # j DO
         WriteString('   ') ;
      	 INC(j)
      END ;
      Buffer := 0 ;
      WHILE (i MOD SectorSize) # 0 DO
      	 WriteNBytes(fo, 1, ADR(Buffer)) ;
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
      WriteLn
   END ;
   RETURN( i )
END PadToNextSector ;


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
      	 IF StrEqual(a, '-o')
      	 THEN
      	    INC(i)
      	 ELSIF StrEqual(a, '-Mi+d')
      	 THEN
      	    SeperateID := FALSE
      	 ELSIF StrEqual(a, '-Mi-d')
      	 THEN
      	    SeperateID := TRUE ;
      	    TextFirst := TRUE
      	 ELSIF StrEqual(a, '-Md-i')
      	 THEN
      	    SeperateID := TRUE ;
      	    TextFirst := FALSE
      	 ELSIF StrEqual(a, '-v') OR StrEqual(a, '-d')
      	 THEN
      	 ELSIF (a[0]='-') AND IsDigit(a[1])
      	 THEN
      	    a[0] := ' ' ;
      	    StrToCard(a, SectorNo)
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
