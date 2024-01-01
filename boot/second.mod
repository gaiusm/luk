MODULE second ;


FROM ASCII IMPORT nul, cr, nl, bs ;
FROM go32 IMPORT CallOS ;

FROM minbios IMPORT WriteChar ;

FROM bios IMPORT GetGeometry, GetExtendedSize,
                 GetVideoMode, ReadTrack, Get16, Put16,
                 CursorOff, SetKeyboard,
                 OSSize, OSStart, SecondSize, SecondData,
                 DebuggingWithGDB, StackSize, DriveNo ;

FROM OSParameters IMPORT ExtendedMemAddr, VideoAddrAddr,
                         OSSizeAddr, NoOfSectorsAddr,
                         DebuggingAddr, StackSizeAddr ;

CONST
   BytesPerSector = 512 ;
   ClicksPerSector= BytesPerSector DIV 16 ;
   SecondSegment  = 090200H DIV 16;
   OSStartAddress = 010000H ;
   OSSegment      = OSStartAddress DIV 16 ;
   DEBUGTRACK     = FALSE ;
   DEBUGCONTENTS  = FALSE ;
   MaxHdCylinders = 1023 ;   (* do not alter this      0..1023 legal *)
   MaxHdSectors   =   63 ;   (* do not alter this      1..63   legal *)


(*
   PassOSParameters - places the values that are needed by the realtime
                      system into a safe place.
*)

PROCEDURE PassOSParameters (ExtendedMem, Video,
                            Size, NoOfSectors,
                            DebuggingWithGDB,
                            StackSize         : CARDINAL) ;
BEGIN
   Put16(ExtendedMemAddr DIV 010H, ExtendedMemAddr MOD 010H, ExtendedMem) ;
   Put16(VideoAddrAddr   DIV 010H, VideoAddrAddr   MOD 010H, Video) ;
   Put16(OSSizeAddr      DIV 010H, OSSizeAddr      MOD 010H, Size) ;
   Put16(NoOfSectorsAddr DIV 010H, NoOfSectorsAddr MOD 010H, NoOfSectors) ;
   Put16(DebuggingAddr   DIV 010H, DebuggingAddr   MOD 010H, DebuggingWithGDB) ;
   Put16(StackSizeAddr   DIV 010H, StackSizeAddr   MOD 010H, StackSize)
END PassOSParameters ;


(*
   WriteString - write a string to the display using the BIOS
*)

PROCEDURE WriteString (a: ARRAY OF CHAR) ;
VAR
   n    ,
   high : CARDINAL ;
BEGIN
   high := HIGH( a ) ;
   n := 0 ;
   WHILE (n <= high) AND (a[n] # nul) DO
      IF a[n]='\'
      THEN
         IF (n+1<=high) AND (a[n+1]='n')
         THEN
            WriteChar(nl) ;
            WriteChar(cr) ;
            INC(n)
         END
      ELSE
         WriteChar( a[n] )
      END ;
      INC( n )
   END
END WriteString ;


(*
   GetVideoDisplay - returns the memory mapped address of the video
                     memory.
*)

PROCEDURE GetVideoDisplay () : CARDINAL ;
BEGIN
   IF GetVideoMode() = 7
   THEN
      (* Mono screen *)
      WriteString('Non graphic video is not sufficient\n') ;
      RETURN( 0B0H )
   ELSE
      RETURN( 0B8H )
   END
END GetVideoDisplay ;


(*
   Wheel - the standard SUN boot wheel..
           which increments the spoke for each read.
*)

PROCEDURE Wheel (p: ARRAY OF CHAR; i: CARDINAL) ;
BEGIN
   WriteChar(p[i]) ;
   WriteChar(bs)
END Wheel ;


(*
   WriteCard - writes a CARDINAL value with, s, spaces.
*)

PROCEDURE WriteCard (n: CARDINAL; s: CARDINAL) ;
BEGIN
   WriteBase(n, s, 10)
END WriteCard ;


(*
   WriteBase - write a number in base, b.
*)

PROCEDURE WriteBase (n: CARDINAL; s: CARDINAL; b: CARDINAL) ;
VAR
   Buf: ARRAY [0..9] OF CARDINAL ;
   i  : CARDINAL ;
BEGIN
   i := 0;
   IF n#0
   THEN
      WHILE n#0 DO
         Buf[i] := n MOD b ;
         n := n DIV b ;
         INC(i)
      END
   ELSE
      Buf[i] := 0 ;
      INC(i)
   END ;
   WHILE s>i DO
      WriteChar(' ') ;
      DEC(s)
   END ;
   WHILE i>0 DO
      DEC(i) ;
      WriteHexDigit(Buf[i])
   END
END WriteBase ;


(*
   WriteHex - write a hex number with, s, spaces.
*)

PROCEDURE WriteHex (n: CARDINAL; s: CARDINAL) ;
BEGIN
   WriteBase(n, s, 16)
END WriteHex ;


(*
   WriteHexDigit - writes the hex digit, i.
*)

PROCEDURE WriteHexDigit (i: CARDINAL) ;
BEGIN
   IF i>9
   THEN
      WriteChar( CHR(i-10+ORD('A')) )
   ELSE
      WriteChar( CHR(i+ORD('0')) )
   END
END WriteHexDigit ;


(*
   stop - 
*)

PROCEDURE stop ;
BEGIN
   LOOP
   END
END stop ;


(*
   ReadSector - 
*)

PROCEDURE ReadSector (Cylinder, Sector, Head, Drive, Segment, Offset: CARDINAL) ;
VAR
   ok        : BOOLEAN ;
   val,
   NoOfErrors,
   CX, c     : CARDINAL ;
BEGIN
   IF DEBUGTRACK
   THEN
      WriteString('\ncyl:') ; WriteCard(Cylinder, 0) ;
      WriteString(' sec:') ; WriteCard(Sector, 0) ;
      WriteString(' hd:') ; WriteCard(Head, 0) ;
      WriteString(' seg:') ; WriteHex(Segment, 4) ;
      WriteString(' off:') ; WriteHex(Offset, 4)
   END ;
   NoOfErrors := 0 ;
   REPEAT
      c := (Cylinder DIV 0100H) MOD 4 ;
      CX := ((Cylinder MOD 0100H) * 0100H) + (c * 64) + Sector;

      ok := ReadTrack(Head, Drive, CX DIV 0100H, CX MOD 0100H, 1, Segment, Offset) ;
      IF NOT ok
      THEN
         INC(NoOfErrors)
      END
   UNTIL (NoOfErrors=3) OR ok ;
   IF NOT ok
   THEN
      WriteString('\nerror on cyl:') ; WriteCard(Cylinder, 0) ;
      WriteString(' sec:') ; WriteCard(Sector, 0) ;
      WriteString(' hd:') ; WriteCard(Head, 0) ;
      WriteString(' seg:') ; WriteHex(Segment, 4) ;
      WriteString(' off:') ; WriteHex(Offset, 4)
   END ;
   IF DEBUGCONTENTS
   THEN
      WriteString('\n -> ') ;
      DumpSeg(Segment, Offset, 16) ;
      WriteString('\n') ;
      val := Get16(Segment, Offset) MOD 0100H ;
      IF val#count
      THEN
         WriteString('failed to read ') ; WriteHex(count MOD 0100H, 2) ;
         WriteString(' after the ') ; WriteCard(count, 0) ; WriteString(' read\n') ;
         stop
      END
   END ;
   count := (count+1) MOD 0100H
END ReadSector ;


(*
   Min - returns the smallest of, a, or, b.
*)

PROCEDURE Min (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a<b
   THEN
      RETURN(a)
   ELSE
      RETURN(b)
   END
END Min ;


(*
   LoadOperatingSystem - loads in the operating system at
                         physical address OSSEGMENT*16.
			 It uses track reads whenever possible.
*)

PROCEDURE LoadOperatingSystem (StartSector      : CARDINAL;
                               NoOfSectorsToRead: INTEGER) ;
VAR
   Segment,
   Sector,
   Cylinder,
   Head, i    : CARDINAL ;
BEGIN
   i := 1 ;
   Segment := OSSegment ;
   Sector := (StartSector+1) MOD (GeoSectors+1) ;
   Cylinder := 0 ;
   Head := 0 ;

   WHILE NoOfSectorsToRead>0 DO
      Wheel('-\|/', i) ;
      i := (i+1) MOD 4;

      ReadSector(Cylinder, Sector, Head, DriveNo, Segment, 0) ;
      DEC(NoOfSectorsToRead) ;
      INC(Segment, ClicksPerSector) ;
      Sector := (Sector + 1) MOD (GeoSectors+1) ;
      IF Sector=0
      THEN
         INC(Sector) ;   (* values 1..GeoSectors are legal *)
         INC(Head) ;
(*
         IF Head>GeoHeads
         THEN
            Head := 0 ;
            INC(Cylinder)
         END
*)
      END
   END
END LoadOperatingSystem ;


(*
   DumpMem - 
*)

PROCEDURE DumpMem ;
CONST
   ParamMem = (640 * 1024 DIV 16) - 2 ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR j := 0 TO 2 DO
      FOR i := 0 TO 15 DO
         WriteHex(Get16(ParamMem+j, i) MOD 0100H, 2) ;
         WriteChar(' ')
      END ;
      WriteString('\n')
   END
END DumpMem ;


(*
   DumpSeg - 
*)

PROCEDURE DumpSeg (seg, offset: CARDINAL; bytes: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   IF bytes MOD 16#0
   THEN
      WriteString('bytes read _must_ be a multiple of 16\n') ;
   END ;
   FOR j := 0 TO (bytes DIV 16)-1 DO
      FOR i := 0 TO 15 DO
         WriteHex(Get16(seg+j, i) MOD 0100H, 2) ;
         WriteChar(' ')
      END ;
      WriteString('\n')
   END
END DumpSeg ;


VAR
   GeoSectors,
   GeoHeads,
   count     : CARDINAL ;

BEGIN
   WriteString('\nMicrokernel secondary boot activated\n') ;

   WriteString('\nloading third stage\n') ;
   WriteString('\nOSSize            ='); WriteHex(OSSize, 8); WriteCard(OSSize*BytesPerSector, 8); WriteString('k') ;

   WriteString('\nOSStart           ='); WriteHex(OSStart, 8);
   WriteString('\nSecondSize        ='); WriteHex(SecondSize, 8);
   WriteString('\nSecondData        ='); WriteHex(SecondData, 8);
   WriteString('\n');

   WriteString('\nExtended memory   ='); WriteCard(GetExtendedSize(), 4);
   WriteString(' k');
   WriteString('\nDrive no          ='); WriteCard(DriveNo, 4); WriteString('\n') ;
   GetGeometry(DriveNo, GeoHeads, GeoSectors);
   WriteString('\nSectors per track ='); WriteCard(GeoSectors, 4);
   WriteString('\nHeads             ='); WriteCard(GeoHeads, 4);
   WriteString('\nVideo mode        ='); WriteHex(GetVideoMode(), 4);
   WriteString('\nVideo display     ='); WriteHex(GetVideoDisplay(), 4);
   WriteString('\nDebugging with GDB='); WriteHex(DebuggingWithGDB, 4);
   WriteString('\nStack size        ='); WriteCard(StackSize, 4);
   WriteString(' k\n');

   count := 0 ;
   Wheel('-\|/', 0);
   PassOSParameters(GetExtendedSize(), GetVideoDisplay(),
                    OSSize, GeoSectors,
                    DebuggingWithGDB, StackSize) ;
   LoadOperatingSystem(OSStart, INTEGER(OSSize)) ;
   CursorOff ;
   SetKeyboard ;
   WriteString('before callOS...\n') ;
   DumpSeg(OSSegment, 0, 32) ;
   WriteString('loaded ') ; WriteCard(count * BytesPerSector, 0) ; WriteString('K bytes of text+data\n') ;
   CallOS
END second.
