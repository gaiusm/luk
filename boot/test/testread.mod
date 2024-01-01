MODULE testread ;


FROM ASCII IMPORT nul, cr, nl, bs ;
FROM StdIO IMPORT Write ;

CONST
   BytesPerSector = 512 ;
   OSStartAddress = 010000H ;
   OSSegment      = OSStartAddress DIV 16 ;


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
            Write(nl) ;
            Write(cr) ;
            INC(n)
         END
      ELSE
         Write( a[n] )
      END ;
      INC( n )
   END
END WriteString ;


(*
   Wheel - the standard SUN boot wheel..
*)

PROCEDURE Wheel (p: ARRAY OF CHAR; i: CARDINAL) ;
BEGIN
   Write(p[i]) ;
   Write(bs)
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
   Buf : ARRAY [0..9] OF CARDINAL ;
   i, j: CARDINAL ;
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
      Write(' ') ;
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
      Write( CHR(i-10+ORD('a')) )
   ELSE
      Write( CHR(i+ORD('0')) )
   END
END WriteHexDigit ;


(*
   LoadOperatingSystem - loads in the operating system at
                         physical address OSSEGMENT*16.
			 It uses track reads whenever possible.
*)

PROCEDURE LoadOperatingSystem (StartSector: CARDINAL;
                               NoOfSectors: INTEGER) ;
VAR
   Segment,
   CurSector,
   CurTrack,
   SectorsLeft,
   Head, i    : CARDINAL ;
BEGIN
   i := 1 ;
   Head := 0 ;
   Segment := OSSegment ;
   CurTrack := StartSector DIV NoOfSectorsPerTrack ;
   CurSector := StartSector MOD NoOfSectorsPerTrack ;

   WHILE NoOfSectors>0 DO
      Wheel('-\|/', i) ;
      i := (i+1) MOD 4;
      IF CurSector=0
      THEN
         IF NOT ReadTrack(Head, 0, CurTrack, CurSector+1,
                          NoOfSectorsPerTrack, Segment, 0)
         THEN
            WriteString('error loading track') ; WriteCard(CurTrack, 4) ; WriteString('\n')
         END ;
         Head := 1-Head;
         IF Head=0
         THEN
            INC(CurTrack)
         END ;
         INC(Segment, (NoOfSectorsPerTrack * (BytesPerSector DIV 16)) ) ;
         DEC(NoOfSectors, NoOfSectorsPerTrack)
      ELSE
         SectorsLeft := NoOfSectorsPerTrack-CurSector ;
         IF NOT ReadTrack(Head, 0, CurTrack, CurSector+1,
                          SectorsLeft, Segment, 0)
         THEN
            WriteString('error trying to load sectors') ; WriteCard(CurSector, 4) ;
            WriteString('..') ; WriteCard(NoOfSectorsPerTrack-1, 4) ;
            WriteString(' of track') ; WriteCard(CurTrack, 4); WriteString('\n')
         END ;
         Head := 1-Head ;
         DEC(NoOfSectors, SectorsLeft) ;
         INC(Segment, SectorsLeft * (BytesPerSector DIV 16)) ;
         IF Head=0
         THEN
            INC(CurTrack)
         END ;
         CurSector := 0
      END
   END
END LoadOperatingSystem ;


(*
   ReadTrack - 
*)

PROCEDURE ReadTrack (Head, Drive, Track, Sector,
                     NoOfSectors, SegAddr, OffsetAddr: CARDINAL) : BOOLEAN ;
BEGIN
   WriteString('ReadTrack (Head = ') ; WriteCard(Head, 0) ;
   WriteString('; Drive = ') ; WriteCard(Drive, 0) ;
   WriteString('; Track = ') ; WriteCard(Track, 0) ;
   WriteString('; Sector = ') ; WriteCard(Sector, 0) ;
   WriteString('; NoOfSectors = ') ; WriteCard(NoOfSectors, 0) ;
   WriteString('; SegAddr = ') ; WriteCard(SegAddr, 0) ;
   WriteString('; OffsetAddr = ') ; WriteCard(OffsetAddr, 0) ;
   WriteString(')\n') ;
   RETURN( TRUE )
END ReadTrack ;



VAR
   OSSize, OSStart    ,
   NoOfSectorsPerTrack: CARDINAL ;
BEGIN
   WriteString('\nReal time system secondary boot activated\n') ;

   WriteString('\nBooting Operating System\n') ;

   OSSize := 0BH ;
   OSStart := 0BH ;

   WriteString('\nOSSize            ='); WriteHex(OSSize, 8);

   WriteString('\nOSStart           ='); WriteHex(OSStart, 8);
   WriteString('\n');
   NoOfSectorsPerTrack := 15 ;
   WriteString('\nSectors per track ='); WriteCard(NoOfSectorsPerTrack, 4);
   WriteString('\n');
   Wheel('-\|/', 0);
   LoadOperatingSystem(OSStart, INTEGER(OSSize)) ;
(*
   CallOS();
*)
END testread.
