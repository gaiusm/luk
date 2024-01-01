

#if defined(HOST)
#   define WriteChar    putchar
#else
    extern void         WriteChar (char ch);
    extern int          GetSectorsPerTrack (void) ;
    extern int          ReadTrack (int Head, int Drive, int Track, int Sector,
				   int NoOfSectors, int SegAddr, int OffsetAddr);
    extern void         CallOS (void);
    extern unsigned int Get16  (int segment, int offset);
    extern int          GetTest(int sector, int head);
    extern void         InitComm1 (void);
    extern void         WriteComm1 (char ch);
    extern int          GetVideoMode (void);
#endif


void        Second();
void        WriteString (char *p);
void        WriteLn (void);
static void WriteHexDigit (int i);
void        WriteHex (unsigned int n, int spaces);
static void Wheel (char *p, int i);
void        WriteCard (unsigned int n, int spaces);
static void LoadOperatingSystem (int StartSector, int NoOfSectors);
static int  DebugReadTrack (int Head, int Drive, int Track, int Sector,
			    int NoOfSectors, int SegAddr, int OffsetAddr);
static void DumpSegment (int s);


#define CR   13
#define LF   10
#define BS    8

#define OSSTARTADDR     (0x10000)             /* OS lives at 0x10000 upwards */
#define OSSEGMENT       (OSSTARTADDR / 16)
#define BYTESPERSECTOR  (0x200)               /* bytes in a sector */


#if defined(DEBUG)
#   define WriteChar   WriteComm1             /* all debugging to comm 1     */
#endif


int OSSize, OSStart;
int SecondSize, SecondData;
int NoOfSectorsPerTrack;
int VideoPage;


#if defined(HOST)
main()
{
  Second();
}
#endif


/*
   GetVideoDisplay - returns the memory mapped address of the video
                     memory.
*/

static int GetVideoDisplay (void)
{
  if (GetVideoMode() == 7) {
    /* Mono screen */
    WriteString("Non graphic video is not sufficient\n");
    return( 0xb0 );
  } else {
    return( 0xb8 );
  }
}


/*
   Second - the second boot phase.
            Sets the system parameters and then loads in the operating system.
*/

void Second()
{
#if defined(DEBUG)
  InitComm1();   /* initialize the serial port */
#endif

  WriteString("\nBooting Operating System\n");
  WriteString("\nOSSize          ="); WriteHex(OSSize, 8);
  WriteString("\nOSStart         ="); WriteHex(OSStart, 8);
  WriteString("\nSecondSize      ="); WriteHex(SecondSize, 8);
  WriteString("\nSecondData      ="); WriteHex(SecondData, 8);
  WriteString("\n");
  WriteString("\nExtended memory ="); WriteCard(GetExtendedSize(), 4);
  WriteString(" k\n");
  NoOfSectorsPerTrack = GetSectorsPerTrack();
  WriteString("Sectors per track ="); WriteCard(NoOfSectorsPerTrack, 4);
  WriteString("\nVideo mode      ="); WriteHex(GetVideoMode(), 4);
  WriteString("\nVideo display   ="); WriteHex(GetVideoDisplay(), 4);
  WriteString("\n");
  Wheel("-\\|/", 0);
  LoadOperatingSystem(OSStart, OSSize);
  KillMotor();
  CallOS();
}


/*
   DumpSegment - dumps the segment, s, in hex to the display.
*/

static void DumpSegment (int s)
{
  int si, oi;
  unsigned int w;

  si = s;
  WriteHex(si, 4); WriteString("0  ");
  while (si < s+0x1) {
    oi = 0 ;
    while (oi < 0x10) {
      w = Get16(si, oi);
      WriteHex(w % 0x100, 3);
      WriteHex(w / 0x100, 3);
      oi = oi+2;
    }
    si++;
    WriteString("\n");
    if (si < s+0x1) {
      WriteHex(si, 4);
      WriteString("0  ");
    }
  }
}


/*
   LoadOperatingSystem - loads in the operating system at
                         physical address OSSEGMENT*16.
			 It uses track reads whenever possible.
*/

static void LoadOperatingSystem (int StartSector, int NoOfSectors)
{
  int Segment;
  int CurSector, CurTrack;
  int SectorsLeft;
  int Head;
  int i;

  i = 1;
  Head = 0;
  Segment = OSSEGMENT;
  CurTrack = StartSector/NoOfSectorsPerTrack;
  CurSector = StartSector % NoOfSectorsPerTrack;

  while (NoOfSectors>0) {
    Wheel("-\\|/", i);
    i = (i+1) % 4;
    if (CurSector == 0) {
      if (! DebugReadTrack(Head, 0, CurTrack, CurSector+1, NoOfSectorsPerTrack, Segment, 0)) {
	WriteString("error loading track"); WriteCard(CurTrack, 4); WriteLn;
      }
      Head = 1-Head;
      if (Head == 0) {
	CurTrack++;
      }
      Segment += (NoOfSectorsPerTrack * (BYTESPERSECTOR/16));
      NoOfSectors -= NoOfSectorsPerTrack;
    } else {
      SectorsLeft = NoOfSectorsPerTrack-CurSector;
      if (! DebugReadTrack(Head, 0, CurTrack, CurSector+1, SectorsLeft, Segment, 0)) {
	WriteString("error trying to load sectors") ; WriteCard(CurSector, 4);
	WriteString(".."); WriteCard(NoOfSectorsPerTrack-1, 4);
	WriteString(" of track"); WriteCard(CurTrack, 4); WriteLn;
      }
      return;
      Head = 1-Head;
      NoOfSectors -= SectorsLeft;
      Segment += SectorsLeft * (BYTESPERSECTOR/16);
      if (Head == 0) {
	CurTrack++;
      }
      CurSector = 0;
    }
  }
}


static int DebugReadTrack (int Head, int Drive, int Track, int Sector,
			   int NoOfSectors, int SegAddr, int OffsetAddr)
{
#if defined(DEBUG)
  WriteString("ReadTrack (seg "); WriteHex(SegAddr, 5);
  WriteString("H  track"); WriteCard(Track, 4);
  WriteString("   sector"); WriteCard(Sector, 4);
  WriteString("   head"); WriteCard(Head, 4);
  WriteString("   amount"); WriteCard(NoOfSectors, 4);
  WriteString(")\n");
#endif
  return( ReadTrack(Head, Drive, Track, Sector, NoOfSectors, SegAddr, OffsetAddr) );
}


void Wheel (char *p, int i)
{
  WriteChar(p[i]);
  WriteChar(BS);
}


void WriteLn (void)
{
#if !defined(HOST)
  WriteChar((char) CR);
#endif
  WriteChar((char) LF);
}


void WriteString (char *p)
{
  int i=0;

  while (p[i] != (char)0) {
    if (p[i] == '\n') {
      WriteLn();
    } else {
      WriteChar(p[i]);
    }
    i++;
  }
}


void WriteCard (unsigned int n, int spaces)
{
  int Buf[10];
  int i, j;

  i = 0;
  if (n != 0) {
    while (n != 0) {
      Buf[i] = n % 10;
      n = n / 10;
      i++;
    }
  } else {
    Buf[i] = 0;
    i++;
  }
  while (spaces > i) {
    WriteChar(' ');
    spaces--;
  }
  while (i > 0) {
    i--;
    WriteHexDigit(Buf[i]);
  }
}


void WriteHex (unsigned int n, int spaces)
{
  int Buf[10];
  int i, j;

  i = 0;
  if (n != 0) {
    while (n != 0) {
      Buf[i] = n % 16;
      n = n / 16;
      i++;
    }
  } else {
    Buf[i] = 0;
    i++;
  }
  while (spaces > i) {
    WriteChar(' ');
    spaces--;
  }
  while (i > 0) {
    i--;
    WriteHexDigit(Buf[i]);
  }
}



static void WriteHexDigit (int i)
{
  if (i>9) {
    WriteChar( (char) (i-10+'a') );
  } else {
    WriteChar( (char) (i+'0') );
  }
}
