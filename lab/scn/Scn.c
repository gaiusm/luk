
#define ScnC

#include "../../ch/luk-types.h"
#include "../../ch/Scn.h"
#include "../../ch/SYSTEM.h"
#include "../../ch/ASCII.h"
#include "../../ch/osinit.h"
#include "../../ch/PortIO.h"



#define Width     80
#define Height    25
#define underline (char)(64+48)
#define normal    (char)(7)


typedef struct DisplayUnit {
  char ch /* __attribute__ ((packed)) */ ;
  char attrib /* __attribute__ ((packed)) */ ;
} DisplayUnit;

typedef DisplayUnit Line[Width];
typedef Line Screen[Height];

static unsigned int xcur, ycur;
static Screen *screen;


/*
 *  prototypes
 */
static void incy (void);
static void incx (void);
static void ScrollUp (void);
static void BlankBottomLine (void);
static void AppendLine (unsigned int y);
static void DeleteLine (unsigned int y);
static void OnSound (unsigned int tone);
static void OffSound (void);
static void Delay (unsigned int maxI, unsigned int maxJ);



void Scn_Write (char ch)
{
  OnOrOff ToOldState = SYSTEM_TurnInterrupts (Off);

  (*screen)[ycur][xcur].attrib = normal;
  if (ch == ASCII_cr)
    xcur = 0;
  else if (ch == ASCII_lf)
    incy();
  else if (ch == ASCII_bs) {
    if (xcur > 0)
      xcur--;
    else if (ycur > 0) {
      ycur--;
      xcur = Width-1;
    }
  }
  else if (ch >= ' ') {
    (*screen)[ycur][xcur].ch = ch ;
    incx();
  }
  (*screen)[ycur][xcur].attrib = underline;
  ToOldState = SYSTEM_TurnInterrupts (ToOldState);
}


static void incy (void)
{
  if (ycur == Height-1)
    ScrollUp();
  else
    ycur++;
}


static void incx (void)
{
  if (xcur == Width-1) {
    incy();
    xcur = 0;
  }
  else
    xcur++;
}


/*
 *  ScrollUp - moves each line up the screen and then finally blanks
 *             the bottom line.
 */

static void ScrollUp (void)
{
  unsigned int i, j;

  for (j = 0; j<=Height-2; j++)              /* remove for student */
    for (i = 0; i<=Width-1; i++)             /* remove for student */
      (*screen)[j][i] = (*screen)[j+1][i];   /* remove for student */
  BlankBottomLine();                         /* remove for student */
}


static void BlankBottomLine (void)
{
  Scn_ClearLine(Height-1);
}


/*
 *  ClearLine - clears line, y, filling it with spaces.
 */

void Scn_ClearLine (unsigned int y)
{
  OnOrOff ToOldState = SYSTEM_TurnInterrupts (Off);
  unsigned int i;

  for (i=0; i<=Width-1; i++) {
    (*screen)[y][i].ch = ' ' ;
    (*screen)[y][i].attrib = normal;
  }
  ToOldState = SYSTEM_TurnInterrupts (ToOldState);
}


/*
 *  AppendLine - adds another line after, y, to the display, all the
 *               other lines move down one line.
 */

static void AppendLine (unsigned int y)
{
  OnOrOff ToOldState = SYSTEM_TurnInterrupts (Off);
  unsigned int bottomLine = Height-1;
  unsigned int i;

  while (bottomLine-1>y) {
    for (i=0; i<=Width-1; i++)
      (*screen)[bottomLine][i] = (*screen)[bottomLine-1][i];
    bottomLine--;
  }
  ToOldState = SYSTEM_TurnInterrupts (ToOldState);
}


/*
 *  DeleteLine - deletes line, y, from the screen, the lower lines
 *               move up to fill in the gap.
 */

static void DeleteLine (unsigned int y)
{
  OnOrOff ToOldState = SYSTEM_TurnInterrupts (Off);
  unsigned int i;

  while (y<Height-1) {
    for (i=0; i<=Width-1; i++)
      (*screen)[y][i] = (*screen)[y+1][i];
    y++;
  }
  BlankBottomLine() ;
  ToOldState = SYSTEM_TurnInterrupts (ToOldState);
}


/*
 *  MoveCursor - moves the cursor to, x, y.
 *               0, 0 is the top left corner.
 */

void Scn_MoveCursor (unsigned int x, unsigned int y)
{
  OnOrOff ToOldState = SYSTEM_TurnInterrupts (Off);

  (*screen)[ycur][xcur].attrib = normal ;
  xcur = x;
  ycur = y;
  (*screen)[ycur][xcur].attrib = underline ;
  ToOldState = SYSTEM_TurnInterrupts (ToOldState);
}
   

void Scn_ClearScreen (void)
{
  OnOrOff ToOldState = SYSTEM_TurnInterrupts (Off);
  unsigned int i, j;

  for (j=0; j<=Height-1; j++) {
    for (i-0; i<=Width-1; i++) {
      (*screen)[j][i].ch = ' ';
      (*screen)[j][i].attrib = normal;
    }
  }
  ToOldState = SYSTEM_TurnInterrupts (ToOldState);
}


/*
 *  OnSound - 
 */

static void OnSound (unsigned int tone)
{
 unsigned char b;

 PortIO_Out8(0x43, 0xb6);              /* tell timer prepare for new sound */
 PortIO_Out8(0x42, tone % 0x100);      /* new tone to timer LSB */
 PortIO_Out8(0x42, tone / 0x100);      /* LSB -> timer          */
 b = PortIO_In8(0x61);                 /* enable speaker via time */
 b |= 3;                               /* b = b + {0, 1}        */
 PortIO_Out8(0x61, b);
}


/*
 *  OffSound - 
 */

static void OffSound (void)
{
  unsigned char b;

  b = PortIO_In8(0x61);          /* disable speaker    */
  b &= 0xfc;                     /* remove bits 0, 1   */
  PortIO_Out8(0x61, b);          /* output via a timer */
}


/*
 *  Delay - 
 */

static void Delay (unsigned int maxI, unsigned int maxJ)
{
  unsigned int i, j, x;

  x = 1;
  for (i = 0; i<maxI; i++)
    for (j = 0; j<maxJ; j++)
      x += 1;  /* something for the processor to do, so gcc */
	       /* does not easily through it away */
}


/*
 *  Scn_Bell - turn the PC speaker on and then off again.
 */

void Scn_Bell (void)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off) ;
  OnSound(500);
  Delay(10, 400);
  OffSound();
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);
}


/*
 *  Scn_Init - initialize the data structures.
 */

void Scn_Init (void)
{
   screen = osinit_ScreenAddress();
   xcur = 0;
   ycur = 0;
   Scn_MoveCursor(0, 24);
   Scn_Bell();
}


void _M2_Scn_init (void)
{
  Scn_Init();
}

void _M2_Scn_finish (void) {}
