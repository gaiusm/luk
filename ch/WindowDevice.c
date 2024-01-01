/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/WindowDevice.mod" */


#include <p2c/p2c.h>


#define WindowDeviceG
#include "GWindowDevice.h"


#ifndef StorageH
#include "GStorage.h"
#endif

#ifndef ASCIIH
#include "GASCII.h"
#endif

#ifndef ColourTextH
#include "GColourText.h"
#endif

#ifndef ColoursH
#include "GColours.h"
#endif

#ifndef StrLibH
#include "GStrLib.h"
#endif

#ifndef NumberIOH
#include "GNumberIO.h"
#endif

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif


/*
   Title      : MonStrIO
   Author     : Gaius Mulley
   Date       : 16/8/87
   LastEdit   : 12/8/94
   System     : LOGITECH MODULA-2/86 & (RTS GM2)
   Description: Provides a method of writting text to a screen that is
                unbuffered and process independant.
*/


#ifndef MonStrIOH
#include "GMonStrIO.h"
#endif

#ifndef StdIOH
#include "GStdIO.h"
#endif


#define MaxWidth        79
#define MaxHeight       25
#define BoarderCol      (Colours_LightGrey * 0x10)
    /* LightGrey background; Black foreground */


/*
   The type Window is semi device specific. The parameters to control
   the window are NOT specific but the implementation of Window may
   change slightly.
*/

typedef enum {
  none, small, large
} CursorType;

typedef struct window {
  unsigned int Xoffset;   /* x position of bottom left text  */
  unsigned int Yoffset;   /* y position of bottom right text */
  unsigned int Width;   /* Width of text                   */
  unsigned int Height;   /* Height of text                  */
  unsigned int Xcursor;   /* x position of cursor in window  */
  unsigned int Ycursor;   /* y position of cursor in window  */
  unsigned int BgCol, FgCol, Attrib;   /* Attribute made from Bg and Fg   */
  struct window *Up;   /* Pointer to upwards visability   */
  struct window *Down;   /* Pointer to downwards visability */
  CursorType Cur;
  BOOLEAN Border;   /* Determines if a Border exists   */
  Char Display[MaxWidth + 1][MaxHeight + 1];
  Char Title[MaxWidth + 1];
} window;


Static window *Top;   /* Top Window Pointer */
Static window *Default;   /* The default window */


/*
   InitWindow - returns a Window handle. This Window is uninitialized.
*/

Static window *(InitWindow(void))
{
/* p2c: ../luk-1.0/mod/WindowDevice.mod:60:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int OldInterruptState;
  window *w;

  OldInterruptState = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/WindowDevice.mod:63:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/WindowDevice.mod:63:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  w = Malloc(sizeof(window));
  if (w == NULL)
    _OutMem();
  OldInterruptState = TurnInterrupts(OldInterruptState);
/* p2c: ../luk-1.0/mod/WindowDevice.mod:65:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  return w;
}


/*
   KillWindow - destroys a Window, w, and returns NIL.
*/

Static window *(KillWindow(window *w))
{
/* p2c: ../luk-1.0/mod/WindowDevice.mod:76:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int OldInterruptState;

  OldInterruptState = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/WindowDevice.mod:78:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/WindowDevice.mod:78:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  Free(w);
  w = NULL;
  OldInterruptState = TurnInterrupts(OldInterruptState);
/* p2c: ../luk-1.0/mod/WindowDevice.mod:81:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  return NULL;
}


/* p2c: ../luk-1.0/mod/WindowDevice.mod:100:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/WindowDevice.mod:100:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
/* Maybe - place into an attribute */
/* ok have done */
/* ../luk-1.0/mod/WindowDevice.mod, line 120: undefined symbol AddWindow */
/* Translation aborted. */
--------------------------
