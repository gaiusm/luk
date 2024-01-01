#define DebugC
#include "../../ch/luk-types.h"
#include "../../ch/Debug.h"
#include "../../ch/SYSTEM.h"
#include "../../ch/Scn.h"
#include "../../ch/ASCII.h"


#define Debug_MaxNoOfDigits          12   /* should be large enough for most source files.. */


/*
 *  WriteLn - writes a carriage return and a newline
 *            character.
 */

static void WriteLn (void)
{
  Scn_Write(ASCII_cr);
  Scn_Write(ASCII_lf);
}


/*
 *  DebugString - writes a string to the debugging device (Scn_Write).
 *                It interprets \n as carriage return, linefeed.
 *
 *                Notice that there are two parameters to DebugString
 *                the pointer to the start of the string and the maximum
 *                number of characters which maybe in the string.
 *
 *                All characters from the pointer up until the <nul>
 *                character (char)0 should be emitted.  No more than the
 *                maximum number of characters should be emitted, in case
 *                there is no trailing (char)0.
 */

void Debug_DebugString (char *a, unsigned int high)
{
  OnOrOff      OldInterruptState = SYSTEM_TurnInterrupts(Off);
  unsigned int n                 = 0;

  /* your code needs to go here */

  while ((n <= high) && (a[n] != ASCII_nul)) {      /* remove for student */
    if (a[n] == '\\') {                             /* remove for student */
      if (n+1 <= high) {                            /* remove for student */
	if (a[n+1] == 'n') {                        /* remove for student */
	  WriteLn();                                /* remove for student */
	  n++;                                      /* remove for student */
	} else if (a[n+1] == '\\') {                /* remove for student */
	  Scn_Write('\\');                          /* remove for student */
	  n++;                                      /* remove for student */
	}                                           /* remove for student */
      }                                             /* remove for student */
    } else {                                        /* remove for student */
      Scn_Write(a[n]);                              /* remove for student */
    }                                               /* remove for student */
    n++;                                            /* remove for student */
  }                                                 /* remove for student */
  OldInterruptState = SYSTEM_TurnInterrupts(OldInterruptState);
}


/*
 *  simple utility function
 */

static int strlen (char *s)
{
  int i=0;

  while (s[i] != (char)0) {
    i++;
  }
  return i;
}


/*
   DebugCString - writes a string to the debugging device (Scn.Write).
                  It interprets \n as carriage return, linefeed.
*/

void Debug_DebugCString (char *a)
{
  Debug_DebugString(a, strlen(a));
}


/*
 *  Halt - writes a message in the format:
 *         filename:line:message
 *
 *         to the debugging device. (Scn.Write).
 *         It then terminates by looping forever.
 */

void Debug_Halt (char *message, unsigned int high_message,
		 unsigned int line_no,
		 char *filename, unsigned int high_filename)
{
  char     StrNo[Debug_MaxNoOfDigits];
  OnOrOff  OldInterruptState;

  OldInterruptState = SYSTEM_TurnInterrupts(Off) ;
  Debug_DebugString(filename, high_filename) ;
  
  NumberIO_CardToStr(line_no, 0, StrNo, Debug_MaxNoOfDigits-1) ;
  Debug_DebugString(":", 1) ;
  Debug_DebugString(StrNo, Debug_MaxNoOfDigits-1) ;
  Debug_DebugString(":", 1) ;
  Debug_DebugString(message, high_message) ;
  Debug_DebugString("\n", 1) ;
  while (TRUE) {
    /* now terminate, by spinning forever */
  }
}


/* linker fodder - leave alone */

_M2_Debug_init () {}
_M2_Debug_finish () {}
