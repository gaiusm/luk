/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/KeyBoardConvert.mod" */


#include <p2c/p2c.h>


#define KeyBoardConvertG
#include "GKeyBoardConvert.h"


#ifndef ASCIIH
#include "GASCII.h"
#endif

#ifndef NonAsciiH
#include "GNonAscii.h"
#endif

#ifndef KeyBoardLEDsH
#include "GKeyBoardLEDs.h"
#endif

/* FROM SysError IMPORT Reset ; */
#ifndef StrLibH
#include "GStrLib.h"
#endif



#define MaxFunctionDef  30   /* MaxFunctionKey definitions      */
#define MaxFunctionKey  10   /* MaxFunctionKey keys recognised  */
#define StartFunctionScanCode  59
#define EndFunctionScanCode  72   /* 68 for PC : 72 for AT keyboard  */
#define MaxStringSize   40

#define EscapeCharacter  '\\'


Static Char Bc[84], Uc[84], Cc[84], Ac[84];
Static Char Nc[13];

Static BOOLEAN Shift1, Shift2, NumLock, CapsLock, Ctrl, Alt;
Static Char FunctionString[MaxFunctionDef][MaxStringSize + 1];

/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:35:
 * Warning: Symbol 'ConsoleSwitchProc' is not defined [221] */
Static int ConsoleSelect;
/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:38: 
 * Warning: Symbol 'ProcWrite' is not defined [221] */


Static void ScanToASCII(int ReadDeliver, Char ch, int ConsoleSwitch)
{
  BOOLEAN KeyDown;
  unsigned int Ord;

/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:81:
 * Warning: Expected END, found ELSIF [227] */
  ConsoleSelect = ConsoleSwitch;
  Ord = ch;
  KeyDown = (Ord < 128);
  if (!KeyDown) {
    Ord -= 128;
    ch = Ord;
  }

  switch (Ord) {

  case 29:
    Ctrl = KeyDown;
    break;

  case 42:
    Shift1 = KeyDown;
    break;

  case 54:
    Shift2 = KeyDown;
    break;

  case 56:
    Alt = KeyDown;
    break;

  case 58:
    if (KeyDown) {
      CapsLock = !CapsLock;
      KeyBoardLEDs_SwitchCaps(CapsLock);
    }
    break;

  case 69:
    if (KeyDown) {
      NumLock = !NumLock;
      KeyBoardLEDs_SwitchNum(NumLock);
    }
    break;

  case 70:
    break;
    /* Reset */

    /* Not KeyBoard Full */
/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:77:
 * Warning: Expected END, found a '(' [227] */
/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:77:
 * Warning: Expected END, found a '(' [227] */
/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:77:
 * Warning: Expected END, found a '(' [227] */
/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:77:
 * Warning: Expected END, found a '(' [227] */
  }
}


/*
         IF ch<>377C
         THEN
            IF ch=nul
            THEN
               ReadDeliver( ch ) ;
               ch := CHR( Ord )
            END ;
            ReadDeliver( ch )
         END
*/
void _M2_KeyBoardConvert_init(void);

void _M2_KeyBoardConvert_init(void)
{
}
void _M2_KeyBoardConvert_fini(void);

void _M2_KeyBoardConvert_fini(void)
{
}
/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:117: 
 * Warning: Expected a '.', found END [227] */
/* p2c: ../luk-1.0/mod/KeyBoardConvert.mod:117: 
 * Warning: Junk at end of input file ignored [277] */


/* End. */
