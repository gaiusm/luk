/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/KeyBoardLEDs.mod" */


#include <p2c/p2c.h>


#define KeyBoardLEDsG
#include "GKeyBoardLEDs.h"

/* this module remains for compatability reasons only */

#ifndef TTIOH
#include "GTTIO.h"
#endif


Static void SwitchLeds(BOOLEAN NumLock, BOOLEAN CapsLock, BOOLEAN ScrollLock)
{
  TTIO_SwitchLeds(NumLock, CapsLock, ScrollLock);
}


Static void SwitchScroll(BOOLEAN Scroll)
{
  TTIO_SwitchScroll(Scroll);
}


Static void SwitchNum(BOOLEAN Num)
{
  TTIO_SwitchNum(Num);
}


Static void SwitchCaps(BOOLEAN Caps)
{
  TTIO_SwitchCaps(Caps);
}


void _M2_KeyBoardLEDs_init(void);

void _M2_KeyBoardLEDs_init(void)
{
}
void _M2_KeyBoardLEDs_fini(void);

void _M2_KeyBoardLEDs_fini(void)
{
}


/* End. */
