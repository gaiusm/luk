/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/ColourText.mod" */


#include <p2c/p2c.h>


#define ColourTextG
#include "GColourText.h"


#ifndef osinitH
#include "Gosinit.h"
#endif

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif


#define Width           80
#define Height          25

#define underline       'p'
#define normal          '\007'


typedef struct DisplayUnit {
  Char char_, attrib;
} DisplayUnit;

typedef DisplayUnit Line[Width];
typedef Line Screen[Height];


Static Line *screen;


/*
   WriteCharacterAndAttribute - writes character, ch, to screen location,
                                x, y, with attibute colour, colour.
*/

Static void WriteCharacterAndAttribute(unsigned int x, unsigned int y,
				       Char ch, unsigned int colour)
{
  DisplayUnit *WITH;

  WITH = &screen[y][x];
  WITH->attrib = (Char)colour;
  WITH->char_ = ch;
}



Static void ClearScreen(void)
{
  unsigned int i, j;

  for (j = 0; j < Height; j++) {
    for (i = 0; i < Width; i++) {
      screen[j][i].char_ = ' ';
      screen[j][i].attrib = normal;
    }
  }
}


void _M2_ColourText_init(void)
{
  static int _was_initialized = 0;
  if (_was_initialized++)
    return;
  screen = osinit_ScreenAddress();
}
void _M2_ColourText_fini(void);

void _M2_ColourText_fini(void)
{
}


/* End. */
