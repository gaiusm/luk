#if !defined(ColourTextH)
#   define ColourTextH

/*
    Title      : ColourText
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Sun Jul 31 20:31:10 1994
    Last edit  : Sun Jul 31 20:31:10 1994
    Description: provides a simple interface to basic colour text.
*/


/*
   WriteCharacterAndAttribute - writes character, ch, to screen location,
                                x, y, with attibute colour, colour.
*/

extern void ColourText_WriteCharacterAndAttribute (unsigned int x, unsigned int y,
						   char ch, unsigned int colour);


#endif

