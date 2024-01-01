#if !defined(ScnH)
#   define ScnH


/*
   Write - writes character, ch, to the screen.
           Write interprets cr, lf.
*/

extern void Scn_Write (char ch);


/*
   MoveCursor - moves the cursor to position, x, y.
*/

extern void Scn_MoveCursor (unsigned int x, unsigned int y);


/*
   ClearScreen - clears the screen, filling it with spaces.
*/

extern void Scn_ClearScreen (void);


/*
   ClearLine - clears line, y, filling it with spaces.
*/

extern void Scn_ClearLine (unsigned int y);


/*
   AppendLine - adds another line after, y, to the display, all the
                other lines move down one line.
*/

extern void Scn_AppendLine (unsigned int y);


/*
   DeleteLine - deletes line, y, from the screen, the lower lines
                move up to fill in the gap.
*/

extern void Scn_DeleteLine (unsigned int y);


/*
   Bell - rings the bell on the console.
*/

extern void Scn_Bell (void);


/*
   Init - initialize all module data structures.
*/

extern void Scn_Init (void);


#endif
