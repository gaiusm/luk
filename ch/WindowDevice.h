
#if !defined(WindowDeviceH)
#define WindowDeviceH

/*
   Author      : Gaius Mulley
   Version     : 1.1
   Date        : 4/3/86
   Description : Window Utilities
   Last Update : 12/7/86
                 4/8/94
*/

#if !defined(WindowDeviceC)
/* opaque type declaration */
typedef void *WindowDevice_Window;
#endif


/*
   InitWindow - returns a Window handle. This Window is uninitialized.
*/


extern void *WindowDevice_InitWindow (void);


/*
   KillWindow - destroys a Window, w, and returns NIL.
*/

extern void *WindowDevice_KillWindow (void *w);


/*
   SetWindow - sets a Window, w, to contain background colour, bg,
               foreground colour, fg. The width, height are specified
               and border indicates whether the window has a Grey border.
               The Window, w, is returned.
*/

extern void * WindowDevice_SetWindow (void *w, unsigned int bg,
				      unsigned int fg, unsigned int width,
				      unsigned int height,
				      unsigned int x, unsigned int y, BOOLEAN border);

/*
   AddWindow - adds a Window, w, to the display.
*/

extern void WindowDevice_AddWindow (void *w);


/*
   SubWindow - subtracts a Window, w, from the display.
*/

extern void WindowDevice_SubWindow (void *w);


/*
   NulWindow - returns TRUE if Window, w, is NIL.
               (Meaning it does not exist).
*/

extern BOOLEAN WindowDevice_NulWindow (void *w);


/*
   MoveCursor - moves the cursor of Window, w, to x, y.
*/

extern void WindowDevice_MoveCursor (void *w, unsigned int x, unsigned int y);

extern void WindowDevice_LargeCursor (void *w);

extern void WindowDevice_SmallCursor (void *w);

extern void WindowDevice_NoCursor (void *w);

extern void WindowDevice_ClearWindow (void *w);


/*
   MoveWindow - moves a Window, w, to position, x, y.
                The Window must have been removed from the display
                by SubWindow before this is called.
*/

extern void WindowDevice_MoveWindow (void *w, unsigned int x, unsigned int y);


/*
   WriteChar - writes a character, ch, to Window, w.
*/

extern void WindowDevice_WriteChar (void *w, Char ch);


/*
   WriteString - writes a string, a, to window, w.
*/

extern void WindowDevice_WriteString (void *w, const int a_LOW,
				      const int a_HIGH, const Char *a);


/*
   WriteLn - places the cursor onto the beginning of a new line
             in Window, w.
*/

extern void WindowDevice_WriteLn (void *w);

/*
   ColourWindow - alters the foreground and background
                  colour of Window, w.
*/

extern void WindowDevice_ColourWindow (void *w, unsigned int bg,
				       unsigned int fg);


/*
   SizeWindow - not implemented.
*/

extern void WindowDevice_SizeWindow (void *w, unsigned int width,
				     unsigned int height);


/*
   PutOnTop - places window, w, on top of the pile of windows.
*/

extern void WindowDevice_PutOnTop (void *w);


/*
   PutOnBottom - places Window, w, on the bottom of the pile
                 of Windows.
*/

extern void WindowDevice_PutOnBottom(void *w);

/*
   SelectWindow - returns a Window which can be seen at screen
                  location, x, y.
                  If no Window is seen then NIL is returned.
*/

extern void *WindowDevice_SelectWindow (unsigned int x, unsigned int y);


/*
   TitleWindow - adds a title to a Window, w.
*/

extern void WindowDevice_TitleWindow (void *w, const int a_LOW,
				      const int a_HIGH, const Char *a);


/*
   DefaultWindow - returns the default window.
*/

extern void *WindowDevice_DefaultWindow (void);


/*
   SetDefaultWindow - sets the default window.
*/

extern void WindowDevice_SetDefaultWindow (void *w);

#endif
