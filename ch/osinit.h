
#if !defined(osinitH)
#   define osinitH

/*
    Title      : osinit
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Wed Aug  3 13:27:10 1994
    Last edit  : Wed Aug  3 13:27:10 1994
    Description: provides an interface which contains all
                 the initialization constants for the realtime system.
*/

#include "luk-types.h"

/*
   ScreenAddress - returns the start address of the video memory.
*/

extern void *osinit_ScreenAddress (void);


/*
   ExtendedMemoryEnd - returns the maximum extended memory address.
*/

extern void *osinit_ExtendedMemoryEnd (void);


/*
   ExtendedMemoryStart - returns the start of the extended memory.
*/

extern void *osinit_ExtendedMemoryStart (void);


/*
   BaseMemoryEnd - returns the maximum base memory address.
*/

extern void *osinit_BaseMemoryEnd (void);


/*
   BaseMemoryStart - returns the start of the base memory.
*/

extern void *osinit_BaseMemoryStart (void);


/*
   SizeOfOS - returns the size of the realtime system.
*/

extern unsigned int osinit_SizeOfOS (void);


/*
   DebuggingWithGDB - returns TRUE if we are remote debugging with gdb.
*/

extern boolean osinit_DebuggingWithGDB (void);


/*
   SetDebugging - sets the debugging variable.
*/

extern void osinit_SetDebugging (unsigned int d);


/*
   GetDebugging - returns the debugging variable.
                  0  means not debugging with GDB
                  1  means debugging via com1
                  2  means debugging via com2
*/

extern unsigned int osinit_GetDebugging (void);


/*
   MainStackSize - returns the size of mains stack.
*/

extern unsigned int osinit_MainStackSize (void);


/*
   Init - retrieve all parameters from second bootstage.
*/

extern void osinit_Init (void);


/*
   ResetParameters - replaces the parameters back to the original position
                     ready for a reboot.
*/

extern void osinit_ResetParameters (void);


#endif
