#if !defined(InterruptVectorH)
#   define InterruptVectorH


#   if !defined(SYSTEMH)
#      include "GSYSTEM.h"
#   endif


/*
    Title      : InterruptVector
    Author     : Gaius Mulley
    System     : RTS (gm2)
    Date       : Mon Aug  8 19:14:59 1994
    Last edit  : Mon Aug  8 19:14:59 1994
    Description: provides a simple interface to interrupt vectors.
*/


typedef void (*ProcISR)(unsigned int);


/*
   InitInterruptVector - initializes interrupt, VectorNo, to call
                         PROCEDURE, p, when an interrupt occurs.
*/


extern void InterruptVector_InitInterruptVector (unsigned int VectorNo,
						 void (*p)(unsigned int));


/*
   KillInterruptVector - removes interrupt, VectorNo.
*/

extern void InterruptVector_KillInterruptVector (unsigned int VectorNo);


/*
   InstallIsr - installs an interrupt service routine at interrupt vector,
                VectorNo, which will call address, a.
                (A very low level routine which is only exported so that the remote
                 debugger stub can get access to the breakpoint et al. exceptions).
*/

extern void InterruptVector_InstallIsr (unsigned int VectorNo, void *a);


/*
   Init - sets up IsrTemplate and then initializes all interrupt vectors.
*/

extern void InterruptVector_Init (void);


#endif
