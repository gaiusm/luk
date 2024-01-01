/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/Storage.mod" */


#include <p2c/p2c.h>


#define StorageG
#include "GStorage.h"

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif

/*
   Standard dynamic storage management
*/

#ifndef MemRegionH
#include "GMemRegion.h"
#endif

#ifndef M2RTSH
#include "GM2RTS.h"
#endif

#ifndef osinitH
#include "Gosinit.h"
#endif



Static void *TopOfHeap;
Static unsigned int Used, Left;


Static void ALLOCATE(void **a, unsigned int Size)
{
/* p2c: ../luk-1.0/mod/Storage.mod:24:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int ToOldState;

  ToOldState = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/Storage.mod:26:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/Storage.mod:26:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  if (Left < Size)
    M2RTS_Halt(1L, 26L, "../luk-1.0/mod/Storage.mod", 29, 1L, 13L,
	       "some function", 1L, 22L, "not enough free memory");
  else {
    Used += Size;
    *a = (void *)(((unsigned int)TopOfHeap) - Used);
    Left -= Size;
  }
  ToOldState = TurnInterrupts(ToOldState);
/* p2c: ../luk-1.0/mod/Storage.mod:36:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
}


Static void DEALLOCATE(void *a, unsigned int Size)
{
}


/*
   Available - returns TRUE if, Size, bytes can be allocated.
*/

Static BOOLEAN Available(unsigned int Size)
{
  return (Size <= Left);
}


/*
   Init - initializes the heap.
          There are two different scenarios:

          (i)   that the OS is placed into base memory <1M
                in which case the heap lies 1M..MAX-Stack
          (ii)  that the OS is placed at 1M upwards in this
                more difficult case we have two heaps.

                (a)  0..base memory top
                (b)  end of OS .. MAX-Stack

          We detect which case at runtime, for now (a) is thrown away.
*/

Static void Init(void)
{
  Used = 0;
  if (MemRegion_EndOfOS() < osinit_ExtendedMemoryStart()) {
    /* case (i) */
    Left = ((unsigned int)(osinit_ExtendedMemoryEnd() -
	      osinit_ExtendedMemoryStart())) - osinit_MainStackSize() - 1;
  } else {
    /* case (ii) */
    Left = ((unsigned int)(osinit_ExtendedMemoryEnd() - MemRegion_EndOfOS())) -
	   osinit_MainStackSize() - 1;
  }
  TopOfHeap = osinit_ExtendedMemoryEnd() - ((void *)osinit_MainStackSize()) -
	      ((void *)1);
}


void _M2_Storage_init(void);

void _M2_Storage_init(void)
{
}
void _M2_Storage_fini(void);

void _M2_Storage_fini(void)
{
}


/* End. */
