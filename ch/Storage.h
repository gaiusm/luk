#if !defined(StorageH)
#define StorageH

#if !defined(SYSTEMH)
#   include "SYSTEM.h"
#endif


/*
   Author     : Gaius Mulley
   Title      : Storage
   Date       : Mon Jun 18 14:37:05 BST 1990
   Description: Implements the dynamic Storage handler for the
                Modula-2 compiler.
   Last update: Mon Jun 18 14:37:41 BST 1990
*/



/*
   ALLOCATE - attempt to allocate memory from the heap.
              NIL is returned in, a, if ALLOCATE fails.
*/

extern void Storage_ALLOCATE (void **a, unsigned int Size);


/*
   DEALLOCATE - return, Size, bytes to the heap.
*/

extern void Storage_DEALLOCATE (void *a, unsigned int Size);


/*
   Available - returns TRUE if, Size, bytes can be allocated.
*/

extern boolean Storage_Available (unsigned int Size);


/*
   Init - initialise the free store for the entire system.
*/

extern void Storage_Init (void);

#endif
