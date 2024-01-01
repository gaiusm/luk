#if !defined(DumpH)
#   define DumpH

#   if !defined(SYSTEMH)
#      include "SYSTEM.h"
#   endif

/*
    Title      : Dump
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Thu Aug  4 16:39:08 1994
    Last edit  : Thu Aug  4 16:39:08 1994
    Description: provides a crude hex dump of memory.
*/


/*
   DumpDown - displays Length bytes in WORDs starting at, Top,
              and working down.
*/

extern void Dump_DumpDown (void *Top, unsigned int Length);


/*
   DumpUp - displays Length bytes in WORDs starting at, Bot,
            and working up.
*/

extern void Dump_DumpUp (void *Bot, unsigned int Length);


#endif

