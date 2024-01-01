/************************************************************************
 *
 * defines created by Gaius so that the rest of the code i386-stub.c
 * remaine the same.
 ************************************************************************/

#include "../C/linkage.h"

#define GAIUS
#define exceptionHandler  InterruptVector_InstallIsr
#define getDebugChar      collectChar
#define putDebugChar      placeChar

extern void libg_Read       (char *);
static void set_debug_traps (void);
       void breakpoint      (void);


void _M2_gdb_init (void)
{
}


void _M2_gdb_finish (void)
{
}


static char collectChar (void)
{
  char ch;

  libg_Read(&ch);
  return( ch );
}


static char placeChar (char ch)
{
  libg_Write(ch);
  return(ch);
}


void gdb_Init (void)
{
  putDebugChar('G') ;  putDebugChar('D') ;  putDebugChar('B') ;
  set_debug_traps();
  /* **************************************************************
     THIS IS THE FIRST LINE YOU WILL SEE WHEN YOU USE GDB IN THE uK
     ************************************************************** */
  breakpoint();
}

void gdb_breakpoint (void)
{
  breakpoint();
}
