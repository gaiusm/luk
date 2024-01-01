/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/Exceptions.mod" */


#include <p2c/p2c.h>


#define ExceptionsG
#include "GExceptions.h"



/*
   Title      : MonStrIO
   Author     : Gaius Mulley
   Date       : 16/8/87
   LastEdit   : 12/8/94
   System     : LOGITECH MODULA-2/86 & (RTS GM2)
   Description: Provides a method of writting text to a screen that is
                unbuffered and process independant.
*/


#ifndef MonStrIOH
#include "GMonStrIO.h"
#endif

#ifndef InterruptVectorH
#include "GInterruptVector.h"
#endif


/*
   Init - install a set of 386/486 exception handlers.
*/

Static void Init(void)
{
/* p2c: ../luk-1.0/mod/Exceptions.mod:14:
 * Warning: Symbol 'DivideError' is not defined [221] */
  InterruptVector_InitInterruptVector(0, DivideError);
/* p2c: ../luk-1.0/mod/Exceptions.mod:15:
 * Warning: Symbol 'DebugException' is not defined [221] */
  InterruptVector_InitInterruptVector(1, DebugException);
/* p2c: ../luk-1.0/mod/Exceptions.mod:16:
 * Warning: Symbol 'NMIInterrupt' is not defined [221] */
  InterruptVector_InitInterruptVector(2, NMIInterrupt);
/* p2c: ../luk-1.0/mod/Exceptions.mod:17:
 * Warning: Symbol 'Breakpoint' is not defined [221] */
  InterruptVector_InitInterruptVector(3, Breakpoint);
/* p2c: ../luk-1.0/mod/Exceptions.mod:18:
 * Warning: Symbol 'GeneralProtection' is not defined [221] */
  InterruptVector_InitInterruptVector(13, GeneralProtection);
}
/* ../luk-1.0/mod/Exceptions.mod, line 30: undefined symbol DumpRegisters */
/* Translation aborted. */
--------------------------
