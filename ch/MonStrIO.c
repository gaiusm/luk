/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/MonStrIO.mod" */


#include <p2c/p2c.h>


#define MonStrIOG
#include "GMonStrIO.h"


#ifndef StdIOH
#include "GStdIO.h"
#endif

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif

#ifndef ScnH
#include "GScn.h"
#endif
/* p2c: ../luk-1.0/mod/MonStrIO.mod:8: 
 * Warning: Could not find module StrIO [271] */

#include "GStrIO.h"

#ifndef NumberIOH
#include "GNumberIO.h"
#endif



/* p2c: ../luk-1.0/mod/MonStrIO.mod:14:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
Static int OldState;
Static void (*DebuggingWrite)(Char);


/*
   DebuggingStream - sets the debugging output to use, p.
*/

Static void DebuggingStream(void (*p)(Char))
{
  int OldState;

  OldState = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/MonStrIO.mod:26:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/MonStrIO.mod:26:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  DebuggingWrite = p;
  OldState = TurnInterrupts(OldState);
/* p2c: ../luk-1.0/mod/MonStrIO.mod:28:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
}


/*
   StartDebugging - initializes the output for a debugging message.
*/

Static void StartDebugging(void)
{
  OldState = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/MonStrIO.mod:38:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/MonStrIO.mod:38:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  StdIO_PushOutput(DebuggingWrite);
}


/*
   EndDebugging - returns the output back to that before StartDebugging
                  was called.
*/

Static void EndDebugging(void)
{
  OldState = TurnInterrupts(OldState);
/* p2c: ../luk-1.0/mod/MonStrIO.mod:50:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  StdIO_PopOutput();
}
/* ../luk-1.0/mod/MonStrIO.mod, line 58: undefined symbol StrIO */
/* Translation aborted. */
--------------------------
