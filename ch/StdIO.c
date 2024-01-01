/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/StdIO.mod" */


#include <p2c/p2c.h>


#define StdIOG
#include "GStdIO.h"


#ifndef ScnH
#include "GScn.h"
#endif


#define MaxStack        40


/* p2c: ../luk-1.0/mod/StdIO.mod:11:
 * Warning: Symbol 'ProcWrite' is not defined [221] */
Static int Stack[MaxStack + 1];
Static unsigned int StackPtr;
/* ../luk-1.0/mod/StdIO.mod, line 22: undefined symbol Write */
/* Translation aborted. */
--------------------------
