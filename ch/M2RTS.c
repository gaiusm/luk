/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/M2RTS.mod" */


#include <p2c/p2c.h>


#define M2RTSG
#include "GM2RTS.h"

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif


#ifndef ScnH
#include "GScn.h"
#endif

#ifndef StdIOH
#include "GStdIO.h"
#endif
/* p2c: ../luk-1.0/mod/M2RTS.mod:7: 
 * Warning: Could not find module StrIO [271] */

#include "GStrIO.h"

#ifndef NumberIOH
#include "GNumberIO.h"
#endif
/* p2c: ../luk-1.0/def/Descriptors.def:15: 
 * Warning: Could not find module WordSizes [271] */

#ifndef DescriptorsH
#include "GDescriptors.h"
#endif

#ifndef ASCIIH
#include "GASCII.h"
#endif

#ifndef StrLibH
#include "GStrLib.h"
#endif

#ifndef osinitH
#include "Gosinit.h"
#endif

#ifndef crt0H
#include "Gcrt0.h"
#endif

#ifndef InterruptVectorH
#include "GInterruptVector.h"
#endif

#ifndef IRQH
#include "GIRQ.h"
#endif

#ifndef StorageH
#include "GStorage.h"
#endif

#ifndef ExceptionsH
#include "GExceptions.h"
#endif

#ifndef MonStrIOH
#include "GMonStrIO.h"
#endif

#ifndef libgH
#include "Glibg.h"
#endif

#ifndef gdbH
#include "Ggdb.h"
#endif


#define MaxProcedures   10


Static unsigned int iPtr, tPtr;
Static void (*(InitialProc[MaxProcedures + 1]))(void),
	    (*(TerminateProc[MaxProcedures + 1]))(void);
Static int ExitValue;
Static BOOLEAN CallExit;


/*
   exit -
*/

Static void exit_(int r)
{
  StdIO_Write('e');
  StdIO_Write('x');
  StdIO_Write('i');
  StdIO_Write('t');
  while (1) ;
}


/*
   abort -
*/

Static void abort(void)
{
  StdIO_Write('a');
  StdIO_Write('b');
  StdIO_Write('o');
  StdIO_Write('r');
  StdIO_Write('t');
  while (1) ;
}


/*
   ExecuteTerminationProcedures - calls each installed termination procedure
                                  in reverse order.
*/

Static void ExecuteTerminationProcedures(void)
{
  unsigned int i;

  i = tPtr;
  while (i > 0) {
    i--;
    TerminateProc[i]();
  }
}


/*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure
                                 ExecuteTerminationProcedures
                                 is invoked.  It returns TRUE is the
                                 procedure is installed.
*/

Static BOOLEAN InstallTerminationProcedure(void (*p)(void))
{
  if (tPtr > MaxProcedures)
    return FALSE;
  else {
    TerminateProc[tPtr] = p;
    tPtr++;
    return TRUE;
  }
}


/*
   ExecuteInitialProcedures - executes the initial procedures installed by
                              InstallInitialProcedure.
*/

Static void ExecuteInitialProcedures(void)
{
  unsigned int i;

  i = iPtr;
  while (i > 0) {
    i--;
    InitialProc[i]();
  }
}


/*
   InstallInitialProcedure - installs a procedure to be executed just before the
                             BEGIN code section of the main program module.
*/

Static BOOLEAN InstallInitialProcedure(void (*p)(void))
{
  if (iPtr > MaxProcedures)
    return FALSE;
  else {
    InitialProc[iPtr] = p;
    iPtr++;
    return TRUE;
  }
}


/*
   HALT - terminate the current program.  The procedure
          ExecuteTerminationProcedures
          is called before the program is stopped.  The parameter
          exitcode is optional.  If the parameter is not supplied
          HALT will call libc 'abort', otherwise it will exit with
          the code supplied.  Supplying a parameter to HALT has the
          same effect as calling ExitOnHalt with the same code and
          then calling HALT with no parameter.
*/

Static void HALT(int exitcode)
{
  if (exitcode != (-1)) {
    CallExit = TRUE;
    ExitValue = exitcode;
  }
  ExecuteTerminationProcedures();
  StdIO_Write('H');
  StdIO_Write('A');
  StdIO_Write('L');
  StdIO_Write('T');
  if (CallExit)
    exit_(ExitValue);
  else
    abort();
}


/*
   Terminate - provides compatibility for pim.  It call exit with
               the exitcode provided in a prior call to ExitOnHalt
               (or zero if ExitOnHalt was never called).  It does
               not call ExecuteTerminationProcedures.
*/

Static void Terminate(void)
{
  exit_(ExitValue);
}


/*
   ErrorString - writes a string to the Scn device.
*/

Static void ErrorString(const int a_LOW, const int a_HIGH, const Char *a)
{
  unsigned int i;

  i = 0;
  while ((i <= (a_HIGH - a_LOW)) && (a[i] != ASCII_nul)) {
    StdIO_Write(a[i]);
    i++;
  }
}


/*
   ErrorMessage - emits an error message to stderr and then calls exit (1).
*/

Static void ErrorMessage(const int message_LOW, const int message_HIGH,
  const Char *message, const int file_LOW, const int file_HIGH,
  const Char *file, unsigned int line, const int function_LOW,
  const int function_HIGH, const Char *function)
{
  Char LineNo[11];

  ErrorString(0L, file_HIGH - file_LOW, file);
  ErrorString(1L, 1L, ":");
  NumberIO_CardToStr(line, 0, 0L, 10L, LineNo);
  ErrorString(0L, 10L, LineNo);
  ErrorString(1L, 1L, ":");
  if (!StrLib_StrEqual(0L, function_HIGH - function_LOW, function, 1L, 0L, "")) {
    ErrorString(1L, 3L, "in ");
    ErrorString(0L, function_HIGH - function_LOW, function);
    ErrorString(1L, 12L, " has caused ");
  }
  ErrorString(0L, message_HIGH - message_LOW, message);
  LineNo[0] = ASCII_nl;
  LineNo[1] = ASCII_nul;
  ErrorString(0L, 10L, LineNo);
  exit_(1);
}


/*
   Halt - provides a more user friendly version of HALT, which takes
          four parameters to aid debugging.
*/

Static void Halt(const int file_LOW, const int file_HIGH, const Char *file,
  unsigned int line, const int function_LOW, const int function_HIGH,
  const Char *function, const int description_LOW, const int description_HIGH,
  const Char *description)
{
  ErrorMessage(0L, description_HIGH - description_LOW, description, 0L,
	       file_HIGH - file_LOW, file, line, 0L,
	       function_HIGH - function_LOW, function);
  HALT(-1);
}


/*
   addChar - adds, ch, to the Scn device.
*/

Static void addChar(Char ch)
{
  Scn_Write(ch);
}


/*
   stripPath - returns the filename from the path.
*/

Static void *(stripPath(void *s))
{
  Char *f, *p;

  p = s;
  f = s;
  while ((*p) != ASCII_nul) {
    if ((*p) == '/') {
      p++;
      f = p;
    } else
      p++;
  }
  return f;
}


/*
   addFile - adds the filename determined by, s, to the Scn device.  It strips
             any preceeding path.
*/

Static void addFile(void *s)
{
  Char *p;

  p = stripPath(s);
  while ((p != NULL) && ((*p) != ASCII_nul)) {
    addChar(*p);
    p++;
  }
}


/*
   addStr - adds a C string from address to the Scn device.
*/

Static void addStr(void *s)
{
  Char *p;

  p = s;
  while ((p != NULL) && ((*p) != ASCII_nul)) {
    addChar(*p);
    p++;
  }
}


/*
   addNum - writes number to the screen.
*/

Static void addNum(unsigned int n)
{
  if (n < 10)
    addChar((n % 10) + '0');
  else {
    addNum(n / 10);
    addNum(n % 10);
  }
}


/*
   Raise - invoke the exception handler associated with, number,
           in the active EHBlock.  It keeps a record of the number
           and message in the EHBlock for later use.
*/

Static void Raise(void *file, unsigned int line, unsigned int column,
		  void *function, void *message)
{
  addFile(file);
  addChar(':');
  addNum(line);
  addChar(':');
  addNum(column);
  addChar(':');
  addStr(message);
  addChar(' ');
  addChar('i');
  addChar('n');
  addChar(' ');
  addStr(function);
  addChar(ASCII_nl);
  while (1) ;
}


/*
   The following are the runtime exception handler routines.
*/

Static void AssignmentException(void *filename, unsigned int line,
				unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"variable exceeds range during assignment");
}


Static void IncException(void *filename, unsigned int line,
			 unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"variable exceeds range during INC statement");
}


Static void DecException(void *filename, unsigned int line,
			 unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"variable exceeds range during DEC statement");
}


Static void InclException(void *filename, unsigned int line,
			  unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"bit exceeds set range during INCL statement");
}


Static void ExclException(void *filename, unsigned int line,
			  unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"bit exceeds set range during EXCL statement");
}


Static void ShiftException(void *filename, unsigned int line,
			   unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"bit exceeds set range during SHIFT statement");
}


Static void RotateException(void *filename, unsigned int line,
			    unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"bit exceeds set range during ROTATE statement");
}


Static void StaticArraySubscriptException(void *filename, unsigned int line,
					  unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"array index out of bounds during static array access");
}


Static void DynamicArraySubscriptException(void *filename, unsigned int line,
					   unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"array index out of bounds during dynamic array access");
}


Static void ForLoopBeginException(void *filename, unsigned int line,
				  unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"iterator variable exceeds range during FOR loop initial assignment");
}


Static void ForLoopToException(void *filename, unsigned int line,
			       unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
    "iterator variable will exceed range when calculating final value in FOR loop");
}


Static void ForLoopEndException(void *filename, unsigned int line,
				unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
    "iterator variable exceeds range during increment at the end of a FOR loop");
}


Static void PointerNilException(void *filename, unsigned int line,
				unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"attempting to dereference a NIL valued pointer");
}


Static void NoReturnException(void *filename, unsigned int line,
			      unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"about to finish a PROCEDURE without executing a RETURN statement");
}


Static void CaseException(void *filename, unsigned int line,
			  unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"the expression in the CASE statement cannot be selected");
}


Static void WholeNonPosDivException(void *filename, unsigned int line,
				    unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
    "the division expression has a divisor which is less than or equal to zero");
}


Static void WholeNonPosModException(void *filename, unsigned int line,
				    unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
    "the modulus expression has a divisor which is less than or equal to zero");
}


Static void WholeZeroDivException(void *filename, unsigned int line,
				  unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"the division expression has a divisor which is equal to zero");
}


Static void WholeZeroRemException(void *filename, unsigned int line,
				  unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
	"the remainder expression has a divisor which is equal to zero");
}


Static void NoException(void *filename, unsigned int line,
			unsigned int column, void *scope)
{
  Raise(filename, line, column, scope,
    "M2Expection was called when no there was no outstanding exception to be returned");
}


/*
   ExitOnHalt - if HALT is executed then call exit with the exit code, e.
*/

Static void ExitOnHalt(int e)
{
  ExitValue = e;
  CallExit = TRUE;
}


/*
   Length - returns the length of a string, a. This is called whenever
            the user calls LENGTH and the parameter cannot be calculated
            at compile time.
*/

Static unsigned int Length(const int a_LOW, const int a_HIGH, const Char *a)
{
  unsigned int l, h;

  l = 0;
  h = a_HIGH - a_LOW;
  while ((l <= h) && (a[l] != ASCII_nul))
    l++;
  return l;
}


/*
   Init -
*/

Static void Init(void)
{
  iPtr = 0;
  tPtr = 0;
  ExitValue = 0;
  CallExit = FALSE;   /* default by calling abort */
}


void _M2_M2RTS_init(void)
{
  static int _was_initialized = 0;
  if (_was_initialized++)
    return;
  /*
     it is the job of M2RTS to call each all runtime initialization
     routines. We need a global initialization routine as the order
     it critical and we dare not let the statical analysis guess it..
  */
  Init();   /* M2RTS    initialization */
  crt0_red();
  osinit_Init();   /* osinit   initialization: RTS mem parameters  */
  Scn_Init();   /* Scn      initialization: screen address      */
  Scn_Write('S');
  Scn_Write('c');
  Scn_Write('n');

  StdIO_Init();   /* StdIO    initialization: use Scn.Write       */
  Scn_Write('S');
  Scn_Write('t');
  Scn_Write('d');
  Scn_Write('i');
  Scn_Write('o');
  MonStrIO_Init();
      /* MonStrIO initialization: use Scn.Write for debugging */
  Scn_Write('M');
  Scn_Write('o');
  Scn_Write('n');
  Scn_Write('S');
  Scn_Write('t');
  Scn_Write('r');
  Scn_Write('I');
  Scn_Write('O');

  Descriptors_SetupIDT();   /* 386/486 MMU Interrupt descriptor tables     */
  Scn_Write('I');
  Scn_Write('D');
  Scn_Write('T');

  Descriptors_SetupGDT();   /* 386/486 MMU Global    descriptor tables     */
  Scn_Write('G');
  Scn_Write('D');
  Scn_Write('T');

  InterruptVector_Init();   /* Catchall interrupt handlers      */
  Scn_Write('i');
  Scn_Write('n');
  Scn_Write('t');

  IRQ_Init();   /* Catchall IRQ specific handlers   */
  Scn_Write('I');
  Scn_Write('R');
  Scn_Write('Q');

/* p2c: ../luk-1.0/mod/M2RTS.mod:561:
 * Warning: Symbol 'SYSTEM' is not of the appropriate class [222] */
/* p2c: ../luk-1.0/mod/M2RTS.mod:561:
 * Warning: No field called Init in that record [288] */
  /* SYSTEM  initialization: IOTRANSFER table    */
  Scn_Write('S');
  Scn_Write('Y');
  Scn_Write('S');

  Storage_Init();   /* Storage initialization: heap management     */
  Scn_Write('S');
  Scn_Write('t');
  Scn_Write('o');
  Scn_Write('r');
  Scn_Write('a');
  Scn_Write('g');
  Scn_Write('e');
  StdIO_Write(ASCII_cr);
  StdIO_Write(ASCII_nl);

  StdIO_Write('g');
  StdIO_Write('o');
  StdIO_Write(ASCII_cr);
  StdIO_Write(ASCII_nl);

  Exceptions_Init();   /* install exception interrupt handlers     */

  if (osinit_DebuggingWithGDB())
  {  /* remove this when not debugging              */
    libg_Init();
    gdb_Init();
  }
}
void _M2_M2RTS_fini(void);

void _M2_M2RTS_fini(void)
{
}


/* End. */
