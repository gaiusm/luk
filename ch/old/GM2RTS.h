/* Header for module M2RTS, generated by p2c */
#ifndef M2RTSH
#define M2RTSH


#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif


#ifdef M2RTSG
# define vextern
#else
# define vextern extern
#endif

/*
   Author     : Gaius Mulley
   Title      : M2RTS
   Date       : Wed Jun 20 15:21:04 BST 1990
   Description: Implements the run time system facilities of Modula-2.
*/



/*
   ExecuteTerminationProcedures - calls each installed termination
                                  procedure in reverse order.
*/

extern void M2RTS_ExecuteTerminationProcedures(void);


/*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure
                                 ExecuteTerminationProcedures
                                 is invoked.  It returns TRUE is the
                                 procedure is installed.
*/

extern BOOLEAN M2RTS_InstallTerminationProcedure(void (*p)(void));


/*
   ExecuteInitialProcedures - executes the initial procedures installed
                              by InstallInitialProcedure.
*/

extern void M2RTS_ExecuteInitialProcedures(void);


/*
   InstallInitialProcedure - installs a procedure to be executed just
                             before the BEGIN code section of the main
                             program module.
*/

extern BOOLEAN M2RTS_InstallInitialProcedure(void (*p)(void));


/*
   Terminate - provides compatibility for pim.  It calls exit with
               the exitcode provided in a prior call to ExitOnHalt
               (or zero if ExitOnHalt was never called).  It does
               not call ExecuteTerminationProcedures.
*/

extern void M2RTS_Terminate(void);


/*
   HALT - terminate the current program.  The procedure Terminate
          is called before the program is stopped.  The parameter
          exitcode is optional.  If the parameter is not supplied
          HALT will call libc 'abort', otherwise it will exit with
          the code supplied.  Supplying a parameter to HALT has the
          same effect as calling ExitOnHalt with the same code and
          then calling HALT with no parameter.
*/

extern void M2RTS_HALT(int exitcode);


/*
   Halt - provides a more user friendly version of HALT, which takes
          four parameters to aid debugging.
*/

extern void M2RTS_Halt(const int file_LOW, const int file_HIGH,
  const Char *file, unsigned int line, const int function_LOW,
  const int function_HIGH, const Char *function, const int description_LOW,
  const int description_HIGH, const Char *description);


/*
   ExitOnHalt - if HALT is executed then call exit with the exit code, e.
*/

extern void M2RTS_ExitOnHalt(int e);


/*
   ErrorMessage - emits an error message to stderr and then calls exit (1).
*/

extern void M2RTS_ErrorMessage(const int message_LOW, const int message_HIGH,
  const Char *message, const int file_LOW, const int file_HIGH,
  const Char *file, unsigned int line, const int function_LOW,
  const int function_HIGH, const Char *function);


/*
   Length - returns the length of a string, a. This is called whenever
            the user calls LENGTH and the parameter cannot be calculated
            at compile time.
*/

extern unsigned int M2RTS_Length(const int a_LOW, const int a_HIGH,
				 const Char *a);


/*
   The following are the runtime exception handler routines.
*/

extern void M2RTS_AssignmentException(void *filename, unsigned int line,
				      unsigned int column, void *scope);
extern void M2RTS_IncException(void *filename, unsigned int line,
			       unsigned int column, void *scope);
extern void M2RTS_DecException(void *filename, unsigned int line,
			       unsigned int column, void *scope);
extern void M2RTS_InclException(void *filename, unsigned int line,
				unsigned int column, void *scope);
extern void M2RTS_ExclException(void *filename, unsigned int line,
				unsigned int column, void *scope);
extern void M2RTS_ShiftException(void *filename, unsigned int line,
				 unsigned int column, void *scope);
extern void M2RTS_RotateException(void *filename, unsigned int line,
				  unsigned int column, void *scope);
extern void M2RTS_StaticArraySubscriptException(void *filename,
  unsigned int line, unsigned int column, void *scope);
extern void M2RTS_DynamicArraySubscriptException(void *filename,
  unsigned int line, unsigned int column, void *scope);
extern void M2RTS_ForLoopBeginException(void *filename, unsigned int line,
					unsigned int column, void *scope);
extern void M2RTS_ForLoopToException(void *filename, unsigned int line,
				     unsigned int column, void *scope);
extern void M2RTS_ForLoopEndException(void *filename, unsigned int line,
				      unsigned int column, void *scope);
extern void M2RTS_PointerNilException(void *filename, unsigned int line,
				      unsigned int column, void *scope);
extern void M2RTS_NoReturnException(void *filename, unsigned int line,
				    unsigned int column, void *scope);
extern void M2RTS_CaseException(void *filename, unsigned int line,
				unsigned int column, void *scope);
extern void M2RTS_WholeNonPosDivException(void *filename, unsigned int line,
					  unsigned int column, void *scope);
extern void M2RTS_WholeNonPosModException(void *filename, unsigned int line,
					  unsigned int column, void *scope);
extern void M2RTS_WholeZeroDivException(void *filename, unsigned int line,
					unsigned int column, void *scope);
extern void M2RTS_WholeZeroRemException(void *filename, unsigned int line,
					unsigned int column, void *scope);
extern void M2RTS_NoException(void *filename, unsigned int line,
			      unsigned int column, void *scope);


#undef vextern

#endif /*M2RTSH*/

/* End. */