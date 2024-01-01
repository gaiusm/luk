#if !defined(M2RTSH)
#   define M2RTSH

#   if !defined(SYSTEMH)
#      include "SYSTEM.h"
#   endif

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

extern void M2RTS_ExecuteTerminationProcedures (void);


/*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure
                                 ExecuteTerminationProcedures
                                 is invoked.  It returns TRUE is the
                                 procedure is installed.
*/

extern BOOLEAN M2RTS_InstallTerminationProcedure (void (*p)(void));


/*
   ExecuteInitialProcedures - executes the initial procedures installed
                              by InstallInitialProcedure.
*/

extern void M2RTS_ExecuteInitialProcedures (void);


/*
   InstallInitialProcedure - installs a procedure to be executed just
                             before the BEGIN code section of the main
                             program module.
*/

extern BOOLEAN M2RTS_InstallInitialProcedure (void (*p)(void));


/*
   Terminate - provides compatibility for pim.  It calls exit with
               the exitcode provided in a prior call to ExitOnHalt
               (or zero if ExitOnHalt was never called).  It does
               not call ExecuteTerminationProcedures.
*/

extern void M2RTS_Terminate (void);


/*
   HALT - terminate the current program.
*/

extern void M2RTS_HALT (void);


/*
   Halt - provides a more user friendly version of HALT, which takes
          four parameters to aid debugging.
*/

extern void M2RTS_Halt (const int file_LOW, const int file_HIGH, const char *file,
			unsigned int line,
			const int function_LOW, const int function_HIGH, const char *function,
			const int description_LOW, const int description_HIGH, const char *description);



#endif
