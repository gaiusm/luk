
#if !defined(SYSTEMH)
#   define SYSTEMH

/*
   Author     : Gaius Mulley
   Title      : SYSTEM
   Date       : 3/4/86
   Description: Implements the SYSTEM dependant module
                in the Modula-2 compiler.
   Last update: 9/9/89  - types and pseudo procedures exported:
                          ADR, SIZE, TSIZE.
                29/7/94 - started to add items to SYSTEM.mod
                          added In, Out.
                3/8/94  - added TRANSFER
                4/8/94  - added NEWPROCESS
                10/8/94 - added IOTRANSFER
                12/8/94 - added LISTEN
                23/7/97 - removed port io and placed into separate module.
                12/9/2011 - ported LUK to the GNU Modula-2.

   $Version$

   $Log: SYSTEM.def,v $
   Revision 1.2  1996-07-26 12:05:09  gaius
   added minor support modules for the larger test MorlocTower
*/


typedef enum {
  On, Off
} OnOrOff;

#if !defined(SYSTEMC)
/* opaque type declaration */
typedef void *PROCESS;
#endif


/*
   TRANSFER - save the current volatile environment into, p1.
              Restore the volatile environment from, p2.
*/


extern void SYSTEM_TRANSFER (void **p1, void *p2);


/*
   NEWPROCESS - p is a parameterless procedure, a, is the origin of
                the workspace used for the process stack and containing
                the volatile environment of the process. n, is the amount
                in bytes of this workspace. new, is the new process.
*/

extern void SYSTEM_NEWPROCESS (void (*p)(void), void *a, unsigned int n,
			       void **new_);


/*
   IOTRANSFER - saves the current volatile environment into, First,
                and restores volatile environment, Second.
                When an interrupt, InterruptNo, is encountered then
                the reverse takes place. (The then current volatile
                environment is shelved onto Second and First is resumed).

                NOTE: that upon interrupt the Second might not be the
                      same process as that before the original call to
                      IOTRANSFER.
*/

extern void SYSTEM_IOTRANSFER (void **First, void **Second,
			       unsigned int InterruptNo);


/*
   LISTEN - briefly listen for any interrupts.
*/

extern void SYSTEM_LISTEN (void);


/*
   TurnInterrupts - switches interrupts on or off depending
                    on Switch. It returns the old value.
*/

extern OnOrOff SYSTEM_TurnInterrupts (OnOrOff Switch);


/*
   Init - initialize SYSTEM data structures. IOTRANSFER process tables.
*/

extern void SYSTEM_Init (void);


extern void SYSTEM_CheckOff (void);

#endif
