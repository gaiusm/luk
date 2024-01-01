/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/SYSTEM.mod" */


#include <p2c/p2c.h>


#define SYSTEMG
#include "GSYSTEM.h"


#ifndef M2RTSH
#include "GM2RTS.h"
#endif

#ifndef ScnH
#include "GScn.h"
#endif

#ifndef IRQH
#include "GIRQ.h"
#endif

#ifndef InterruptVectorH
#include "GInterruptVector.h"
#endif

#ifndef StdIOH
#include "GStdIO.h"
#endif
/* p2c: ../luk-1.0/mod/SYSTEM.mod:10: 
 * Warning: Could not find module StrIO [271] */

#include "GStrIO.h"

#ifndef NumberIOH
#include "GNumberIO.h"
#endif


#define MaxInterruptNo  255
    /* maximum number of interrupt vectors          */
#define InterruptBit    9   /* bit 9 of the 386/486 flags indicates ints on */
#define StartIRQ        0x20
    /* start of vectors that are IRQ 8259 related   */
#define EndIRQ          (StartIRQ + 15)

#define DebugOn         FALSE
    /* should we check the interrupt state?         */


typedef struct IOstate {
  BOOLEAN Used;   /* is there a pending IOTRANSFER? */
  BOOLEAN IsIRQ;   /* is this interrupt no. an IRQ?  */
  void *AddressOfFirst, *AddressOfSecond;
} IOstate;


Static IOstate IOTransferTo[MaxInterruptNo + 1];


/* p2c: ../luk-1.0/mod/SYSTEM.mod:45:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/SYSTEM.mod:45:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
/* ../luk-1.0/mod/SYSTEM.mod, line 46: undefined symbol ASM */
/* Translation aborted. */
--------------------------
