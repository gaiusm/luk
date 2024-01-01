/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/IRQ.mod" */


#include <p2c/p2c.h>


#define IRQG
#include "GIRQ.h"




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

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif


#ifndef PortIOH
#include "GPortIO.h"
#endif


/* p2c: ../luk-1.0/mod/IRQ.mod:15:
 * Warning: Symbol 'MaxIRQ' is not defined [221] */
/* p2c: ../luk-1.0/mod/IRQ.mod:15:
 * Warning: Symbol 'ProcIRQ' is not defined [221] */
Static int IRQHandler[MaxIRQ + 1];
Static int Cache21, CacheA1;   /* cache bytes for the two 8259s */


/*
   DisableIRQ - disable irq, IrqNo.
*/

Static void DisableIRQ(unsigned int IrqNo)
{
/* p2c: ../luk-1.0/mod/IRQ.mod:26:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int Back;

  Back = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/IRQ.mod:28:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/IRQ.mod:28:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  if (IrqNo < 8) {
    Cache21 |= 1L << IrqNo;
    PortIO_Out8(0x21, *(uchar *)(&Cache21));
  } else if (IrqNo < 16) {
    IrqNo &= 7;
    CacheA1 |= 1L << IrqNo;
    PortIO_Out8(0xa1, *(uchar *)(&CacheA1));
  }
  Back = TurnInterrupts(Back);
/* p2c: ../luk-1.0/mod/IRQ.mod:40:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
}


/*
   EnableIRQ - enable irq, IrqNo.
*/

Static void EnableIRQ(unsigned int IrqNo)
{
/* p2c: ../luk-1.0/mod/IRQ.mod:49:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int Back;

  Back = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/IRQ.mod:51:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/IRQ.mod:51:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  /*
     WriteString('\nEnableIRQ ') ; WriteCard(IrqNo, 4) ;
  */
  if (IrqNo < 8) {
    Cache21 &= ~(1L << IrqNo);
    PortIO_Out8(0x21, *(uchar *)(&Cache21));
  } else if (IrqNo < 16) {
    IrqNo &= 7;
    CacheA1 &= ~(1L << IrqNo);
    PortIO_Out8(0xa1, *(uchar *)(&CacheA1));
  }
  Back = TurnInterrupts(Back);
/* p2c: ../luk-1.0/mod/IRQ.mod:66:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
}


/*
   InitIRQ - initialises irq, IrqNo, to call PROCEDURE, p,
             when this interrupt occurs.
*/

Static void InitIRQ(unsigned int IrqNo, int p)
{
/* p2c: ../luk-1.0/mod/IRQ.mod:76:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int Back;

  Back = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/IRQ.mod:78:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/IRQ.mod:78:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  /* WriteString('\nInitIRQ ') ; WriteCard(IrqNo, 4) ; */
  IRQHandler[IrqNo] = p;
  if (IrqNo < 8) {
    Cache21 &= ~(1L << IrqNo);
    PortIO_Out8(0x21, *(uchar *)(&Cache21));
  } else if (IrqNo < 16) {
    Cache21 &= ~(1L << 4);
    CacheA1 &= ~(1L << (IrqNo - 8));
    PortIO_Out8(0x21, *(uchar *)(&Cache21));
    PortIO_Out8(0xa1, *(uchar *)(&CacheA1));
  } else {
    MonStrIO_WriteString(1L, 27L, "InitIRQ IrqNo out of range:");
    MonStrIO_WriteCard(IrqNo, 4);
  }
  Back = TurnInterrupts(Back);
/* p2c: ../luk-1.0/mod/IRQ.mod:95:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
}


/*
   GenericHandler - the generic handler which will call the
                    appropriate IRQ handler.
*/

Static void GenericHandler(unsigned int IntNo)
{
  unsigned int irq;
  uchar b;

/* p2c: ../luk-1.0/mod/IRQ.mod:122:
 * Warning: Expected END, found a '(' [227] */
  /* user must turn 8259 irq back on again */
  /* Turn 8259 interrupt irq off */
  /* user must turn 8259 irq back on again */
  /*
     WriteString('\nInside GenericHandler') ;
  */
  if ((IntNo < 32) || (IntNo >= 48))
    return;
  /* ok correct interrupt range for our IRQ */
  irq = IntNo - 32;
  if (irq >= 8)
    return;
  b = PortIO_In8(0x21);
  /* Turn 8259 interrupt irq off */
  Cache21 |= 1L << irq;
  PortIO_Out8(0x21, *(uchar *)(&Cache21));
  PortIO_Out8(0x20, 0x20);

/* p2c: ../luk-1.0/mod/IRQ.mod:122:
 * Warning: Expected END, found a '(' [227] */
/* p2c: ../luk-1.0/mod/IRQ.mod:122:
 * Warning: Expected END, found a '(' [227] */
}
/* p2c: ../luk-1.0/mod/IRQ.mod:134: 
 * Warning: Expected a semicolon, found ELSE [227] */
/* p2c: ../luk-1.0/mod/IRQ.mod:136: 
 * Warning: Expected END, found 'WriteCard' [227] */


void _M2_IRQ_init(void);

void _M2_IRQ_init(void)
{
}
void _M2_IRQ_fini(void);

void _M2_IRQ_fini(void)
{
}
/* p2c: ../luk-1.0/mod/IRQ.mod:139: 
 * Warning: Expected a '.', found END [227] */
/* p2c: ../luk-1.0/mod/IRQ.mod:139: 
 * Warning: Junk at end of input file ignored [277] */


/* End. */
