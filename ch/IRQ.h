#if !defined(IRQH)
#   define IRQH

/*
    Title      : IRQ
    Author     : Gaius Mulley
    System     : RTS (gm2)
    Date       : Tue Aug  9 12:11:54 1994
    Last edit  : Tue Aug  9 12:11:54 1994
    Description: provides a simple interface to the IRQ hardware.
*/

#define IRQ_MaxIRQ      15

typedef void (*ProcIRQ)(unsigned int);


/*
   InitIRQ - initialises irq, IrqNo, to call PROCEDURE, p,
             when this interrupt occurs.
*/


extern void IRQ_InitIRQ (unsigned int IrqNo, void (*p)(unsigned int));


/*
   KillIRQ - removes an IRQ handler.
*/

extern void IRQ_KillIRQ (unsigned int IrqNo);


/*
   EnableIRQ - enable irq, IrqNo.
*/

extern void IRQ_EnableIRQ (unsigned int IrqNo);


/*
   DisableIRQ - disable irq, IrqNo.
*/

extern void IRQ_DisableIRQ (unsigned int IrqNo);


/*
   Init - initializes the module data structures and assigns default
          irq handlers.
*/

extern void IRQ_Init (void);


#endif

