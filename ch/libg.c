/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/libg.mod" */


#include <p2c/p2c.h>


#define libgG
#include "Glibg.h"

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif

/*
    Title      : libg
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Fri Sep  9 08:12:01 1994
    Last edit  : Fri Sep  9 08:12:01 1994
    Description: Contains a very simple polling Write and interrupt driven
                 Read routine which is used by gdb.
*/

#ifndef PortIOH
#include "GPortIO.h"
#endif

#ifndef DeviceConfigurationH
#include "GDeviceConfiguration.h"
#endif

#ifndef IRQH
#include "GIRQ.h"
#endif

#ifndef gdbH
#include "Ggdb.h"
#endif

#ifndef M2RTSH
#include "GM2RTS.h"
#endif

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


#define ControlC        '\003'


/* Set up constants for the ports that are accessed.          */

/* the following constant declaration may be modified to change */
/* the module for the alternate communications adapter.         */

#define Com1            1   /* = 1 means TRUE  or COM1 is used */
/* = 0 means FALSE or COM2 is used */

#define IrqBase         0x20   /* irq interrupts live from 020..030 */

#define IOBase          ((Com1 * 0x100) + 0x2f8)

#define LineContrReg    (IOBase + 0x3)
    /* to specify format of transmitted data  */
#define LowBaudRateDiv  IOBase   /* lower byte of divisor                  */
#define HighBaudRateDiv  (IOBase + 0x1)
    /* higher byte of divisor                 */
#define LineStatusReg   (IOBase + 0x5)
    /* holds status info on the data transfer */
#define ReceiverReg     IOBase   /* received char is in this register      */
#define TransmitReg     IOBase   /* char to send is to put in this reg     */
#define IntEnableReg    (IOBase + 0x1)
    /* to enable the selected interrupt       */
#define ModemContrReg   (IOBase + 0x4)
    /* controls the interface to a modem      */
#define IntIdentReg     (IOBase + 0x2)
    /* Determines what caused interrupt.      */
#define ModemStatusReg  (IOBase + 0x6)
    /* Determines status on the ctrl lines.   */
#define ScratchReg      (IOBase + 0x7)
    /* Only available on some UARTs           */
#define FifoControlReg  (IOBase + 0x2)
    /* Only available on some UARTs           */

/* Line Status Registers Bits */

#define DataReadyBit    0
#define OverrunErrorBit  0x1
#define ParityErrorBit  0x2
#define FramingErrorBit  0x3
#define BreakIndicatorBit  0x4
#define TxHoldingRegEmptyBit  0x5
#define TxShiftRegEmptyBit  0x6
#define FifoErrorBit    0x7

/* Interrupt Enable Register Bits */

#define EnableDataAvailableInt  0
#define EnableTxHoldingRegEmptyInt  0x1
#define EnableModemStatusInt  0x3

/* Interrupt identification register values */

#define NoInterrupt     0x1
#define CharacterErrorInterrupt  0x6   /* OE, PE, FE, BI    */
#define DataReadyInterrupt  0x4   /* character waiting */
#define DataFifoInterrupt  0xc   /* no fifo action?   */
#define TransmitInterrupt  0x2
#define ModemInterrupt  0   /* must read msr     */


typedef enum {
  UartUnknown, Uart8250, Uart16450, Uart16550, Uart16550A
} TypeOfUART;


Static unsigned int IrqNo;
Static TypeOfUART ThisUart;


Static void DLAB(unsigned int Bit)
{
  int Status;
  Char Byte;

  Byte = PortIO_In8(LineContrReg);
  Status = *(int *)(&Byte);
  if (Bit == 1)
    Status |= 1L << 7;
  else
    Status &= ~(1L << 7);
  PortIO_Out8(LineContrReg, *(uchar *)(&Status));
}


/*
   InitDevice - Initializes the serial port to specific values.
                The legal values for the parameters are:
                    BaudRate  : 300..9600
                    StopBits  : 1..2
                    WordLength: 5..8
*/

Static BOOLEAN InitDevice(unsigned int BaudRate, unsigned int StopBits,
			  unsigned int WordLength,
			  DeviceConfiguration_Parity p)
{
  uchar DivisorLow, DivisorHigh;
  int Parameters;

  switch (BaudRate) {

  case 300:
    DivisorLow = 0x80;
    DivisorHigh = 0x1;
    break;

  case 600:
    DivisorLow = 0xc0;
    DivisorHigh = 0;
    break;

  case 1200:
    DivisorLow = 0x60;
    DivisorHigh = 0;
    break;

  case 2400:
    DivisorLow = 0x30;
    DivisorHigh = 0;
    break;

  case 4800:
    DivisorLow = 0x18;
    DivisorHigh = 0;
    break;

  case 9600:
    DivisorLow = "\0";
    break;

/* p2c: ../luk-1.0/mod/libg.mod:134:
 * Warning: Expected a semicolon, found 'H' [227] */
  case H:
/* p2c: ../luk-1.0/mod/libg.mod:134:
 * Warning: Symbol 'H' is not defined [221] */
/* p2c: ../luk-1.0/mod/libg.mod:134:
 * Warning: Expected a colon, found a semicolon [227] */
    DivisorHigh = 0;
    break;

  case 19200:
    DivisorLow = 0x6;
    DivisorHigh = 0;
    break;

  case 38400L:
    DivisorLow = 0x3;
    DivisorHigh = 0;

    break;

  default:
    return FALSE;
    break;
  }

  /* load the Divisor of the baud rate generator: */
  DLAB(1);
  PortIO_Out8(HighBaudRateDiv, DivisorHigh);
  PortIO_Out8(LowBaudRateDiv, DivisorLow);
  DLAB(0);

  /* prepare the Parameters: */
  Parameters = 0;
  if (StopBits == 2)
    Parameters |= 1L << 2;
  else if (StopBits != 1)
    return FALSE;
  if ((p == DeviceConfiguration_Odd) || (p == DeviceConfiguration_Even)) {
    Parameters |= 1L << 3;
    if (p == DeviceConfiguration_Even)
      Parameters |= 1L << 4;
  }
  switch (WordLength) {

  case 5:
    break;

  case 6:
    Parameters |= 1L << 0;
    break;

  case 7:
    Parameters |= 1L << 1;
    break;

  case 8:
    Parameters |= 1L << 0;

    Parameters |= 1L << 1;
    break;

  default:
    return FALSE;
    break;
  }
  PortIO_Out8(LineContrReg, *(uchar *)(&Parameters));
  return TRUE;
}


/*
   DetermineUART - returns the type of UART.
                   Translated from the Serial FAQ document on the Internet.
*/

Static TypeOfUART DetermineUART(void)
{
  int BitSet;

  /* first step: see if the LCR is there */
/* p2c: ../luk-1.0/mod/libg.mod:196:
 * Warning: Expected a ')', found 'H' [227] */
  PortIO_Out8(LineContrReg, 01);
  if (PortIO_In8(LineContrReg) != ((uchar)01)) {
/* p2c: ../luk-1.0/mod/libg.mod:197:
 * Warning: Expected a ')', found 'H' [227] */
    return UartUnknown;
  }
  PortIO_Out8(LineContrReg, (uchar)0x3);
  if (PortIO_In8(LineContrReg) != ((uchar)0x3))
    return UartUnknown;
  /* next thing to do is look for the scratch register */
  PortIO_Out8(ScratchReg, 0x55);
  if (PortIO_In8(ScratchReg) != ((uchar)0x55))
    return Uart8250;
  PortIO_Out8(ScratchReg, 0xaa);
  if (PortIO_In8(ScratchReg) != ((uchar)0xaa))
    return Uart8250;
  /* then check if there is a FIFO */
  PortIO_Out8(FifoControlReg, 1);
  BitSet = *(int *)(&PortIO_In8(IntIdentReg));
  if ((0x80 & BitSet) == 0)
    return Uart16450;
  if ((0x40 & BitSet) == 0)
    return Uart16550;
  /* some old-fashioned software relies on this! */
  PortIO_Out8(IntIdentReg, 0);
  return Uart16550A;
}


/*
   SetUpCommPort - initializes the comm port for receive interrupts
                   only.
*/

Static void SetUpCommPort(void)
{
  int BitSet;

  ThisUart = DetermineUART();
  MonStrIO_WriteString(1L, 13L, "UART type is ");
  switch (ThisUart) {

  case UartUnknown:
    MonStrIO_WriteString(1L, 7L, "unknown");
    break;

  case Uart8250:
    MonStrIO_WriteString(1L, 7L, "8250   ");
    break;

  case Uart16450:
    MonStrIO_WriteString(1L, 7L, "16450  ");
    break;

  case Uart16550:
    MonStrIO_WriteString(1L, 7L, "16550  ");
    break;

  case Uart16550A:
    MonStrIO_WriteString(1L, 7L, "16550A ");
    break;

  default:
    MonStrIO_WriteString(1L, 26L, "<detection routine failed>");
    break;
  }

  /* Select the type of interrupts we require. */

  BitSet = 0;
  BitSet = *(int *)(&PortIO_In8(ModemContrReg));
  BitSet |= 1L << 3;   /* bit 3 must be set, otherwise no  */
  /* interrupts will be generated;    */
  /* see technical reference of       */
  /* IBM-PC, page 1-200               */
  /*
     INCL(BitSet, 0) ;      (* set DTR                          *)
     INCL(BitSet, 1) ;      (* set RTS - always keep active     *)
  */
  PortIO_Out8(ModemContrReg, *(uchar *)(&BitSet));

  /* Enable interrupts in the communications controller (8250):  */
  /*        Allow receiver interrupts and transmitter interrupts */
  /*        and Modem status interrupts. Specifically CTS.       */

  PortIO_Out8(IntEnableReg, *(uchar *)(&(1L << EnableDataAvailableInt)));
  IRQ_EnableIRQ(IrqNo);
  if (ThisUart == Uart16550A) {
    /* enable fifo: clear fifo set trigger at 14 bytes */
    PortIO_Out8(FifoControlReg, 0xc7);
  }
}


/*
   Write - exported user procedure which waits until a char can be sent
*/

Static void Write(Char ch)
{
/* p2c: ../luk-1.0/mod/libg.mod:295:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int ToOldState;
  int CopyOfLineStatusReg;

  ToOldState = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/libg.mod:298:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/libg.mod:298:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  /*   MonStrIO.WriteString('inside libg.Write') ; MonStrIO.WriteLn ; */
  /*   MonStrIO.WriteString('before linestatusreg ') ; */
  do {
    CopyOfLineStatusReg = *(int *)(&PortIO_In8(LineStatusReg));
    /*      ; MonStrIO.WriteBin(CARDINAL(CopyOfLineStatusReg), 8) ; MonStrIO.WriteLn ; */
  } while ((((unsigned long)TxHoldingRegEmptyBit) >= 32) ||
	   (((1L << TxHoldingRegEmptyBit) & CopyOfLineStatusReg) == 0));
  /*   MonStrIO.WriteString('before transmitreg') ; MonStrIO.WriteLn ; */
  PortIO_Out8(TransmitReg, ch);
  ToOldState = TurnInterrupts(ToOldState);
  /* ; MonStrIO.WriteString('finished Write') ; MonStrIO.WriteLn ; */
/* p2c: ../luk-1.0/mod/libg.mod:309:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
}


#define MaxBuffer       1024


Static Char buff[MaxBuffer + 1];
Static unsigned int count, in, out;


/*
   Read - exported user procedure which waits until a
          character is ready in the buffer.
*/

Static void Read(Char *ch)
{
/* p2c: ../luk-1.0/mod/libg.mod:327:
 * Warning: Symbol 'OnOrOff' is not defined [221] */
  int ToOldState;
  int CopyOfLineStatusReg;

  ToOldState = TurnInterrupts(Off);
/* p2c: ../luk-1.0/mod/libg.mod:330:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/libg.mod:330:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
  if (count > 0) {
    count--;
    *ch = buff[out];
    out = (out + 1) & (MaxBuffer - 1);
  } else {
    /* nothing in the buffer wait for something to arrive */
    do {
      CopyOfLineStatusReg = *(int *)(&PortIO_In8(LineStatusReg));
    } while ((((unsigned long)DataReadyBit) >= 32) ||
	     (((1L << DataReadyBit) & CopyOfLineStatusReg) == 0));
    *ch = PortIO_In8(ReceiverReg);
  }
  /* something in the buffer - use that */
  ToOldState = TurnInterrupts(ToOldState);
/* p2c: ../luk-1.0/mod/libg.mod:345:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
}


/* MonStrIO.WriteString('no interrupt') ; */
/* MonStrIO.WriteString('pe fe oe ') */
/* MonStrIO.WriteString('tx interrupt') ; */
/* serviced by reading IIR, done above */
/* MonStrIO.WriteString('modem interrupt') ; */
/* MonStrIO.WriteString('DR interrupt') ; */
/* ^C typed - empty the buff and call breakpoint */
/* ../luk-1.0/mod/libg.mod, line 380: undefined symbol InitBuffer */
/* Translation aborted. */
--------------------------
