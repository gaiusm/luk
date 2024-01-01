/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/TTIO.mod" */


#include <p2c/p2c.h>


#define TTIOG
#include "GTTIO.h"

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif


/*
   Sadly by introducing the LEDs this module has become
   more complex. If only the LEDs did not require ack/naks..
*/


#ifndef PortIOH
#include "GPortIO.h"
#endif


#ifndef KeyBoardConvertH
#include "GKeyBoardConvert.h"
#endif

#ifndef DebugH
#include "GDebug.h"
#endif


/*
   Import concurrent process control functions from Executive.
*/

#ifndef ExecutiveH
#include "GExecutive.h"
#endif

#ifndef BufferDeviceH
#include "GBufferDevice.h"
#endif


#define StackArea       10000   /* Allows StackArea BYTES per PROCESS. */
#define MaxLEDBytes     2   /* max bytes that can be outstanding   */
/* at any time. LED control bytes      */
#define MaxNegAcks      4   /* how many times do we try resending? */


#define KbdCtrl         0x61
#define KbdPort         0x60
#define KbdStatus       0x64
#define KbdCmd          0x64


Static uchar ToKeyboard[MaxLEDBytes + 1];
Static unsigned int OutstandingLEDs;   /* no of bytes to be sent     */
Static unsigned int SentBytes;   /* bytes already sent         */
Static unsigned int EndIndex;   /* index to the end of buffer */
Static unsigned int NegativeAckCount;   /* number of naks for byte    */
Static BOOLEAN OldScroll, OldNum, OldCaps;


#define IntNo           0x21   /* IRQ 1 */


/* p2c: ../luk-1.0/mod/TTIO.mod:66:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/TTIO.mod:66:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
/* ../luk-1.0/mod/TTIO.mod, line 67: undefined symbol SendCommandToKeyboard */
/* Translation aborted. */
--------------------------
