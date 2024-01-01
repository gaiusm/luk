/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/DeviceConfiguration.mod" */


#include <p2c/p2c.h>


#define DeviceConfigurationG
#include "GDeviceConfiguration.h"

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif



#ifndef PortIOH
#include "GPortIO.h"
#endif


#define LineContrReg    3


/*
   Taken from the Serial documentation gathered from the net.

   This useful function has been provided by Mike Surikov; it allows you to
   detect which interrupt is used by a certain UART.
*/
/* p2c: ../luk-1.0/mod/DeviceConfiguration.mod:42:
 * Warning: Symbol 'Off' is not defined [221] */
/* p2c: ../luk-1.0/mod/DeviceConfiguration.mod:42:
 * Warning: Symbol 'TurnInterrupts' is not defined [221] */
/* disable CPU interrupts */

/* read lcr               */
/* ../luk-1.0/mod/DeviceConfiguration.mod, line 45: undefined symbol DLAB */
/* Translation aborted. */
--------------------------
