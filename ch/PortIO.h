
#if !defined(PortIOH)
#define PortIOH


#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif

/*
    Title      : PortIO
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Tue Jul 23 15:55:15 1996
    Last edit  : Tue Jul 23 15:55:15 1996
    Description: provides a separate module for In and Out instructions.
*/


/*
   SlowDownIO - linux mechanism for slowing down port accesses.
*/

extern void PortIO_SlowDownIO (void);


/*
   In8 - returns a BYTE from port, Port.
*/

extern uchar PortIO_In8 (unsigned int Port);


/*
   Out8 - sends a byte, Value, to port, Port.
*/

extern void PortIO_Out8 (unsigned int Port, uchar Value);


/*
   In16 - returns a WORD from port, Port.
          The top 16 bits are 0, the bottom 16 bits are assigned from
          the value of the port.
*/

extern int PortIO_In16 (unsigned int Port);


/*
   Out16 - sends a 16 bit value, Value, to port, Port.
           Value is actually a 32 bit entity but the top
           16 bits are ignored.
*/

extern void PortIO_Out16 (unsigned int Port, int Value);


/*
   InS8 - reads, n, bytes in from port, Port, to address, a.
*/

extern void PortIO_InS8 (unsigned int Port, void *a, unsigned int n);


/*
   InS16 - reads, n, 16 bit words in from port, Port, to address, a.
*/

extern void PortIO_InS16 (unsigned int Port, void *a, unsigned int n);


/*
   InS32 - reads, n, 32 bit words in from port, Port, to address, a.
*/

extern void PortIO_InS32 (unsigned int Port, void *a, unsigned int n);


/*
   OutS8 - writes, n, bytes to port, Port, from address, a.
*/

extern void PortIO_OutS8 (unsigned int Port, void *a, unsigned int n);


/*
   OutS16 - writes, n, 16 bit words to port, Port, from address, a.
*/

extern void PortIO_OutS16 (unsigned int Port, void *a, unsigned int n);


/*
   OutS32 - writes, n, 32 bit words to port, Port, from address, a.
*/

extern void PortIO_OutS32 (unsigned int Port, void *a, unsigned int n);


#endif
