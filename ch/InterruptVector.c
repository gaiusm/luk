/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/InterruptVector.mod" */


#include <p2c/p2c.h>


#define InterruptVectorG
#include "GInterruptVector.h"
/* p2c: ../luk-1.0/def/Descriptors.def:15: 
 * Warning: Could not find module WordSizes [271] */

#ifndef DescriptorsH
#include "GDescriptors.h"
#endif

#ifndef DumpH
#include "GDump.h"
#endif
/* p2c: ../luk-1.0/mod/InterruptVector.mod:5: 
 * Warning: Could not find module WordSizes [271] */

#include "GWordSizes.h"

#ifndef SYSTEMH
#include "GSYSTEM.h"
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


#define CodeSegmentSelector  8
#define MaxInterrupt    255
#define CodeInterruptId  20   /* byte 20 of interrupt code contains id   */
#define CodeProcedureId  25   /* byte 25 of interrupt code contains proc */
#define MaxCodeBytes    39   /* number of bytes in our hand coded ISRs  */


typedef struct ISR {
/* p2c: ../luk-1.0/mod/InterruptVector.mod:19:
 * Warning: Symbol 'IsrCode' is not defined [221] */
  int Code;
  BOOLEAN Used;
} ISR;

typedef int IsrCode;


/* p2c: ../luk-1.0/mod/InterruptVector.mod:23:
 * Warning: Symbol 'IsrCode' was already defined [220] */
typedef uchar IsrCode_[MaxCodeBytes + 1];


Static ISR InterruptIsr[MaxInterrupt + 1];
Static IsrCode_ IsrTemplate;
/* p2c: ../luk-1.0/mod/InterruptVector.mod:37: 
 * Warning: Symbol 'ProcISR' is not defined [221] */



/*
   ClaimIsr - claims the interrupt vector, Vector, and assigns the
              PROCEDURE, p, to be called when this interrupt fires.
              Vector should be between 0..255.
*/

Static void *(ClaimIsr(unsigned int Vector, int p))
{
  int *PtrToP;
  ISR *WITH;

  WITH = &InterruptIsr[Vector];
  if (!WITH->Used) {
    WITH->Used = TRUE;
    WITH->Code = IsrTemplate;
    /* now fill in Vector and p */
    WITH->Code[CodeInterruptId] = (uchar)Vector;
/* p2c: ../luk-1.0/mod/InterruptVector.mod:50:
 * Warning: Index on a non-array variable [287] */
    PtrToP = &WITH->Code[CodeProcedureId];
/* p2c: ../luk-1.0/mod/InterruptVector.mod:51:
 * Warning: Index on a non-array variable [287] */
    *PtrToP = p;

    return (&WITH->Code);
  }
  MonStrIO_WriteString(1L, 47L,
		       "interrupt service routine code already in use..");
  M2RTS_HALT(-1);
}
/* p2c: ../luk-1.0/mod/InterruptVector.mod:65: 
 * Warning: Symbol 'ProcISR' is not defined [221] */
/* ../luk-1.0/mod/InterruptVector.mod, line 70: undefined symbol InstallIsr */
/* Translation aborted. */
--------------------------
