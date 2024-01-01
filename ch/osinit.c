/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/osinit.mod" */


#include <p2c/p2c.h>


#define osinitG
#include "Gosinit.h"
/* p2c: ../luk-1.0/mod/osinit.mod:3: 
 * Warning: Could not find module OSParameters [271] */

#include "GOSParameters.h"

#ifndef SYSTEMH
#include "GSYSTEM.h"
#endif




#define KiloByte        1024
#define Meg1            (KiloByte * KiloByte)
#define BytesPerSector  512


/* ***************************************************
(*
   GetKey - waits for a key to be pressed.
*)

PROCEDURE GetKey ;
BEGIN
   ASM
      pushl %eax
      pushl %ebx
again:
      in    $0x60, %al     # get scan code
      movb  %al, %ah       # store scan code
      in    $0x61, %al     # send ack to kbd controller
      orb    $0x80, %al
      out   %al, $0x61
      andb  $0x7f, %al
      out   %al, $0x61
      cmpb  $0xf0, %ah     # this byte prefaces key releases
      je   finish          # key released

      # discard scan code of released key
      in    $0x61, %al     # send ack to kbd controller
      orb   $0x80, %al
      out   %al, $0x61
      andb  $0x7f, %al
      out   %al, $0x61
      jmp   again
finish:
      popl %eax
      popl %ebx
   END
END GetKey ;
********************************************************** */

/*
   Get16 - returns a CARDINAL whose value is taken from a
           2 byte entity at address, a.
*/

Static unsigned int Get16(void *a)
{
  uchar *shortPtr;
  unsigned int result;

  shortPtr = a;
  result = (unsigned int)(*shortPtr);
  shortPtr++;
  result += ((unsigned int)(*shortPtr)) * 0x100;
  return result;
}


/*
   Put16 - places a 2 byte entity into address, a.
*/

Static void Put16(void *a, unsigned int c)
{
  uchar *shortPtr;

  shortPtr = a;
  *shortPtr = (uchar)(c & 0xff);
  shortPtr++;
  *shortPtr = (uchar)(c / 0x100);
}


/*
   RetrieveOSParameters - collects values needed by the realtime
                          system.
*/

Static void RetrieveOSParameters(unsigned int *ExtendedMem,
  unsigned int *Video, unsigned int *Size, unsigned int *NoOfSectors,
  unsigned int *Debugging, unsigned int *StackSize)
{
  *ExtendedMem = Get16(ExtendedMemAddr);
/* p2c: ../luk-1.0/mod/osinit.mod:93:
 * Warning: Symbol 'ExtendedMemAddr' is not defined [221] */
  *Video = Get16(VideoAddrAddr);
/* p2c: ../luk-1.0/mod/osinit.mod:94:
 * Warning: Symbol 'VideoAddrAddr' is not defined [221] */
  *Size = Get16(OSSizeAddr);
/* p2c: ../luk-1.0/mod/osinit.mod:95:
 * Warning: Symbol 'OSSizeAddr' is not defined [221] */
  *NoOfSectors = Get16(NoOfSectorsAddr);
/* p2c: ../luk-1.0/mod/osinit.mod:96:
 * Warning: Symbol 'NoOfSectorsAddr' is not defined [221] */
  *Debugging = Get16(DebuggingAddr);
/* p2c: ../luk-1.0/mod/osinit.mod:97:
 * Warning: Symbol 'DebuggingAddr' is not defined [221] */
  *StackSize = Get16(StackSizeAddr);
/* p2c: ../luk-1.0/mod/osinit.mod:98:
 * Warning: Symbol 'StackSizeAddr' is not defined [221] */
}


/*
   ScreenAddress - returns the start address of the video memory.
*/

Static void *(ScreenAddress(void))
{
  return ((void *)Video);
/* p2c: ../luk-1.0/mod/osinit.mod:108:
 * Warning: Symbol 'Video' is not defined [221] */
}


/*
   ExtendedMemoryEnd - returns the maximum extended memory address.
*/

Static void *(ExtendedMemoryEnd(void))
{
  return ((void *)ExtendedMem);
/* p2c: ../luk-1.0/mod/osinit.mod:118:
 * Warning: Symbol 'ExtendedMem' is not defined [221] */
}


/*
   ExtendedMemoryStart - returns the start of the extended memory.
*/

Static void *(ExtendedMemoryStart(void))
{
  return ((void *)Meg1);
}


/*
   BaseMemoryEnd - returns the maximum base memory address.
*/

Static void *(BaseMemoryEnd(void))
{
  return ((void *)655360);
}


/*
   BaseMemoryStart - returns the start of the base memory.
*/

Static void *(BaseMemoryStart(void))
{
  return ((void *)OSSize);   /* should add the data size when we know it */
/* p2c: ../luk-1.0/mod/osinit.mod:148:
 * Warning: Symbol 'OSSize' is not defined [221] */
}


/*
   SizeOfOS - returns the size of the realtime system.
*/

Static unsigned int SizeOfOS(void)
{
  return (OSSize);
/* p2c: ../luk-1.0/mod/osinit.mod:158:
 * Warning: Symbol 'OSSize' is not defined [221] */
}


/*
   DebuggingWithGDB - returns TRUE if we are remote debugging with gdb.
*/

Static BOOLEAN DebuggingWithGDB(void)
{
  return (Debugging != 0);
/* p2c: ../luk-1.0/mod/osinit.mod:168:
 * Warning: Symbol 'Debugging' is not defined [221] */
}
/* ../luk-1.0/mod/osinit.mod, line 178: undefined symbol Debugging */
/* Translation aborted. */
--------------------------
