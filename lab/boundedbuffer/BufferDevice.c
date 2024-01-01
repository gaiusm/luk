
#include "../../ch/luk-types.h"
#include "../../ch/SYSTEM.h"
#include "../../ch/Storage.h"
#include "../../ch/Debug.h"
#include "../../ch/Executive.h"


#define MaxBufferSize   0x10

typedef struct Buffer {
  Semaphore *ItemAvailable, *SpaceAvailable, *Mutex;
  char Buf[MaxBufferSize + 1];
  unsigned int in, out;
} Buffer;

#define BufferDeviceC
#include "../../ch/BufferDevice.h"

#define SafeStr(X)  X, strlen(X)


/*
 *  simple utility function
 */

static int strlen (char *s)
{
  int i=0;

  while (s[i] != (char)0) {
    i++;
  }
  return i;
}


/*
   InitBuffer - creates, and initializes, a buffer and returns it.
*/


Buffer *BufferDevice_InitBuffer (void)
{
  Buffer *b;

  Debug_DebugString(SafeStr("InitBuffer\\n"));
  Storage_ALLOCATE((void **)&b, sizeof(Buffer));                 /* remove for student */
  if (b == NULL)                                                 /* remove for student */
    Debug_Halt(SafeStr("out of memory error"), __LINE__, SafeStr(__FILE__));             /* remove for student */

  b->ItemAvailable = Executive_InitSemaphore(0, SafeStr("ItemAvailable"));               /* remove for student */
  b->SpaceAvailable = Executive_InitSemaphore(MaxBufferSize, SafeStr("SpaceAvailable")); /* remove for student */
  b->Mutex = Executive_InitSemaphore(1, SafeStr("Mutex"));       /* remove for student */
  b->in = 0;                                                     /* remove for student */
  b->out = 0;                                                    /* remove for student */
  return b;
}


/*
   KillBuffer - destroys the buffer and relinquishes all associated
                resources.
*/

void BufferDevice_KillBuffer (Buffer **b)
{
  /*
     when we implement KillSemaphore
        Executive_KillSemaphore( ItemAvailable ) ;
        Executive_KillSemaphore( SpaceAvailable ) ;
        Executive_KillSemaphore( Mutex )

        Free(*b);
  */
}


/*
   ReadBuffer - reads a character, ch, from the buffer, b.
*/

void BufferDevice_ReadBuffer (Buffer *b, char *ch)
{
  Executive_Wait(b->ItemAvailable);          /* remove for student */
  Executive_Wait(b->Mutex);                  /* remove for student */
  *ch = b->Buf[b->out];                      /* remove for student */
  b->out = (b->out + 1) % MaxBufferSize;     /* remove for student */
  Executive_Signal(b->Mutex);                /* remove for student */
  Executive_Signal(b->SpaceAvailable);       /* remove for student */
}


/*
   WriteBuffer - places a character, ch, into buffer, b.
*/

void BufferDevice_WriteBuffer (Buffer *b, char ch)
{
  Executive_Wait(b->SpaceAvailable);
  Executive_Wait(b->Mutex);
  b->Buf[b->in] = ch;
  b->in = (b->in + 1) % MaxBufferSize;
  Executive_Signal(b->Mutex);
  Executive_Signal(b->ItemAvailable);
}


void _M2_BufferDevice_init (void)
{
}

void _M2_BufferDevice_finish (void)
{
}
