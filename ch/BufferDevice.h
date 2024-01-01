#if !defined(BufferDeviceH)
#   define BufferDeviceH

/*
   Author      : Gaius Mulley
   Version     : 1.0
   Date        : 12/2/86
   Last Update : 2/8/86
   Description : Exports a general Read and Write Buffer procedure. Buffers
                 are dynamic.
*/


#if defined(BufferDeviceC)
#   define EXTERN
#else
#   define EXTERN extern
/* opaque type declaration */
typedef void Buffer;
#endif


/*
   InitBuffer - creates, and initializes, a buffer and returns it.
*/

EXTERN Buffer *BufferDevice_InitBuffer (void);


/*
   KillBuffer - destroys the buffer and relinquishes all associated
                resources.
*/

EXTERN void BufferDevice_KillBuffer (Buffer **b);


/*
   ReadBuffer - reads a character, ch, from the buffer, b.
*/

EXTERN void BufferDevice_ReadBuffer (Buffer *b, char *ch);


/*
   WriteBuffer - places a character, ch, into buffer, b.
*/

EXTERN void BufferDevice_WriteBuffer (Buffer *b, char ch);

#undef EXTERN

#endif

