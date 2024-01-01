
#if !defined(MonStrIOH)
#   define MonStrIOH

/*
   Title      : MonStrIO
   Author     : Gaius Mulley
   Date       : 16/8/87
   LastEdit   : 12/8/94
   System     : LOGITECH MODULA-2/86 & (RTS GM2)
   Description: Provides a method of writting text to a screen that is
                unbuffered and process independant.
*/

#if !defined(StdIOH)
#   include "StdIO.h"
#endif


extern void MonStrIO_DebuggingStream (void (*p)(char));

extern void MonStrIO_WriteLn (void);

extern void MonStrIO_WriteString (const int a_LOW, const int a_HIGH, const char *a);

extern void MonStrIO_WriteCard (unsigned int x, unsigned int n);

extern void MonStrIO_WriteHex (unsigned int x, unsigned int n);

extern void MonStrIO_WriteBin (unsigned int x, unsigned int n);

extern void MonStrIO_WriteInt (int x, unsigned int n);


/*
   Init - sets the DebuggingStream to use Scn.Write.
          Should only be called by M2RTS.
*/

extern void MonStrIO_Init (void);


#endif
