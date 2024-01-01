
#if !defined(StdIOH)
#  define StdIOH

/*
   Author     : Gaius Mulley
   Title      : StdIO
   Date       : 12/2/86  [$Date: 1997-11-03 17:21:43 $]
   SYSTEM     : UNIX SUN and Logitech M2
   Description: Exports a general Read and Write procedure that ALL character
                processes should use.
   Version    : $Revision: 1.4 $
*/

typedef void (*StdIO_ProcWrite)(char);
typedef void (*StdIO_ProcRead)(char *);


/*
   Read - is the generic procedure that all higher application layers
          should use to receive a character.
*/


extern void StdIO_Read (char *ch);


/*
   Write - is the generic procedure that all higher application layers
           should use to emit a character.
*/

extern void StdIO_Write (char ch);


/*
   PushOutput - pushes the current Write procedure onto a stack,
                any future references to Write will actually invoke
                procedure, p.
*/

extern void StdIO_PushOutput (void (*p)(char));


/*
   PopOutput - restores Write to use the previous output procedure.
*/

extern void StdIO_PopOutput (void);


/*
   GetCurrentOutput - returns the current output procedure.
*/

extern void *StdIO_GetCurrentOutput (void);


/*
   Init - initialize this module data structures.
          This is only made available for M2RTS.
*/

extern void StdIO_Init (void);

#endif

