
#if !defined(KeyBoardConvertH)
#   define KeyBoardConvertH

#   if !defined(StdIOH)
#       include "StdIO.h"
#   endif


typedef void (*ConsoleSwitchProc)(unsigned int);


extern void KeyBoardConvert_ScanToASCII (void (*ReadDeliver)(char), char ch,
					 void (*ConsoleSwitch)(unsigned int));


/*
   SetFunctionString - sets a function key to deliver a string, a,
                       when pressed.
*/

extern void KeyBoardConvert_SetFunctionString (unsigned int Function,
					       const int a_LOW, const int a_HIGH, const char *a);


#endif

