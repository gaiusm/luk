#if !defined(DebugH)
#   define DebugH

/*
    Title      : Debug
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Sat Aug 13 19:41:57 1994
    Last edit  : Sat Aug 13 19:41:57 1994
    Description: provides some simple debugging routines.
*/


/*
   Halt - writes a message in the format:
          filename:line:message

          to the debugging device. (Scn.Write).
          It then terminates by looping forever.
*/

extern void Debug_Halt (char *message, unsigned int high_message,
			unsigned int line_no,
			char *filename, unsigned int high_filename);


/*
   DebugString - writes a string to the debugging device (Scn.Write).
                 It interprets \n as carriage return, linefeed.
*/

extern void Debug_DebugString (char *a, unsigned int high);


/*
   DebugCString - writes a string to the debugging device (Scn.Write).
                  It interprets \n as carriage return, linefeed.
*/

extern void Debug_DebugCString (char *a);


#endif

