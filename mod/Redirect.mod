IMPLEMENTATION MODULE Redirect ;


FROM StreamDev IMPORT SetStream ;

FROM IO IMPORT STREAM ;

FROM Kernel IMPORT PtrToProcDes ;

FROM ProcessName IMPORT GetProcessName ;

FROM StrIO IMPORT WriteString, WriteLn ;

FROM Environment IMPORT GetEnvironment ;

FROM CmdArgs IMPORT GetArg, Narg ;

FROM StrLib IMPORT StrEqual ;


CONST
   MaxStrLen = 70 ;


PROCEDURE DoRedirect ;
VAR
   CommandLine,
   a          : ARRAY [0..MaxStrLen] OF CHAR ;
   p          : PtrToProcDes ;
BEGIN
   GetEnvironment('cmd', CommandLine) ;
   WriteString(CommandLine) ; WriteLn ;
   IF GetArg(CommandLine, 0, a) AND StrEqual(a, 'redirect')
   THEN
      IF GetArg(CommandLine, 1, a) AND GetProcessName(p, a)
      THEN
         SetStream(p^.PStream, CommandLine)
      ELSE
         WriteString('Process ') ; WriteString(a) ; WriteString(' not found') ;
         WriteLn
      END
   ELSE
      WriteString('Usage: redirect processname < stdin > stdout') ; WriteLn
   END
END DoRedirect ;


END Redirect.
