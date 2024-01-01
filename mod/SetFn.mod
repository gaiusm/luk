IMPLEMENTATION MODULE SetFn ;


FROM Environment IMPORT GetEnvironment ;

FROM CmdArgs IMPORT GetArg, Narg ;

FROM StrIO IMPORT WriteString, WriteLn ;

FROM NumberIO IMPORT StrToCard ;

FROM KeyBoardConvert IMPORT SetFunctionString ;


CONST
   MaxStrLen = 70 ;


(*
   DoSetFn - sets the function key, Function, to the string, a.
             This string may contain ^x or ^X which is interpreted as
             <Ctrl> X.
*)

PROCEDURE DoSetFn ;
VAR
   CommandLine,
   a          : ARRAY [0..MaxStrLen] OF CHAR ; 
   Key        : CARDINAL ;
BEGIN
   GetEnvironment('cmd', CommandLine) ;
   IF Narg(CommandLine)=2
   THEN
      IF GetArg(CommandLine, 1, a)
      THEN
         StrToCard(a, Key) ;
         IF GetArg(CommandLine, 2, a)
         THEN
            SetFunctionString(Key, a)
         END
      END
   ELSE
      WriteString('Usage: setfn number string') ; WriteLn
   END
END DoSetFn ;


END SetFn.
