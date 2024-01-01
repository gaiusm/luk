IMPLEMENTATION MODULE Mon ;


FROM Environment IMPORT SetEnvironment, GetEnvironment ;

FROM CmdArgs IMPORT GetArg ;

FROM Kernel IMPORT Suspend, Resume, ClockFreq, KillProcess,
                   GetPriority, PutPriority, MaxPriority,
                   RunQueue,
                   ProcType, ProcStatus, PtrToProcDes, CurrentProcess,
                   SystemProcessQueue, UserProcessQueue, IdlePd,
                   SystemRunQueue, UserRunQueue, OS ;

FROM StdIO IMPORT Read, Write ;
FROM StrIO IMPORT ReadString, WriteString, WriteLn ;
FROM StrLib IMPORT StrLen, StrEqual, StrCopy ;
FROM NumberIO IMPORT WriteCard, WriteHex, StrToCard ;

IMPORT Scn ;
IMPORT TTIO ;

FROM Scn IMPORT ClearScreen, MoveCursor ;
FROM IO IMPORT STREAM, NewStream, SwapStream, InitStream, DisposeStream ;
FROM Stty IMPORT DoStty ;
FROM Redirect IMPORT DoRedirect ;
FROM StreamDev IMPORT LsDev ;
FROM Ps IMPORT DoPs ;
FROM SetFn IMPORT DoSetFn ;



CONST
   MaxStrLen = 70 ;

VAR
   MonStream: STREAM ;


PROCEDURE Interpret ;
VAR
   CommandLine,
   a          : ARRAY [0..MaxStrLen] OF CHAR ; 
BEGIN
   SwapStream(CurrentProcess^.PStream, MonStream) ;
   ClearScreen ;
   MoveCursor( 0, 0 ) ;
   REPEAT
      GetSymbol(CommandLine) ;
      SetEnvironment('cmd', CommandLine) ;
      IF GetArg(CommandLine, 0, a)
      THEN
         IF StrEqual(a, 'suspend')
         THEN
            suspend
         ELSIF StrEqual(a, 'resume')
         THEN
            resume
         ELSIF StrEqual(a, 'rename')
         THEN
            rename
         ELSIF StrEqual(a, 'pri')
         THEN
            pri
         ELSIF StrEqual(a, 'kill')
         THEN
            kill
         ELSIF StrEqual(a, 'ps')
         THEN
            DoPs
         ELSIF StrEqual(a, 'stty')
         THEN
            DoStty
         ELSIF StrEqual(a, 'redirect')
         THEN
            DoRedirect
         ELSIF StrEqual(a, 'setfn')
         THEN
            DoSetFn
         ELSIF StrEqual(a, 'ls')
         THEN
            LsDev
         ELSIF StrEqual(a, 'help') OR StrEqual(a, '?')
         THEN
            help
         ELSIF StrEqual(a, 'exit')
         THEN
         ELSIF NOT StrEqual(a, '')
         THEN
            WriteString(a) ; WriteString(' :not found') ; WriteLn
         END ;
      END
   UNTIL StrEqual(a, 'exit') ;
   SwapStream(CurrentProcess^.PStream, MonStream)
END Interpret ;


PROCEDURE help ;
BEGIN
   WriteString('Commands are:') ; WriteLn ;
   WriteString('help or ?   : This!') ; WriteLn ;
   WriteString('ps          : Process Status') ; WriteLn ;
   WriteString('redirect    : change process input/output') ; WriteLn ;
   WriteString('stty        : change serial port parameters') ; WriteLn ;
   WriteString('setfn       : set a function key to generate a string') ; WriteLn ;
   WriteString('suspend     : Suspend process') ; WriteLn ;
   WriteString('resume      : Resume process') ; WriteLn ;
   WriteString('pri         : Alter process priority') ; WriteLn ;
   WriteString('rename      : Rename a process') ; WriteLn ;
   WriteString('quanta      : Alter a process quanta') ; WriteLn ;
   WriteString('ls          : displays the directory /dev') ; WriteLn ;
   WriteString('exit        : Exit this Interpreter') ; WriteLn
END help ;


PROCEDURE GetProc (a: ARRAY OF CHAR) : PtrToProcDes ;
VAR
   p,
   t: PtrToProcDes ;
BEGIN
   p := NIL ;
   t := SystemProcessQueue ;
   WHILE (p=NIL) AND (t#NIL) DO
      IF StrEqual(t^.PName, a)
      THEN
         p := t
      ELSE
         t := t^.RightPtr ;
         IF t=SystemProcessQueue
         THEN
            t := NIL
         END
      END
   END ;
   t := UserProcessQueue ;
   WHILE (p=NIL) AND (t#NIL) DO
      IF StrEqual(t^.PName, a)
      THEN
         p := t
      ELSE
         t := t^.RightPtr ;
         IF t=UserProcessQueue
         THEN
            t := NIL
         END
      END
   END ;
   RETURN( p )
END GetProc ;


PROCEDURE kill ;
VAR
   a: ARRAY [0..MaxStrLen] OF CHAR ;
   p: PtrToProcDes ;
   ps: ProcStatus ;
BEGIN
   GetEnvironment('cmd', a) ;
   IF GetArg(a, 1, a)
   THEN
      p := GetProc( a ) ;
      IF p=NIL
      THEN
         WriteString('Process not found') ;
         WriteLn
      ELSE
         KillProcess( p )
      END
   ELSE
      WriteString('Usage: kill processname') ; WriteLn
   END
END kill ;


PROCEDURE suspend ;
VAR
   a: ARRAY [0..MaxStrLen] OF CHAR ;
   p: PtrToProcDes ;
   ps: ProcStatus ;
BEGIN
   GetEnvironment('cmd', a) ;
   IF GetArg(a, 1, a)
   THEN
      p := GetProc( a ) ;
      IF p=NIL
      THEN
         WriteString('Process not found') ;
         WriteLn
      ELSE
         Suspend( p )
      END
   ELSE
      WriteString('Usage: suspend processname') ; WriteLn
   END
END suspend ;


PROCEDURE resume ;
VAR
   a: ARRAY [0..MaxStrLen] OF CHAR ;
   p: PtrToProcDes ;
   ps: ProcStatus ;
BEGIN
   GetEnvironment('cmd', a) ;
   IF GetArg(a, 1, a)
   THEN
      p := GetProc( a ) ;
      IF p=NIL
      THEN
         WriteString('Process not found') ;
         WriteLn
      ELSE
         Resume( p )
      END
   ELSE
      WriteString('Usage: suspend processname') ; WriteLn
   END
END resume ;


PROCEDURE rename ;
VAR
   a, b: ARRAY [0..MaxStrLen] OF CHAR ;
   p: PtrToProcDes ;
   ps: ProcStatus ;
BEGIN
   GetEnvironment('cmd', a) ;
   IF GetArg(a, 1, b)
   THEN
      p := GetProc( b ) ;
      IF p=NIL
      THEN
         WriteString('Process not found') ;
         WriteLn
      ELSIF GetArg(a, 2, b)
      THEN
         StrCopy( b, p^.PName ) ;
         WriteString('done') ;
         WriteLn
      ELSE
         WriteString('Usage: rename processname newname') ; WriteLn
      END
   ELSE
      WriteString('Usage: rename processname newname') ; WriteLn
   END
END rename ;


PROCEDURE pri ;
VAR
   a, b : ARRAY [0..MaxStrLen] OF CHAR ;
   pt   : ProcType ;
   p    : PtrToProcDes ;
   c    : CARDINAL ;
BEGIN
   GetEnvironment('cmd', a) ;
   IF GetArg(a, 1, b)
   THEN
      p := GetProc( b ) ;
      IF p=NIL
      THEN
         WriteString('Process not found') ;
         WriteLn
      ELSE
         IF GetArg(a, 2, b)
         THEN
            IF GetType(b, pt)
            THEN
               IF GetArg(a, 3, b)
               THEN
                  StrToCard( a, c ) ;
                  IF c>500
                  THEN
                     WriteString('Max 500') ; WriteLn
                  ELSE
                     PutPriority(p, pt, c) ;
                     WriteString('done') ;
                     WriteLn
                  END
               ELSE
                  WriteString('Usage: pri processname (system | user) quata') ;
                  WriteLn
               END
            ELSE
               WriteString('Usage: pri processname (system | user) quata') ;
               WriteLn
            END
         END
      END
   ELSE
      WriteString('Usage: pri processname (system | user) quata') ;
      WriteLn
   END
END pri ;


PROCEDURE GetType (a: ARRAY OF CHAR; VAR pt: ProcType) : BOOLEAN ;
BEGIN
   IF StrEqual(a, 'system')
   THEN
      pt := System ;
      RETURN( TRUE )
   ELSIF StrEqual(a, 'user')
   THEN
      pt := User ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END GetType ;


PROCEDURE GetSymbol (VAR a: ARRAY OF CHAR) ;
BEGIN
   WriteString('$ ') ;
   ReadString( a ) ;
   WriteLn
END GetSymbol ;


PROCEDURE Pause ;
VAR
   ch: CHAR ;
BEGIN
   WriteString('Press Any Key To Continue') ;
   Read( ch )
END Pause ;
   
         
BEGIN
   NewStream( MonStream ) ;
   InitStream( MonStream, TTIO.Read, NIL, Scn.Write, NIL, Scn.Write, NIL ) ;
END Mon.
