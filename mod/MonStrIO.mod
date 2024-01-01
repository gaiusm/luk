IMPLEMENTATION MODULE MonStrIO ;


FROM StdIO IMPORT Write, PushOutput, PopOutput ;
FROM SYSTEM IMPORT OnOrOff, TurnInterrupts ;

IMPORT Scn ;
IMPORT StrIO ;
IMPORT NumberIO ;



VAR
   OldState: OnOrOff ;
   DebuggingWrite: ProcWrite ;


(*
   DebuggingStream - sets the debugging output to use, p.
*)

PROCEDURE DebuggingStream (p: ProcWrite) ;
VAR
   OldState: OnOrOff ;
BEGIN
   OldState := TurnInterrupts(Off) ;
   DebuggingWrite := p ;
   OldState := TurnInterrupts(OldState) ;
END DebuggingStream ;


(*
   StartDebugging - initializes the output for a debugging message.
*)

PROCEDURE StartDebugging ;
BEGIN
   OldState := TurnInterrupts(Off) ;
   PushOutput(DebuggingWrite)
END StartDebugging ;


(*
   EndDebugging - returns the output back to that before StartDebugging
                  was called.
*)

PROCEDURE EndDebugging ;
BEGIN
   OldState := TurnInterrupts(OldState) ;
   PopOutput
END EndDebugging ;


PROCEDURE WriteLn ;
BEGIN
   StartDebugging ;
   StrIO.WriteLn ;
   EndDebugging
END WriteLn ;


PROCEDURE ReadString (VAR a: ARRAY OF CHAR) ;
BEGIN
   StartDebugging ;
   StrIO.ReadString(a) ;
   EndDebugging
END ReadString ;


PROCEDURE WriteString (a: ARRAY OF CHAR) ;
BEGIN
   StartDebugging ;
   StrIO.WriteString(a) ;
   EndDebugging
END WriteString ;


PROCEDURE WriteCard (x, n: CARDINAL) ;
BEGIN
   StartDebugging ;
   NumberIO.WriteCard(x, n) ;
   EndDebugging
END WriteCard ;


PROCEDURE WriteInt (x: INTEGER; n: CARDINAL) ;
BEGIN
   StartDebugging ;
   NumberIO.WriteInt(x, n) ;
   EndDebugging
END WriteInt ;


PROCEDURE WriteHex (x, n: CARDINAL) ;
BEGIN
   StartDebugging ;
   NumberIO.WriteHex(x, n) ;
   EndDebugging
END WriteHex ;


PROCEDURE WriteBin (x, n: CARDINAL) ;
BEGIN
   StartDebugging ;
   NumberIO.WriteBin(x, n) ;
   EndDebugging
END WriteBin ;


(*
   Init - sets the DebuggingStream to use Scn.Write.
          Should only be called by M2RTS.
*)

PROCEDURE Init ;
BEGIN
   DebuggingStream(Scn.Write)
END Init ;


END MonStrIO.
