IMPLEMENTATION MODULE Debug ;


FROM ASCII IMPORT cr, nul, lf ;
FROM NumberIO IMPORT CardToStr ;
FROM StdIO IMPORT Write ;
FROM SYSTEM IMPORT TurnInterrupts, OnOrOff ;
FROM Architecture IMPORT IsSimulation ;


(*
   Halt - writes a message in the format:
          Module:Line:Message

          to the debugging device. (Scn.Write).
          It then terminates by looping forever.
*)

PROCEDURE Halt (Message: ARRAY OF CHAR;
                LineNo: CARDINAL;
                Module: ARRAY OF CHAR) ;
CONST
   MaxNoOfDigits = 12 ;  (* should be large enough for most source files.. *)
VAR
   StrNo            : ARRAY [0..MaxNoOfDigits] OF CHAR ;
   OldInterruptState: OnOrOff ;
BEGIN
   OldInterruptState := TurnInterrupts(Off) ;
   DebugString(Module) ;
   CardToStr(LineNo, 0, StrNo) ;
   DebugString(':') ;
   DebugString(StrNo) ;
   DebugString(':') ;
   DebugString(Message) ;
   DebugString('\n') ;
   IF IsSimulation
   THEN
      ASM('halt')
   ELSE
      LOOP
         (* now terminate *)
      END
   END
END Halt ;


(*
   DebugString - writes a string to the debugging device (Scn.Write).
                 It interprets \n as carriage return, linefeed.
*)

PROCEDURE DebugString (a: ARRAY OF CHAR) ;
VAR
   n, high          : CARDINAL ;
   OldInterruptState: OnOrOff ;
BEGIN
   OldInterruptState := TurnInterrupts(Off) ;
   high := HIGH( a ) ;
   n := 0 ;

   (* your code needs to go here *)

   WHILE (n <= high) AND (a[n] # nul) DO
      IF a[n]='\'
      THEN
         IF n+1<=high
         THEN
            IF a[n+1]='n'
            THEN
               WriteLn ;
               INC(n)
            ELSIF a[n+1]='\'
            THEN
               Write('\') ;
               INC(n)
            END
         END
      ELSE
         Write( a[n] )
      END ;
      INC( n )
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END DebugString ;


(*
   DebugCString - writes a string to the debugging device (Scn.Write).
                  It interprets \n as carriage return, linefeed.
*)

PROCEDURE DebugCString (a: ADDRESS) ;
VAR
   p                : POINTER TO CHAR ;
   OldInterruptState: OnOrOff ;
BEGIN
   OldInterruptState := TurnInterrupts(Off) ;
   p := a ;

   (* your code needs to go here *)

   WHILE p^#nul DO
      IF p^='\'
      THEN
         INC(p) ;
         IF p^='n'
         THEN
            WriteLn ;
            INC(p)
         ELSIF p^='\'
         THEN
            Write('\') ;
            INC(p)
         END
      ELSE
         Write(p^) ;
         INC(p)
      END
   END ;
   OldInterruptState := TurnInterrupts(OldInterruptState)
END DebugCString ;


(*
   WriteLn - writes a carriage return and a newline
             character.
*)

PROCEDURE WriteLn ;
BEGIN
   Write(cr) ;
   Write(lf)
END WriteLn ;


END Debug.
