IMPLEMENTATION MODULE StdIO ;


IMPORT Scn ;


CONST
   MaxStack = 40 ;

VAR
   Stack   : ARRAY [0..MaxStack] OF ProcWrite ;
   StackPtr: CARDINAL ;


(*
   Read - is the generic procedure that all higher application layers
          should use to receive a character.
*)

PROCEDURE Read (VAR ch: CHAR) ;
BEGIN
   Write('R') ; Write('e') ; Write('a') ; Write('d') ; Write(' ') ;
   Write('i') ; Write('m') ; Write('p') ; Write('l') ; Write('e') ;
   Write('m') ; Write('e') ; Write('n') ; Write('t') ; Write('e') ;
   Write('d')
END Read ;


(*
   Write - is the generic procedure that all higher application layers
           should use to emit a character.
*)
 
PROCEDURE Write (ch: CHAR) ;
BEGIN
   Stack[StackPtr](ch)
END Write ;


(*
   PushOutput - pushes the current Write procedure onto a stack,
                any future references to Write will actually invoke
                procedure, p.
*)

PROCEDURE PushOutput (p: ProcWrite) ;
BEGIN
   IF StackPtr=MaxStack
   THEN
      HALT
   ELSE
      INC(StackPtr) ;
      Stack[StackPtr] := p
   END
END PushOutput ;


(*
   PopOutput - restores Write to use the previous output procedure.
*)

PROCEDURE PopOutput ;
BEGIN
   IF StackPtr=1
   THEN
      HALT
   ELSE
      DEC(StackPtr)
   END
END PopOutput ;


(*
   GetCurrentOutput - returns the current output procedure.
*)

PROCEDURE GetCurrentOutput () : ProcWrite ;
BEGIN
   IF StackPtr>0
   THEN
      RETURN( Stack[StackPtr] )
   ELSE
      HALT
   END
END GetCurrentOutput ;


(*
   Init - initialize this module data structures.
*)

PROCEDURE Init ;
BEGIN
   StackPtr := 0 ;
   PushOutput(Scn.Write)
END Init ;


END StdIO.
