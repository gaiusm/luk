IMPLEMENTATION MODULE M2RTS ;


FROM minbios IMPORT WriteChar ;
FROM SYSTEM IMPORT ADDRESS ;

CONST
   Max = 10 ;

VAR
   Ptr      : CARDINAL ;
   List     : ARRAY [0..Max] OF PROC ;
   ExitValue: INTEGER ;
   CallExit : BOOLEAN ;


(*
   Terminate - calls each installed termination procedure in turn.
*)

PROCEDURE Terminate ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE i<Ptr DO
      List[i] ;
      INC(i)
   END
END Terminate ;


(*
   HALT - terminate the current program calling creating a core dump.
          The procedure Terminate is called before the core dump is
          created.
*)

PROCEDURE HALT ;
BEGIN
   Terminate ;
   WriteChar('H') ; WriteChar('A') ; WriteChar('L') ; WriteChar('T') ;
   LOOP END
END HALT ;


(*
   SubrangeAssignmentError - part of the runtime checking, called if a
                             subrange variable is just about to be assigned an illegal value.
*)

PROCEDURE SubrangeAssignmentError (file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   HALT
END SubrangeAssignmentError ;


(*
   ArraySubscriptError -  part of the runtime checking, called if an
                          array indice is out of range.
*)

PROCEDURE ArraySubscriptError (file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   HALT
END ArraySubscriptError ;


(*
   FunctionReturnError - 
*)

PROCEDURE FunctionReturnError (file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   HALT
END FunctionReturnError ;


(*
   ExitOnHalt - if HALT is executed then call exit with the exit code, e.
*)

PROCEDURE ExitOnHalt (e: INTEGER) ;
BEGIN
   ExitValue := e ;
   CallExit := TRUE
END ExitOnHalt ;


(*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure Terminate
                                 is ionvoked.
*)

PROCEDURE InstallTerminationProcedure (p: PROC) ;
BEGIN
   List[Ptr] := p ;
   INC(Ptr)
END InstallTerminationProcedure ;


BEGIN
   Ptr := 0 ;
   ExitValue := 0 ;
   CallExit := FALSE   (* default by calling abort *)
END M2RTS.
