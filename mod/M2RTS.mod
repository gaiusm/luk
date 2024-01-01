IMPLEMENTATION MODULE M2RTS ;


FROM SYSTEM IMPORT ADDRESS, ADR ;
FROM Scn IMPORT Write ;
FROM StdIO IMPORT PushOutput ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT CardToStr ;
FROM Descriptors IMPORT SetupGDT, SetupIDT ;
FROM ASCII IMPORT cr, nl, nul ;
FROM StrLib IMPORT StrEqual ;
FROM osinit IMPORT DebuggingWithGDB ;
FROM crt0 IMPORT red, blue ;

IMPORT Scn ;
IMPORT osinit ;
IMPORT StdIO ;
IMPORT InterruptVector ;
IMPORT IRQ ;
IMPORT Storage ;
IMPORT SYSTEM ;
IMPORT Exceptions ;
IMPORT MonStrIO ;
IMPORT libg ;
IMPORT gdb ;


CONST
   MaxProcedures = 10 ;

TYPE
   PtrToChar = POINTER TO CHAR ;

VAR
   iPtr, tPtr   : CARDINAL ;
   InitialProc,
   TerminateProc: ARRAY [0..MaxProcedures] OF PROC ;
   ExitValue    : INTEGER ;
   CallExit     : BOOLEAN ;


(*
   exit - 
*)

PROCEDURE exit (r: INTEGER) ;
BEGIN
   Write('e') ; Write('x') ; Write('i') ; Write('t') ;
   LOOP END
END exit ;


(*
   abort - 
*)

PROCEDURE abort ;
BEGIN
   Write('a') ; Write('b') ; Write('o') ; Write('r') ; Write('t') ;
   LOOP END
END abort ;


(*
   ExecuteTerminationProcedures - calls each installed termination procedure
                                  in reverse order.
*)

PROCEDURE ExecuteTerminationProcedures ;
VAR
   i: CARDINAL ;
BEGIN
   i := tPtr ;
   WHILE i>0 DO
      DEC(i) ;
      TerminateProc[i]
   END
END ExecuteTerminationProcedures ;


(*
   InstallTerminationProcedure - installs a procedure, p, which will
                                 be called when the procedure
                                 ExecuteTerminationProcedures
                                 is invoked.  It returns TRUE is the
                                 procedure is installed.
*)

PROCEDURE InstallTerminationProcedure (p: PROC) : BOOLEAN ;
BEGIN
   IF tPtr>MaxProcedures
   THEN
      RETURN( FALSE )
   ELSE
      TerminateProc[tPtr] := p ;
      INC(tPtr) ;
      RETURN( TRUE )
   END
END InstallTerminationProcedure ;


(*
   ExecuteInitialProcedures - executes the initial procedures installed by
                              InstallInitialProcedure.
*)

PROCEDURE ExecuteInitialProcedures ;
VAR
   i: CARDINAL ;
BEGIN
   i := iPtr ;
   WHILE i>0 DO
      DEC(i) ;
      InitialProc[i]
   END
END ExecuteInitialProcedures ;


(*
   InstallInitialProcedure - installs a procedure to be executed just before the
                             BEGIN code section of the main program module.
*)

PROCEDURE InstallInitialProcedure (p: PROC) : BOOLEAN ;
BEGIN
   IF iPtr>MaxProcedures
   THEN
      RETURN( FALSE )
   ELSE
      InitialProc[iPtr] := p ;
      INC(iPtr) ;
      RETURN( TRUE )
   END
END InstallInitialProcedure ;


(*
   HALT - terminate the current program.  The procedure
          ExecuteTerminationProcedures
          is called before the program is stopped.  The parameter
          exitcode is optional.  If the parameter is not supplied
          HALT will call libc 'abort', otherwise it will exit with
          the code supplied.  Supplying a parameter to HALT has the
          same effect as calling ExitOnHalt with the same code and
          then calling HALT with no parameter.
*)

PROCEDURE HALT ([exitcode: INTEGER = -1]) ;
BEGIN
   IF exitcode#-1
   THEN
      CallExit := TRUE ;
      ExitValue := exitcode
   END ;
   ExecuteTerminationProcedures ;
   Write('H') ; Write('A') ; Write('L') ; Write('T') ;
   IF CallExit
   THEN
      exit(ExitValue)
   ELSE
      abort
   END
END HALT ;


(*
   Terminate - provides compatibility for pim.  It call exit with
               the exitcode provided in a prior call to ExitOnHalt
               (or zero if ExitOnHalt was never called).  It does
               not call ExecuteTerminationProcedures.
*)

PROCEDURE Terminate ;
BEGIN
   exit(ExitValue)
END Terminate ;


(*
   ErrorString - writes a string to the Scn device.
*)

PROCEDURE ErrorString (a: ARRAY OF CHAR) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   WHILE (i<=HIGH(a)) AND (a[i]#nul) DO
      Write(a[i]) ;
      INC(i)
   END
END ErrorString ;


(*
   ErrorMessage - emits an error message to stderr and then calls exit (1).
*)

PROCEDURE ErrorMessage (message: ARRAY OF CHAR;
                        file: ARRAY OF CHAR;
                        line: CARDINAL;
                        function: ARRAY OF CHAR) ;
VAR
   LineNo: ARRAY [0..10] OF CHAR ;
BEGIN
   ErrorString(file) ; ErrorString(':') ;
   CardToStr(line, 0, LineNo) ;
   ErrorString(LineNo) ; ErrorString(':') ;
   IF NOT StrEqual(function, '')
   THEN
      ErrorString('in ') ;
      ErrorString(function) ;
      ErrorString(' has caused ') ;
   END ;
   ErrorString(message) ;
   LineNo[0] := nl ; LineNo[1] := nul ;
   ErrorString(LineNo) ;
   exit(1)
END ErrorMessage ;


(*
   Halt - provides a more user friendly version of HALT, which takes
          four parameters to aid debugging.
*)

PROCEDURE Halt (file: ARRAY OF CHAR; line: CARDINAL;
                function: ARRAY OF CHAR; description: ARRAY OF CHAR) ;
BEGIN
   ErrorMessage(description, file, line, function) ;
   HALT
END Halt ;


(*
   addChar - adds, ch, to the Scn device.
*)

PROCEDURE addChar (ch: CHAR) ;
BEGIN
   Scn.Write(ch)
END addChar ;


(*
   stripPath - returns the filename from the path.
*)

PROCEDURE stripPath (s: ADDRESS) : ADDRESS ;
VAR
   f, p: PtrToChar ;
BEGIN
   p := s ;
   f := s ;
   WHILE p^#nul DO
      IF p^='/'
      THEN
         INC(p) ;
         f := p
      ELSE
         INC(p)
      END
   END ;
   RETURN( f )
END stripPath ;


(*
   addFile - adds the filename determined by, s, to the Scn device.  It strips
             any preceeding path.
*)

PROCEDURE addFile (s: ADDRESS) ;
VAR
   p: PtrToChar ;
BEGIN
   p := stripPath(s) ;
   WHILE (p#NIL) AND (p^#nul) DO
      addChar(p^) ;
      INC(p)
   END
END addFile ;


(*
   addStr - adds a C string from address to the Scn device.
*)

PROCEDURE addStr (s: ADDRESS) ;
VAR
   p: PtrToChar ;
BEGIN
   p := s ;
   WHILE (p#NIL) AND (p^#nul) DO
      addChar(p^) ;
      INC(p)
   END
END addStr ;


(*
   addNum - writes number to the screen.
*)

PROCEDURE addNum (n: CARDINAL) ;
BEGIN
   IF n<10
   THEN
      addChar(CHR(n MOD 10 + ORD('0')))
   ELSE
      addNum(n DIV 10) ;
      addNum(n MOD 10)
   END
END addNum ;


(*
   Raise - invoke the exception handler associated with, number,
           in the active EHBlock.  It keeps a record of the number
           and message in the EHBlock for later use.
*)

PROCEDURE Raise (file: ADDRESS; line: CARDINAL;
                 column: CARDINAL; function: ADDRESS;
                 message: ADDRESS) ;
BEGIN
   addFile(file) ;
   addChar(':') ;
   addNum(line) ;
   addChar(':') ;
   addNum(column) ;
   addChar(':') ;
   addStr(message) ;
   addChar(' ') ;
   addChar('i') ;
   addChar('n') ;
   addChar(' ') ;
   addStr(function) ;
   addChar(nl) ;
   LOOP
   END
END Raise ;


(*
   The following are the runtime exception handler routines.
*)

PROCEDURE AssignmentException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("variable exceeds range during assignment"))
END AssignmentException ;


PROCEDURE IncException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("variable exceeds range during INC statement"))
END IncException ;


PROCEDURE DecException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("variable exceeds range during DEC statement"))
END DecException ;


PROCEDURE InclException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("bit exceeds set range during INCL statement"))
END InclException ;


PROCEDURE ExclException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("bit exceeds set range during EXCL statement"))
END ExclException ;


PROCEDURE ShiftException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("bit exceeds set range during SHIFT statement"))
END ShiftException ;


PROCEDURE RotateException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("bit exceeds set range during ROTATE statement"))
END RotateException ;


PROCEDURE StaticArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("array index out of bounds during static array access"))
END StaticArraySubscriptException ;


PROCEDURE DynamicArraySubscriptException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("array index out of bounds during dynamic array access"))
END DynamicArraySubscriptException ;


PROCEDURE ForLoopBeginException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("iterator variable exceeds range during FOR loop initial assignment"))
END ForLoopBeginException ;


PROCEDURE ForLoopToException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("iterator variable will exceed range when calculating final value in FOR loop"))
END ForLoopToException ;


PROCEDURE ForLoopEndException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("iterator variable exceeds range during increment at the end of a FOR loop"))
END ForLoopEndException ;


PROCEDURE PointerNilException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("attempting to dereference a NIL valued pointer"))
END PointerNilException ;


PROCEDURE NoReturnException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("about to finish a PROCEDURE without executing a RETURN statement"))
END NoReturnException ;


PROCEDURE CaseException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("the expression in the CASE statement cannot be selected"))
END CaseException ;


PROCEDURE WholeNonPosDivException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("the division expression has a divisor which is less than or equal to zero"))
END WholeNonPosDivException ;


PROCEDURE WholeNonPosModException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("the modulus expression has a divisor which is less than or equal to zero"))
END WholeNonPosModException ;


PROCEDURE WholeZeroDivException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("the division expression has a divisor which is equal to zero"))
END WholeZeroDivException ;


PROCEDURE WholeZeroRemException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("the remainder expression has a divisor which is equal to zero"))
END WholeZeroRemException ;


PROCEDURE NoException (filename: ADDRESS; line, column: CARDINAL; scope: ADDRESS) ;
BEGIN
   Raise(filename, line, column, scope,
         ADR("M2Expection was called when no there was no outstanding exception to be returned"))
END NoException ;


(*
   ExitOnHalt - if HALT is executed then call exit with the exit code, e.
*)

PROCEDURE ExitOnHalt (e: INTEGER) ;
BEGIN
   ExitValue := e ;
   CallExit := TRUE
END ExitOnHalt ;


(*
   Length - returns the length of a string, a. This is called whenever
            the user calls LENGTH and the parameter cannot be calculated
            at compile time.
*)

PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   l, h: CARDINAL ;
BEGIN
   l := 0 ;
   h := HIGH(a) ;
   WHILE (l<=h) AND (a[l]#nul) DO
      INC(l)
   END ;
   RETURN( l )
END Length ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   iPtr := 0 ;
   tPtr := 0 ;
   ExitValue := 0 ;
   CallExit := FALSE ; (* default by calling abort *)
END Init ;


BEGIN
   (*
      it is the job of M2RTS to call each all runtime initialization
      routines. We need a global initialization routine as the order
      it critical and we dare not let the statical analysis guess it..
   *)
   Init ;          (* M2RTS    initialization *)
   red ;
   osinit.Init ;   (* osinit   initialization: RTS mem parameters  *)
   Scn.Init ;      (* Scn      initialization: screen address      *)
   Scn.Write('S') ;   Scn.Write('c') ;   Scn.Write('n') ;

   StdIO.Init ;    (* StdIO    initialization: use Scn.Write       *)
   Scn.Write('S') ;   Scn.Write('t') ;   Scn.Write('d') ;   Scn.Write('i') ;   Scn.Write('o') ;
   MonStrIO.Init ; (* MonStrIO initialization: use Scn.Write for debugging *)
   Scn.Write('M') ;   Scn.Write('o') ;   Scn.Write('n') ;   Scn.Write('S') ;   Scn.Write('t') ;   Scn.Write('r') ;   Scn.Write('I') ;   Scn.Write('O') ;

   SetupIDT ;     (* 386/486 MMU Interrupt descriptor tables     *)
   Scn.Write('I') ;   Scn.Write('D') ;   Scn.Write('T') ;

   SetupGDT ;     (* 386/486 MMU Global    descriptor tables     *)
   Scn.Write('G') ;   Scn.Write('D') ;   Scn.Write('T') ;

   InterruptVector.Init ;    (* Catchall interrupt handlers      *)
   Scn.Write('i') ;   Scn.Write('n') ;   Scn.Write('t') ;

   IRQ.Init ;                (* Catchall IRQ specific handlers   *)
   Scn.Write('I') ;   Scn.Write('R') ;   Scn.Write('Q') ;

   SYSTEM.Init ;  (* SYSTEM  initialization: IOTRANSFER table    *)
   Scn.Write('S') ;   Scn.Write('Y') ;   Scn.Write('S') ;

   Storage.Init ; (* Storage initialization: heap management     *)
   Scn.Write('S') ;   Scn.Write('t') ;   Scn.Write('o') ;   Scn.Write('r') ;
   Scn.Write('a') ;   Scn.Write('g') ;   Scn.Write('e') ;
   Write(cr) ; 
   Write(nl) ;

   Write('g') ; Write('o') ;
   Write(cr) ; Write(nl) ;

   Exceptions.Init ; (* install exception interrupt handlers     *)

   IF DebuggingWithGDB()
   THEN
      libg.Init ; (* remove this when not debugging              *)
      gdb.Init
   END
END M2RTS.
