IMPLEMENTATION MODULE VarDB ;


CONST
   Max = 30 ;

TYPE
   Variable = RECORD
                 VarName : Name ;
                 VarValue: CARDINAL ;
              END ;

VAR
   Memory       : ARRAY [1..Max] OF Variable ;
   NoOfVariables: CARDINAL ;


(*
   InitVarDB - wipes out the complete variable database.
*)

PROCEDURE InitVarDB ;
BEGIN
   NoOfVariables := 0
END InitVarDB ;


(*
   PutValue - places, Value, into variable, Name.
*)

PROCEDURE PutValue (n: Name; Value: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE (i<=NoOfVariables) AND (Memory[i].VarName#n) DO
      INC(i)
   END ;
   IF i<=NoOfVariables
   THEN
      (* variable already exists - update value *)
      Memory[i].VarValue := Value
   ELSE
      INC(NoOfVariables) ;
      IF NoOfVariables<=Max
      THEN
         WITH Memory[NoOfVariables] DO
            VarName := n ;
            VarValue := Value
         END
      END
   END
END PutValue ;


(*
   GetValue - returns the value to a variable, Name.
*)

PROCEDURE GetValue (n: Name) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE (i<=NoOfVariables) AND (Memory[i].VarName#n) DO
      INC(i)
   END ;
   IF i<=NoOfVariables
   THEN
      (* variable exists - return value *)
      RETURN( Memory[i].VarValue )
   ELSE
      RETURN( 0 )
   END
END GetValue ;


(*
   IsValue - returns TRUE if variable, Name, has been defined.
*)

PROCEDURE IsValue (n: Name) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE (i<=NoOfVariables) AND (Memory[i].VarName#n) DO
      INC(i)
   END ;
   RETURN( i<=NoOfVariables )
END IsValue ;


END VarDB.
