MODULE this ;


FROM SYSTEM IMPORT BYTE ;


VAR
   i, j, k: CARDINAL ;
   b      : BYTE ;
BEGIN
   i := 1234H ;
   j := i MOD 0100H ;
   b := VAL(BYTE, i) ;
