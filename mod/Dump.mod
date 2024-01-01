IMPLEMENTATION MODULE Dump ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteHex ;
FROM SYSTEM IMPORT TSIZE, BYTE ;


CONST
   ItemsPerLine = 16 ;


(*
   DumpDown - displays Length bytes in WORDs starting at, Top,
              and working down.
*)

PROCEDURE DumpDown (Top: ADDRESS; Length: CARDINAL) ;
VAR
   i, j  : CARDINAL ;
   PtrToB: POINTER TO BYTE ;
BEGIN
   FOR i := 0 TO Length DIV (ItemsPerLine * TSIZE(BYTE)) DO
      Top := Top - ADDRESS(TSIZE(BYTE) * ItemsPerLine) ;
      WriteHex(CARDINAL(Top), 8) ; WriteString(': ') ;
      FOR j := 0 TO ItemsPerLine-1 DO
         PtrToB := Top + ADDRESS(j*TSIZE(BYTE)) ;
         WriteHex(VAL(CARDINAL, PtrToB^), 2) ; WriteString(' ')
      END ;
      WriteLn
   END
END DumpDown ;


(*
   DumpUp - displays Length bytes in WORDs starting at, Bot,
            and working up.
*)

PROCEDURE DumpUp (Bot: ADDRESS; Length: CARDINAL) ;
VAR
   i, j  : CARDINAL ;
   PtrToB: POINTER TO BYTE ;
BEGIN
   FOR i := 0 TO Length DIV (ItemsPerLine * TSIZE(BYTE)) DO
      WriteHex(CARDINAL(Bot), 8) ; WriteString(': ') ;
      FOR j := 0 TO ItemsPerLine-1 DO
         PtrToB := Bot + ADDRESS(j*TSIZE(BYTE)) ;
         WriteHex(VAL(CARDINAL, PtrToB^), 2) ; WriteString(' ')
      END ;
      Bot := Bot + ADDRESS(TSIZE(BYTE) * ItemsPerLine) ;
      WriteLn
   END
END DumpUp ;


END Dump.
