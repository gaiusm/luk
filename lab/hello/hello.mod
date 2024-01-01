MODULE hello ;

FROM ASCII IMPORT nl ;
FROM Scn IMPORT Write ;
FROM Debug IMPORT DebugString ;
IMPORT libg ;


PROCEDURE delay (ch: CHAR) ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR i := 0 TO 1024*1024*100 DO
   END ;
   Write(ch)
END delay ;


VAR
   ch: CHAR ;
BEGIN
   (* libg.Init ; *)
   LOOP
(*
      (* libg.Write('.') ; *)
      (* libg.Read(ch) ; *)
      Write('.') ;
      (* libg.Write(ch) ; *)
      Write(ch) ;
*)

      delay('h') ;
      delay(nl) ;
      delay('e') ;
      delay(nl) ;
      delay('l') ;
      delay(nl) ;
      delay('l') ;
      delay(nl) ;
      delay('o') ;
      delay(nl) ;
      DebugString('hello world\n') ;
      DebugString('this has a\nnewline in the middle and one at the end\n')
   END
END hello.
