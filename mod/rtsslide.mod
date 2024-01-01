MODULE rtsslide ;

FROM SYSTEM IMPORT ADDRESS, BITSET, BYTE,
                   OnOrOff, TurnInterrupts ;
FROM PortIO IMPORT In8, Out8 ;


(*
   OnSound - 
*)

PROCEDURE OnSound (tone: CARDINAL) ;
VAR
   b: BYTE ;
   s: BITSET ;
BEGIN
   Out8(043H, 0B6H) ;               (*  tell timer prepare for new sound *)
   Out8(042H, VAL(BYTE, tone MOD 0100H)) ;      (* new tone to timer LSB *)
   Out8(042H, VAL(BYTE, tone DIV 0100H)) ;      (* LSB -> timer          *)
   b := In8(061H) ;                           (* enable speaker via time *)
   s := VAL(BITSET, b) ;
   s := s + {0, 1} ;
   Out8(061H, VAL(BYTE, s))
END OnSound ;


(*
   OffSound - 
*)

PROCEDURE OffSound ;
VAR
   b: BYTE ;
   s: BITSET ;
BEGIN
   b := In8(061H) ;         (* disable speaker    *)
   s := VAL(BITSET, b) ;
   s := s - {0, 1} ;
   Out8(061H, VAL(BYTE, s)) (* output via a timer *)
END OffSound ;


(*
   Delay - 
*)

PROCEDURE Delay (MaxI, MaxJ: CARDINAL) ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR i := 0 TO MaxI DO
      FOR j := 0 TO MaxJ DO
      END
   END
END Delay ;


(*
   Slide - 
*)

PROCEDURE Slide ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 1 TO 500 DO
      OnSound(i) ;
      Delay(10, 400) ;
      OffSound
   END ;
END Slide ;


BEGIN
   LOOP
      Slide
   END
END rtsslide.
