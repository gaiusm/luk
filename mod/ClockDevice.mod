IMPLEMENTATION MODULE ClockDevice ;


FROM PortIO IMPORT In8, Out8 ;
FROM SYSTEM IMPORT BYTE ;


CONST
   Counter0       = 040H ;   (* Constants for the 8253 timer chip *)
   Counter1       = 041H ;
   Counter2       = 042H ;
   ControlWordReg = 043H ;


(*
   LoadClock - returns the clock device current count.
*)

PROCEDURE LoadClock () : CARDINAL ;
VAR
   lo, hi: CARDINAL ;
BEGIN
   (* Tell 8253 that we wish to Read or Write *)
   (* to it. Mode 0 Counter 0.                *)
   Out8(ControlWordReg, VAL(BYTE, {4,5})) ;

   lo := VAL(CARDINAL, In8(Counter0)) ;   (* Least Significant Byte *)
   hi := VAL(CARDINAL, In8(Counter0)) ;   (* Most significant Byte  *)
   RETURN( hi*0100H + lo )
END LoadClock ;


(*
   StartClock - sets the Count into clock.
*)

PROCEDURE StartClock (Count: CARDINAL) ;
BEGIN
   (* Tell 8253 that we wish to Read or Write *)
   (* to it. Mode 0 Counter 0.                *)
   Out8(ControlWordReg, VAL(BYTE, {4,5})) ;

   Out8(Counter0, VAL(BYTE, Count MOD 0100H)) ;   (* Least Significant Byte *)
   Out8(Counter0, VAL(BYTE, Count DIV 0100H)) ;   (* Most significant Byte  *)
END StartClock ;


END ClockDevice.
