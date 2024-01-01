MODULE testctrl2 ;


IMPORT Motor ;
FROM Kernel IMPORT Suspend, CurrentProcess ;
FROM Ps IMPORT DoPs ;
FROM TTIO IMPORT Read ;

VAR
   ch: CHAR ;
BEGIN
   LOOP
      Motor.Recalibrate
   END
END testctrl2.
