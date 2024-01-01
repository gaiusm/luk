MODULE lander ;

(*
    Title      : lander
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Fri Nov 10 22:00:48 1995
    Last edit  : Fri Nov 10 22:00:48 1995
    Description: very crude lunar lander
*)


FROM Motor IMPORT ManuallyAdjust, MotorPositionValue, ResetMotorValues,
                  MicroSwitchOn, TurnMotor ;
IMPORT MonStrIO ;
FROM TTIO IMPORT Read ;
FROM Scn IMPORT Write ;
FROM Kernel IMPORT Sleep, ClockFreq, Resume, InitProcess,
                   InitSemaphore, PtrToProcDes, ProcType,
                   Wait, Signal, SEMAPHORE ;


CONST
   LunarGravity = 1 ;
   MaxSpeed     = 8 ;
   MinSpeed     =-8 ;
   SafeLanding  = 2 ;

VAR
   GravityP       : PtrToProcDes ;
   Speed          : INTEGER ;
   AbsBottom,
   Fuel           : CARDINAL ;
   GravityFinished,
   ToPlay         : SEMAPHORE ;
   Landed,
   Halted         : BOOLEAN ;


(*
   Diff - 
*)

PROCEDURE Diff (a, b: INTEGER) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a-b )
   ELSE
      RETURN( b-a )
   END
END Diff ;


(*
   Min - 
*)

PROCEDURE Min (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a>b
   THEN
      RETURN( b )
   ELSE
      RETURN( a )
   END
END Min ;


(*
   Max - 
*)

PROCEDURE Max (a, b: INTEGER) : INTEGER ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Abs - 
*)

PROCEDURE Abs (i: INTEGER) : CARDINAL ;
BEGIN
   IF i<0
   THEN
      RETURN(CARDINAL(-i))
   ELSE
      RETURN(CARDINAL(i))
   END
END Abs ;


(*
   ManualSafeStart - 
*)

PROCEDURE ManualSafeStart ;
BEGIN
   MonStrIO.WriteLn ;
   MonStrIO.WriteString('You might want to take the lunar lander mid way up and recalibrate') ;
   MonStrIO.WriteLn ;
   MonStrIO.WriteString('Then you MUST take it to the BOTTOM') ; MonStrIO.WriteLn ;
   ManuallyAdjust ;
   ResetMotorValues ;
   AbsBottom := MotorPositionValue() ;
   MonStrIO.WriteString('Now you MUST take it to the TOP') ; MonStrIO.WriteLn ;
   ManuallyAdjust ;
   AbsBottom := Diff(INTEGER(MotorPositionValue()), INTEGER(AbsBottom)) ;
END ManualSafeStart ;


(*
   InitVariables - 
*)

PROCEDURE InitVariables ;
VAR
   ch: CHAR ;
BEGIN
   Landed := FALSE ;
   Halted := FALSE ;
   MonStrIO.WriteString('Height is: ') ;
   MonStrIO.WriteCard(MotorPositionValue(), 4) ;
   MonStrIO.WriteLn ;
   MonStrIO.WriteString('Enter fuel digit: ') ;
   REPEAT
      Read(ch)
   UNTIL (ch>='1') AND (ch<='9') ;
   Write(ch) ;
   MonStrIO.WriteLn ;
   Fuel := 10 * (ORD(ch)-ORD('0')) ;
   MonStrIO.WriteString('Fuel   is: ') ; MonStrIO.WriteCard(Fuel, 4) ;
   MonStrIO.WriteLn
END InitVariables ;


(*
   Gravity - 
*)

PROCEDURE Gravity ;
BEGIN
   LOOP
      Wait(ToPlay) ;
      REPEAT
         Speed := Max(Speed - LunarGravity, MinSpeed) ;
         TurnMotor(0, Speed>=0) ;      (* just simply to slow it down *)
         Sleep(MaxSpeed*2-Abs(Speed)) ;

         IF ((INTEGER(MotorPositionValue())>0) AND (Speed>0)) (* upwards *) OR
            Halted
         THEN
            (* in danger of shooting off the top ... so we do nothing *)
            TurnMotor(0, Speed>=0) ;  (* stop and wait for it to fall *)
         ELSE
            (* ok the lander is within limits. *)
            TurnMotor(2, Speed>=0) ;    (* slowest speed *)
         END ;
         IF Speed>0
         THEN
            Sleep(Abs(Speed)+4)      (* pull upwards is harder.. *)
         ELSE
            Sleep(Abs(Speed))
         END ;
         IF MicroSwitchOn()
         THEN
            Write('#') ;
            Landed := TRUE
         END
      UNTIL Landed ;
      Landed := TRUE ;
      TurnMotor(0, Speed>=0) ;      (* now off *)
      Signal(GravityFinished)
   END
END Gravity ;


(*
   GoForLanding - 
*)

PROCEDURE GoForLanding ;
VAR
   ch: CHAR ;
BEGIN
   Signal(ToPlay) ;
   REPEAT
      Read(ch) ;
      CASE ch OF

      'l':  Landed := TRUE |
      'h':  TurnMotor(0, FALSE) ;
            Halted := NOT Halted |
      ' ':  IF Fuel>0
            THEN
               DEC(Fuel) ;
               MonStrIO.WriteString('fuel: ') ; MonStrIO.WriteCard(Fuel, 0) ;
               MonStrIO.WriteLn ;
               Speed := Min(Speed+LunarGravity, MaxSpeed)
            ELSE
               MonStrIO.WriteString('no more fuel left') ; MonStrIO.WriteLn
            END
      END ;
      MonStrIO.WriteString('height: ') ; MonStrIO.WriteCard(MotorPositionValue(), 0) ;
      MonStrIO.WriteString('    speed: ') ; MonStrIO.WriteInt(Speed, 0) ;
      MonStrIO.WriteLn
   UNTIL Landed ;
   IF Abs(Speed)<=SafeLanding
   THEN
      MonStrIO.WriteString('well done a safe landing') ; MonStrIO.WriteLn
   ELSE
      MonStrIO.WriteString('crashed...') ; MonStrIO.WriteLn
   END ;
   Wait(GravityFinished)
END GoForLanding ;


(*
   Init - 
*)

PROCEDURE Init ;
BEGIN
   ToPlay := InitSemaphore(0, 'ToPlay') ;
   GravityFinished := InitSemaphore(0, 'GravityFinished') ;
   InitProcess(GravityP, Gravity, 7, 1, 10000, 0, System, NIL, 'Gravity') ;
   Resume(GravityP) ;
   LOOP
      ManualSafeStart ;
      InitVariables ;
      GoForLanding
   END
END Init ;



BEGIN
   Init
END lander.
