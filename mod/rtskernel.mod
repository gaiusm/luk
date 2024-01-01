MODULE rtskernel ;


FROM MonStrIO IMPORT WriteString ;
FROM Kernel IMPORT InitProcess, Resume, PtrToProcDes, ProcType,
                   Suspend, CurrentProcess, InitSemaphore, SEMAPHORE,
                   Wait, Signal ;
FROM SYSTEM IMPORT TurnInterrupts, OnOrOff ;
FROM Ps IMPORT DoPs ;


(*
   Process1 - 
*)

PROCEDURE Process1 ;
BEGIN
   Old := TurnInterrupts(On) ;
   LOOP
      (* Resume(p2) ; *)
      (* Signal(To2) ; *)
      WriteString('1') ;
      (* Wait(To1) ; *)
(*    Suspend(p1) *)
   END
END Process1 ;


(*
   Process2 - 
*)

PROCEDURE Process2 ;
BEGIN
   Old := TurnInterrupts(On) ;
   LOOP
      (* Resume(p1) ; *)
      (* Signal(To1) ; *)
      WriteString('2') ;
      (* Wait(To2) ; *)
      (* Suspend(p2) *)
   END
END Process2 ;


VAR
   p1, p2: PtrToProcDes ;
   Old   : OnOrOff ;
   Zero    ,
   To1, To2: SEMAPHORE ;
BEGIN
   To1 := InitSemaphore(1, 'To1') ;
   To2 := InitSemaphore(0, 'To2') ;
   Zero := InitSemaphore(0, 'Zero') ;
   InitProcess(p1, Process1, 4, 3, 50000, 0, User, NIL, 'Process1') ;
   DoPs ;
   Resume(p1) ;
   DoPs ;
   InitProcess(p2, Process2, 4, 3, 50000, 0, User, NIL, 'Process2') ;
   DoPs ;
   Resume(p2) ;
   DoPs ;
(*   Old := TurnInterrupts(On) ; *)
   Suspend(CurrentProcess)
END rtskernel.
