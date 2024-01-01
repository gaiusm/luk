IMPLEMENTATION MODULE IO ;


IMPORT MonStrIO ;

(*
   Set up stream, so that a Read and a Write should become transparent.
   So the same Read and Write will work for every STREAM. Thus making
   the IO orthoganol.
*)

FROM SysStorage IMPORT ALLOCATE, DEALLOCATE ;



PROCEDURE NewStream (VAR s: STREAM) ;
BEGIN
   (* Code to dynamically allocate a STREAM *)
   NEW( s ) ;
   WITH s^ DO
      inchar := ErrorRead ;
      outchar := ErrorWrite ;
      errchar := ErrorWrite
   END
END NewStream ;


PROCEDURE DisposeStream (VAR s: STREAM) ;
BEGIN
   (* Code to dynamically deallocate a STREAM *)
   DISPOSE( s )
END DisposeStream ;


PROCEDURE ConnectStream (VAR s1: STREAM ; s2: STREAM) ;
BEGIN
   s1 := s2
END ConnectStream ;


PROCEDURE DisConnectStream (s1: STREAM ; VAR s2: STREAM) ;
BEGIN
   s2 := s1
END DisConnectStream ;


PROCEDURE SwapStream (VAR s1, s2: STREAM) ;
VAR
   t: STREAM ;
BEGIN
   t := s1 ;
   s1 := s2 ;
   s2 := t
END SwapStream ;


PROCEDURE DupStream (s1: STREAM; s2: STREAM) ;
BEGIN
   s2^ := s1^
END DupStream ;


PROCEDURE InitStream (s: STREAM ;
                      r: ProcRead ; ra: ADDRESS ;
                      w: ProcWrite ; wa: ADDRESS ;
                      e: ProcWrite ; ea: ADDRESS) ;
BEGIN
   WITH s^ DO
      inchar  := r ;
      outchar := w ;
      errchar := e ;
      devin := ra ;
      devout := wa ;
      deverr := ea
   END
END InitStream ;


PROCEDURE ErrorRead (VAR ch: CHAR) ;
BEGIN
   MonStrIO.WriteString('Error - attempting to read from nul channel')
END ErrorRead ;


PROCEDURE ErrorWrite (ch: CHAR) ;
BEGIN
   MonStrIO.WriteString('Error - attempting to write to a nul channel')
END ErrorWrite ;


END IO.
