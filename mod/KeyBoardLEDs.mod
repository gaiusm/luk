IMPLEMENTATION MODULE KeyBoardLEDs ;

(* this module remains for compatability reasons only *)

IMPORT TTIO ;


PROCEDURE SwitchLeds (NumLock, CapsLock, ScrollLock: BOOLEAN) ;
BEGIN
   TTIO.SwitchLeds(NumLock, CapsLock, ScrollLock)
END SwitchLeds ;

PROCEDURE SwitchScroll (Scroll: BOOLEAN) ;
BEGIN
   TTIO.SwitchScroll(Scroll)
END SwitchScroll ;

PROCEDURE SwitchNum (Num: BOOLEAN) ;
BEGIN
   TTIO.SwitchNum(Num)
END SwitchNum ;

PROCEDURE SwitchCaps (Caps: BOOLEAN) ;
BEGIN
   TTIO.SwitchCaps(Caps)
END SwitchCaps ;


END KeyBoardLEDs.
