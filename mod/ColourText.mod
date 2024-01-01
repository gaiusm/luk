IMPLEMENTATION MODULE ColourText ;


FROM osinit IMPORT ScreenAddress ;
FROM SYSTEM IMPORT ADDRESS ;


CONST
   Width     = 80 ;
   Height    = 25 ;
   underline = 0160C ;
   normal    = 0007C ;


TYPE
   DisplayUnit = RECORD
                    char  : CHAR ;
                    attrib: CHAR ;
                 END ;

   Line        = ARRAY [0..Width-1] OF DisplayUnit ;
   Screen      = ARRAY [0..Height-1] OF Line ;

VAR
   screen: POINTER TO Screen ;


(*
   WriteCharacterAndAttribute - writes character, ch, to screen location,
                                x, y, with attibute colour, colour.
*)

PROCEDURE WriteCharacterAndAttribute (x, y: CARDINAL;
                                      ch: CHAR; colour: CARDINAL) ;
BEGIN
   WITH screen^[y][x] DO
      attrib := VAL(CHAR, colour) ;
      char   := ch
   END
END WriteCharacterAndAttribute ;



PROCEDURE ClearScreen ;
VAR
   i, j: CARDINAL ;
BEGIN
   FOR j := 0 TO Height-1 DO
      FOR i := 0 TO Width-1 DO
         screen^[j][i].char := ' ' ;
         screen^[j][i].attrib := normal
      END
   END
END ClearScreen ;


BEGIN
   screen := ScreenAddress()
END ColourText.
