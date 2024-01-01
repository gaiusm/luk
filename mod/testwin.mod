MODULE testwin ;

FROM SYSTEM IMPORT ADDRESS ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, WriteHex ;
FROM WindowDevice IMPORT Window, InitWindow, SetWindow, TitleWindow ;
FROM Colours IMPORT Blue, Red ;

VAR
   w: Window ;
BEGIN
   WriteString('Before InitWindow') ; WriteLn ;
   InitWindow(w) ;
   WriteString('Before SetWindow') ; WriteLn ;
   SetWindow(w, Blue, Red, 60, 20, 2, 2, TRUE) ;
   WriteString('Before TitleWindow') ; WriteLn ;
   TitleWindow(w, 'Gaius first window') ;
   WriteString('Before LOOP END') ; WriteLn ;
   LOOP END ;
END testwin.
