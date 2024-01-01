MODULE rtsgraph ;

FROM TTIO IMPORT Read ;
FROM SVGA IMPORT GraphicsMode, vga_setmode, vga_drawline, vga_getxdim, vga_getydim,
                 vga_flip, vga_setcolor, vga_clear ;

FROM StrIO IMPORT WriteString ;
FROM StdIO IMPORT Write, PushOutput ;
FROM SYSTEM IMPORT OnOrOff, TurnInterrupts ;
IMPORT Consoles ;
FROM MonStrIO IMPORT DebuggingStream ;


CONST
   HighDefinition = TRUE ;



(*
   LocalDebug - 
*)

PROCEDURE LocalDebug (ch: CHAR) ;
BEGIN
(*   DebugIO.Write(ch) ; *)
   Consoles.Write(1, ch)
END LocalDebug ;


(*
   DrawPattern - 
*)

PROCEDURE DrawPattern ;
VAR
   c,
   i, j,
   MaxX, MaxY: CARDINAL ;
BEGIN
   MaxX := vga_getxdim()-1 ;
   MaxY := vga_getydim()-1 ;
   c := 0 ;
   FOR i := 0 TO MaxX BY 1 DO
      vga_setcolor(c MOD 16) ;
      vga_drawline(0, 0, i, MaxY) ;
      INC(c)
   END ;
   FOR i := MaxY TO 0 BY -1 DO
      vga_setcolor(c MOD 16) ;
      vga_drawline(0, 0, MaxX, i) ;
      INC(c)
   END
END DrawPattern ;


VAR
   ch : CHAR ;
   Old: OnOrOff ;
BEGIN
   Old := TurnInterrupts(On) ;
   PushOutput(LocalDebug) ;
   DebuggingStream(LocalDebug) ;
   WriteString('\nhit any key to test the graphics library..') ;
   Read(ch) ;
   Write(ch) ;
   Consoles.SetCurrentConsole(4) ;
   IF HighDefinition
   THEN
      vga_setmode(G640x480x16)
   ELSE
      vga_setmode(G320x200x256)
   END ;
   LOOP
      vga_clear ;
      DrawPattern ;
      Read(ch) ;
      WriteString('hit any key to test the draw_line..') ;
      Read(ch) ;
      vga_flip ;
      Consoles.SetCurrentConsole(1) ;
      Consoles.Write(1, ch) ;
      Write(ch) ;
      Read(ch) ;
      Consoles.SetCurrentConsole(4) ;
      vga_flip ;
   END
END rtsgraph.
