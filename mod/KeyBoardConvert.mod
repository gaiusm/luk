IMPLEMENTATION MODULE KeyBoardConvert ;


FROM ASCII IMPORT nul, soh, stx, etx, eot, enq, ack, bel,
                  bs , ht , lf , vt , ff , cr , so , si ,
                  dle, dc1, dc2, dc3, dc4, nak, syn, etb,
                  can, em , sub, esc, fs , gs , rs , us ,
                  del ;

FROM NonAscii IMPORT hm , up , puh, lft, rgt, end, dwn, in ,
                     dl , pdh, bw , fw , ce , cpd, chm, cpu ;

FROM KeyBoardLEDs IMPORT SwitchLeds, SwitchNum, SwitchCaps ;
(* FROM SysError IMPORT Reset ; *)
FROM StrLib IMPORT StrCopy ;



CONST
   MaxFunctionDef        = 30 ;    (* MaxFunctionKey definitions      *)
   MaxFunctionKey        = 10 ;    (* MaxFunctionKey keys recognised  *)
   StartFunctionScanCode = 59 ;
   EndFunctionScanCode   = 72 ;    (* 68 for PC : 72 for AT keyboard  *)
   MaxStringSize         = 40 ;
   EscapeCharacter       = '\' ;

VAR
   Bc, Uc, Cc, Ac : ARRAY[ 0..83 ] OF CHAR ;
   Nc             : ARRAY[ 71..83 ] OF CHAR ;

   Shift1, Shift2, NumLock, CapsLock, Ctrl, Alt : BOOLEAN ;
   FunctionString : ARRAY [1..MaxFunctionDef] OF
                    ARRAY [0..MaxStringSize] OF CHAR ;

   ConsoleSelect  : ConsoleSwitchProc ;


PROCEDURE ScanToASCII (ReadDeliver: ProcWrite; ch: CHAR; ConsoleSwitch: ConsoleSwitchProc) ;
VAR
   KeyDown : BOOLEAN ;
   Ord     : CARDINAL ;
BEGIN
   ConsoleSelect := ConsoleSwitch ;
   Ord := ORD( ch ) ;
   KeyDown := (Ord < 128) ;
   IF NOT KeyDown
   THEN
      Ord := Ord-128 ;
      ch := CHR( Ord )
   END ;

   CASE Ord OF

   29: Ctrl := KeyDown |
   42: Shift1 := KeyDown |
   54: Shift2 := KeyDown |
   56: Alt := KeyDown |
   58: IF KeyDown
       THEN
          CapsLock := NOT CapsLock ;
          SwitchCaps(CapsLock)
       END |
   69: IF KeyDown
       THEN
          NumLock := NOT NumLock ;
          SwitchNum(NumLock)
       END |
   70: IF KeyDown AND Ctrl THEN (* Reset *) END

   ELSE
      IF KeyDown AND (Ord<84) AND (Ord>0)   (* Not KeyBoard Full *)
      THEN
         IF (Ord>0) AND (Ord<16)
         THEN
            IF Shift1 OR Shift2
            THEN
               ReadDeliver( Uc[Ord] )
            ELSE
               ReadDeliver( Bc[Ord] )
            END
         ELSIF (Ord>70) AND (Ord<84) AND
               ((NumLock AND (NOT( Shift1 OR Shift2 ))) OR
               (Shift1 OR Shift2) AND (NOT NumLock))
         THEN
            ReadDeliver( Nc[Ord] )
         ELSIF IsFunctionKey(Ord)
         THEN
            ProcessFunctionKey(Ord, ReadDeliver) ;
         ELSIF Alt
         THEN
            ReadDeliver( Ac[Ord] )
         ELSIF Ctrl
         THEN
            ReadDeliver( Cc[Ord] )
         ELSIF (CapsLock AND (NOT( Shift1 OR Shift2 ))) OR
               ((Shift1 OR Shift2) AND (NOT CapsLock))
         THEN
            ReadDeliver( Uc[Ord] )
         ELSIF (Ord>70) AND (Ord<84)
         THEN
            ReadDeliver( Nc[Ord] )
         ELSE
            ReadDeliver( Bc[Ord] )
         END ;
(*
         IF ch<>377C
         THEN
            IF ch=nul
            THEN
               ReadDeliver( ch ) ;
               ch := CHR( Ord )
            END ;
            ReadDeliver( ch )
         END
*)
      END
   END

END ScanToASCII ;


PROCEDURE IsFunctionKey (ScanCode: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
           (ScanCode>=StartFunctionScanCode) AND
           (ScanCode<=EndFunctionScanCode)
         )
END IsFunctionKey ;


PROCEDURE ProcessFunctionKey (ScanCode: CARDINAL; ReadDeliver: ProcWrite) ;
BEGIN
   IF Alt
   THEN
      ConsoleSelect(ScanCode-StartFunctionScanCode+1) ;
   ELSIF Ctrl
   THEN
      PutString(ScanCode-StartFunctionScanCode+1+
                MaxFunctionKey+MaxFunctionKey, ReadDeliver)
   ELSIF CapsLock AND (NOT( Shift1 OR Shift2))
   THEN
      PutString(ScanCode-StartFunctionScanCode+1+MaxFunctionKey, ReadDeliver)
   ELSE
      PutString(ScanCode-StartFunctionScanCode+1, ReadDeliver)
   END
END ProcessFunctionKey ;


PROCEDURE PutString (Function: CARDINAL; Write: ProcWrite) ;
VAR
   i: CARDINAL ;
   ch: CHAR ;
BEGIN
   i := 0 ;
   REPEAT
      ch := FunctionString[Function][i] ;
      IF ch#nul
      THEN
         IF (ch=EscapeCharacter) AND (i<MaxStringSize)
         THEN
            INC(i) ;
            ch := FunctionString[Function][i] ;
            IF (ch>='A') AND (ch<='Z')
            THEN
               Write(CHR(ORD(ch)-ORD('A')+1))
            ELSIF (ch>='a') AND (ch<='z')
            THEN
               Write(CHR(ORD(ch)-ORD('a')+1))
            END
         ELSE
            Write(ch)
         END
      END ;
      INC(i) ;
   UNTIL (i=MaxStringSize) OR (FunctionString[Function][i]=nul)
END PutString ;


PROCEDURE SetFunctionString (Function: CARDINAL; a: ARRAY OF CHAR) ;
BEGIN
   IF (Function>0) AND (Function<MaxFunctionDef)
   THEN
      StrCopy(a, FunctionString[Function])
   END
END SetFunctionString ;

   
PROCEDURE InitFunctionStrings ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 1 TO EndFunctionScanCode-StartFunctionScanCode+1 DO
      SetFunctionString(i, '')
   END ;
   (* Normal *)
   SetFunctionString(1, 'mfaaaaaaaa') ;
   SetFunctionString(2, 'mf') ;
   SetFunctionString(3, 'aaaaaaaaaa') ;
   SetFunctionString(4, 'pppppppppp') ;
   SetFunctionString(5, '3rfr3v') ;
   SetFunctionString(6, '3lfl3v') ;
   SetFunctionString(7, '3rmr3v') ;
   SetFunctionString(8, 'aar1l1l1aar1l1l1aar1l1l1aa') ;
   SetFunctionString(9, 'ler11') ;
   SetFunctionString(10, 'rel11') ;
   (* SHIFT *)
   SetFunctionString(11, 'fmfpppppppp') ;
   SetFunctionString(12, 'fmf') ;
   SetFunctionString(13, 'aaaaaaaa') ;
   SetFunctionString(14, 'o1vcv1') ;
   SetFunctionString(15, '3rfr3v') ;
   SetFunctionString(16, '3lfl3v') ;
   SetFunctionString(17, '3rmr3v') ;
   SetFunctionString(18, 'u7\mppppp') ;
   SetFunctionString(19, 'ler11') ;
   SetFunctionString(20, 'rel11') ;
   (* CTRL *)
   SetFunctionString(21, 'fmfpppppppp') ;
   SetFunctionString(22, 'fmf') ;
   SetFunctionString(23, 'aaaaaaaa') ;
   SetFunctionString(24, 'eo1vcu1\mv1') ;
   SetFunctionString(25, '3rfr3v') ;
   SetFunctionString(26, '3lfl3v') ;
   SetFunctionString(27, '3rmr3v') ;
   SetFunctionString(28, 'u7\mppppp') ;
   SetFunctionString(29, 'ler11') ;
   SetFunctionString(30, 'rel11') ;
END InitFunctionStrings ;


PROCEDURE InitScanArray ;
VAR
   x: CARDINAL ;
BEGIN
   FOR x := 1 TO 83 DO
      Bc[ x ] := 377C ;
      Uc[ x ] := 377C ;
      Ac[ x ] := 377C ;
      Cc[ x ] := 377C
   END ;

   Bc[ 1 ] := esc ;  Uc[ 1 ] := esc ;  Cc[ 1 ] := esc ;
   
   Bc[ 14 ] := bs ;  Uc[ 14 ] := bs ;  Cc[ 14 ] := del ;
   Bc[ 15 ] := ht ;  Uc[ 15 ] := nul ;

   Bc[ 28 ] := cr ;  Uc[ 28 ] := cr ;  Cc[ 28 ] := lf ;
   
   Bc[ 55 ] := '*';                 ;  Cc[ 55 ] := nul ;

   Bc[ 57 ] := ' ';  Uc[ 57 ] := ' ';  Cc[ 57 ] := ' ';

   Bc[ 71 ] := hm ;                 ;  Cc[ 71 ] := chm;
   Bc[ 72 ] := up ;
   Bc[ 73 ] := puh;                 ;  Cc[ 73 ] := cpu;
   Bc[ 74 ] := '-';  Uc[ 74 ] := '-';
   Bc[ 75 ] := lft;                 ;  Cc[ 75 ] := bw ;

   Bc[ 77 ] := rgt;                 ;  Cc[ 77 ] := fw ;
   Bc[ 78 ] := '+';  Uc[ 78 ] := '+';
   Bc[ 79 ] := end;                 ;  Cc[ 79 ] := ce ;
   Bc[ 80 ] := dwn;
   Bc[ 81 ] := pdh;                 ;  Cc[ 81 ] := cpd;
   Bc[ 82 ] := in ;
   Bc[ 83 ] := dl ;


   Cc[ 3 ] := nul ;  Cc[ 7 ] := rs  ;  Cc[ 12 ] := us ;
   Cc[ 16 ] := dc1;  Cc[ 17 ] := etb;  Cc[ 18 ] := enq;
   Cc[ 19 ] := dc2;  Cc[ 20 ] := dc4;  Cc[ 21 ] := em ;
   Cc[ 22 ] := nak;  Cc[ 23 ] := ht ;  Cc[ 24 ] := si ;
   Cc[ 25 ] := dle;  Cc[ 26 ] := esc;  Cc[ 27 ] := gs ;
   Cc[ 30 ] := soh;  Cc[ 31 ] := dc3;  Cc[ 32 ] := eot;
   Cc[ 33 ] := ack;  Cc[ 34 ] := bel;  Cc[ 35 ] := bs ;
   Cc[ 36 ] := lf ;  Cc[ 37 ] := vt ;  Cc[ 38 ] := ff ;
   Cc[ 43 ] := fs ;  Cc[ 44 ] := sub;  Cc[ 45 ] := can;
   Cc[ 46 ] := etx;  Cc[ 47 ] := syn;  Cc[ 48 ] := stx;
   Cc[ 49 ] := so ;  Cc[ 50 ] := cr ;


   FOR x := 2 TO 13 DO
      Ac[ x ] := nul
   END ;
   FOR x := 16 TO 25 DO
      Ac[ x ] := nul
   END ;
   FOR x := 30 TO 38 DO
      Ac[ x ] := nul 
   END ;
   FOR x := 30 TO 38 DO
      Ac[ x ] := nul
   END ;
   FOR x := 44 TO 50 DO
      Ac[ x ] := nul
   END ;
   FOR x := 59 TO 68 DO
      Bc[ x ] := nul ;
      Uc[ x ] := nul ;
      Cc[ x ] := nul ;
      Ac[ x ] := nul
   END ;

   Place('1234567890-=', Bc, 2) ;
   Place('qwertyuiop[]', Bc, 16) ;
   Place('asdfghjkl;', Bc, 30) ;
   Bc[ 40 ] := CHAR( 39 ) ;
   Bc[ 41 ] := '`' ;
   Place('\zxcvbnm,./', Bc, 43) ;

   Place('!@#$%^&*()_+', Uc, 2) ;
   Place('QWERTYUIOP{}', Uc, 16) ;
   Place('ASDFGHJKL:"~', Uc, 30) ;
   Place('|ZXCVBNM<>?', Uc, 43) ;

   Nc[ 71 ] := '7' ;  Nc[ 72 ] := '8' ;  Nc[ 73 ] := '9' ;
   Nc[ 74 ] := '-' ;  Nc[ 75 ] := '4' ;  Nc[ 76 ] := '5' ;
   Nc[ 77 ] := '6' ;  Nc[ 78 ] := '+' ;  Nc[ 79 ] := '1' ;
   Nc[ 80 ] := '2' ;  Nc[ 81 ] := '3' ;  Nc[ 82 ] := '0' ;
   Nc[ 83 ] := '.' ;
END InitScanArray ;


PROCEDURE Place (Str1: ARRAY OF CHAR ; VAR Str2: ARRAY OF CHAR ;
                 n: CARDINAL) ;
VAR
   t, High1: CARDINAL ;
BEGIN
   High1 := HIGH( Str1 ) ;
   t := 0 ;
   WHILE (t<=High1) DO
      Str2[ t+n ] := Str1[ t ] ;
      t := t + 1 ;
   END ;
END Place ;


(*
   SetStatus - sets the status of the shift, alt, ctrl, capslock, numlock
               keys.
*)

PROCEDURE SetStatus ;
BEGIN
   Shift1 := FALSE ;
   Shift2 := FALSE ;
   Ctrl := FALSE ;
   Alt := FALSE ;
   CapsLock := FALSE ;
   NumLock := FALSE
END SetStatus ;


BEGIN
   InitScanArray ;
   InitFunctionStrings ;
   SetStatus
END KeyBoardConvert.
