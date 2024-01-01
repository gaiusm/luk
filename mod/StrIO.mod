IMPLEMENTATION MODULE StrIO ;


FROM ASCII IMPORT cr, nul, lf, bel, del, bs, nak, etb, eof ;
FROM StdIO IMPORT Read, Write ;


(* %%%FORWARD%%%
PROCEDURE WriteLn ; FORWARD ;
PROCEDURE ReadString (VAR a: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE WriteString (a: ARRAY OF CHAR) ; FORWARD ;
PROCEDURE Erase ; FORWARD ;
PROCEDURE Echo (ch: CHAR) ; FORWARD ;
PROCEDURE AlphaNum (ch: CHAR) : BOOLEAN ; FORWARD ;
   %%%FORWARD%%% *)


VAR
   IsATTY: BOOLEAN ;   (* Is default input from the keyboard? *)


(*
   WriteLn - writes a carriage return and a newline
             character.
*)

PROCEDURE WriteLn ;
BEGIN
   Echo( cr ) ;
   Write( lf )
END WriteLn ;


(*
   ReadString - reads a sequence of characters into a string.
                Line editing accepts Del, Ctrl H, Ctrl W and
                Ctrl U.
*)

PROCEDURE ReadString (VAR a: ARRAY OF CHAR) ;
VAR
   n    ,
   high : CARDINAL ;
   ch   : CHAR ;
BEGIN
   high := HIGH( a ) ;
   n := 0 ;
   REPEAT
      Read( ch ) ;
      IF (ch=del) OR (ch=bs)
      THEN
         IF n=0
         THEN
            Write( bel )
         ELSE
            Erase ;
            DEC( n )
         END
      ELSIF ch=nak    (* Ctrl U *)
      THEN
         WHILE n>0 DO
            Erase ;
            DEC( n )
         END
      ELSIF ch=etb    (* Ctrl W *)
      THEN
         IF n=0
         THEN
            Echo( bel )
         ELSIF AlphaNum(a[n-1])
         THEN
            REPEAT
               Erase ;
               DEC(n)
            UNTIL (n=0) OR (NOT AlphaNum(a[n-1]))
         ELSE
            Erase ;
            DEC(n)
         END
      ELSIF n <= high
      THEN
         IF ch = cr
         THEN
            a[n] := nul ;
            INC( n )
         ELSIF ch>=' '
         THEN
            Echo( ch ) ;
            a[n] := ch ;
            INC( n )
         ELSIF ch=eof
         THEN
            a[n] := ch ;
            INC( n ) ;
            ch := cr;
            IF n<=high
            THEN
               a[n] := nul
            END
         END
      ELSIF ch#cr
      THEN
         Echo( bel )
      END
   UNTIL ch = cr
END ReadString ;


(*
   WriteString - writes a string to the default output.
*)

PROCEDURE WriteString (a: ARRAY OF CHAR) ;
VAR
   n    ,
   high : CARDINAL ;
BEGIN
   high := HIGH( a ) ;
   n := 0 ;
   WHILE (n <= high) AND (a[n] # nul) DO
      IF a[n]='\'
      THEN
         IF n+1<=high
         THEN
            IF a[n+1]='n'
            THEN
               WriteLn ;
               INC(n)
            ELSIF a[n+1]='\'
            THEN
               Write('\') ;
               INC(n)
            END
         END
      ELSE
         Write( a[n] )
      END ;
      INC( n )
   END
END WriteString ;


(*
   Erase - writes a backspace, space and backspace to remove the
           last character displayed.
*)

PROCEDURE Erase ;
BEGIN
   Echo( bs ) ;
   Echo(' ') ;
   Echo( bs )
END Erase ;


(*
   Echo - echos the character, ch, onto the output channel if IsATTY
          is true.
*)

PROCEDURE Echo (ch: CHAR) ;
BEGIN
   IF IsATTY
   THEN
      Write(ch)
   END
END Echo ;


(*
   AlphaNum- returns true if character, ch, is an alphanumeric character.
*)

PROCEDURE AlphaNum (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN ((ch>='a') AND (ch<='z')) OR
          ((ch>='A') AND (ch<='Z')) OR
          ((ch>='0') AND (ch<='9'))
END AlphaNum ;


BEGIN
   IsATTY := TRUE
END StrIO.
