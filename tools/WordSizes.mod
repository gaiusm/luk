IMPLEMENTATION MODULE WordSizes ;


FROM NumberIO IMPORT WriteHex, WriteCard ;


(*
   ShortWordToCardinal - converts a short word into a cardinal.
*)

PROCEDURE ShortWordToCardinal (s: SHORTWORD) : CARDINAL ;
BEGIN
   RETURN( VAL(CARDINAL, s.ByteHi) * 0100H + VAL(CARDINAL, s.ByteLo) )
END ShortWordToCardinal ;


(*
   LongWordToCardinal - converts a short word into a cardinal.
*)

PROCEDURE LongWordToCardinal (l: LONGWORD) : CARDINAL ;
BEGIN
   RETURN(
           VAL(CARDINAL, l.WordHi.ByteHi) * 01000000H +
           VAL(CARDINAL, l.WordHi.ByteLo) * 010000H +
           VAL(CARDINAL, l.WordLo.ByteHi) * 0100H +
           VAL(CARDINAL, l.WordLo.ByteLo)
      	 )
END LongWordToCardinal ;


(*
   CardinalToShortWord - converts a cardinal into a short word.
*)

PROCEDURE CardinalToShortWord (c: CARDINAL) : SHORTWORD ;
VAR
   s: SHORTWORD ;
BEGIN
   s.ByteLo := VAL(BYTE, c MOD 0100H) ;
   s.ByteHi := VAL(BYTE, (c DIV 0100H) MOD 0100H) ;
   RETURN( s )
END CardinalToShortWord ;


(*
   CardinalToLongWord - converts a cardinal into a long word.
*)

PROCEDURE CardinalToLongWord (c: CARDINAL) : LONGWORD ;
VAR
   l: LONGWORD ;
BEGIN
   l.WordLo.ByteLo := VAL(BYTE, c MOD 0100H) ;
   l.WordLo.ByteHi := VAL(BYTE, (c DIV 0100H) MOD 0100H) ;
   l.WordHi.ByteLo := VAL(BYTE, (c DIV 010000H) MOD 0100H) ;
   l.WordHi.ByteHi := VAL(BYTE, (c DIV 01000000H) MOD 0100H) ;
   RETURN( l )
END CardinalToLongWord ;


(*
   FlipShortWord - returns the byte flipped SHORTWORD, s.
*)

PROCEDURE FlipShortWord (s: SHORTWORD) : SHORTWORD ;
VAR
   r: SHORTWORD ;
BEGIN
   r.ByteHi := s.ByteLo ;
   r.ByteLo := s.ByteHi ;
   RETURN( r )
END FlipShortWord ;


(*
   FlipLongWord - returns the byte flipped LONGWORD, l.
*)

PROCEDURE FlipLongWord (l: LONGWORD) : LONGWORD ;
VAR
   r: LONGWORD ;
BEGIN
   r.WordLo := FlipShortWord(l.WordHi) ;
   r.WordHi := FlipShortWord(l.WordLo) ;
   RETURN( r )
END FlipLongWord ;


(*
   WriteLong - writes the long out in hex characters.
*)

PROCEDURE WriteLong (l: LONGWORD) ;
BEGIN
   WriteHex(VAL(INTEGER, l.WordHi.ByteHi) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, l.WordHi.ByteLo) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, l.WordLo.ByteHi) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, l.WordLo.ByteLo) MOD 0100H, 2) ;
   WriteCard(LongWordToCardinal(l), 8)
END WriteLong ;


(*
   WriteShort - writes the short out in hex characters.
*)

PROCEDURE WriteShort (s: SHORTWORD) ;
BEGIN
   WriteHex(VAL(INTEGER, s.ByteHi) MOD 0100H, 2) ;
   WriteHex(VAL(INTEGER, s.ByteLo) MOD 0100H, 2) ;
   WriteCard(ShortWordToCardinal(s), 6)
END WriteShort ;



END WordSizes.
