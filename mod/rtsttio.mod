MODULE rtsttio ;

FROM Ps IMPORT DoPs ;
FROM Scn IMPORT Write ;
FROM TTIO IMPORT Read ;
FROM ASCII IMPORT nl, cr ;

VAR
   ch: CHAR ;
BEGIN
   Write('>') ; Write(' ') ;
   LOOP
      Read(ch) ;
      Write(ch) ;
      IF ch='p'
      THEN
         Write(nl) ; Write(cr) ;
         DoPs
      END
   END
END rtsttio.
