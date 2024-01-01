MODULE rtsecho ;

(*
    Title      : rtsecho
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Aug  7 19:00:32 1995
    Last edit  : Mon Aug  7 19:00:32 1995
    Description: simple echo facility to test that the serial link is operating.
*)

FROM ASCII IMPORT cr, lf ;
IMPORT libg ;


VAR
   ch: CHAR ;
BEGIN
   libg.Init ;
   libg.Write('>') ; libg.Write(' ') ;
   LOOP
      libg.Read(ch) ;
      libg.Write(ch) ;
      IF ch=cr
      THEN
         libg.Write(lf) ;
         libg.Write('>') ; libg.Write(' ') ;
      END
   END
END rtsecho.
