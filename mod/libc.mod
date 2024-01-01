IMPLEMENTATION MODULE libc ;

(*
   exit - does nothing and enters an infinite loop.
          This function is here to provide compatibility
          for other modules.
*)

PROCEDURE exit (r: INTEGER) ;
BEGIN
   LOOP
   END
END exit ;

END libc.
