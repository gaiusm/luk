MODULE rtsload ;

(*
    Title      : rtsload
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Aug  7 19:00:32 1995
    Last edit  : Mon Aug  7 19:00:32 1995
    Description: provides a simple application module which calls the load
                 routine.
*)

IMPORT libg ;
FROM SLoad IMPORT LoadProgram ;


BEGIN
   libg.Init ;   (* initialize the serial link *)
   LoadProgram
END rtsload.
