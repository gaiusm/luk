MODULE lowlevel ;

FROM SYSTEM IMPORT ADDRESS ;


(*
    TRANSFER - 
*)

PROCEDURE TRANSFER (p1, p2: ADDRESS) ;
BEGIN
   ASM
   push  eax
   push  ebx
   push  ecx
   END
END TRANSFER ;


BEGIN
END lowlevel.
