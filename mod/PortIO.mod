IMPLEMENTATION MODULE PortIO ;


(*
   Note that In and Out send a dummy byte to
        port 0x80 which (should be) an unused port.
        Maybe we never need this as In and Out are procedures?

        I borrowed/stole this idea from linux.
        (what a great system Linux is...)
*)


(*
   SlowDownIO - linux mechanism for slowing down port accesses.
*)

PROCEDURE SlowDownIO ;
BEGIN
   ASM VOLATILE ('outb %al,$0x80')   (* does not trash %eax as it moves al to an unused port *)
END SlowDownIO ;


(*
   In8 - returns a BYTE from port, Port.
*)

PROCEDURE In8 (Port: CARDINAL) : BYTE ;
VAR
   v: BYTE ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;  (* linux idea for slowing fast machines down *)
   ASM VOLATILE('movl  %[port], %%edx  ; inb  %%dx, %%al ; movb %%al, %[v]'
                 : [v] "=rm" (v) : [port] "rm" (Port) : "eax", "edx") ;
   ASM VOLATILE('outb %al,$0x80') ;  (* linux idea for slowing fast machines down *)
   RETURN( v )
END In8 ;


(*
   Out8 - sends a byte, Value, to port, Port.
*)

PROCEDURE Out8 (Port: CARDINAL; Value: BYTE) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;  (* linux idea for slowing fast machines down *)
   ASM VOLATILE('movl %[port], %%edx ;  movb %[Value], %%al ; outb %%al,%%dx'
                :: [port] "rm" (Port), [Value] "rm" (Value) : "eax", "edx") ;
   ASM VOLATILE('outb %al,$0x80')    (* linux idea for slowing fast machines down *)
END Out8 ;


(*
   In16 - returns a WORD from port, Port.
          The top 16 bits are 0, the bottom 16 bits are assigned from
          the value of the port.
*)

PROCEDURE In16 (Port: CARDINAL) : WORD ;
VAR
   v: WORD ;
BEGIN
   v := WORD(0) ;
   ASM VOLATILE('outb %al,$0x80') ;  (* linux idea for slowing fast machines down *)
   ASM VOLATILE('movl  %[port], %%edx  ; inw  %%dx, %%ax ; movl %%eax, %[v]'
                 : [v] "=rm" (v) : [port] "rm" (Port) : "eax", "edx") ;
   ASM VOLATILE('outb %al,$0x80') ;  (* linux idea for slowing fast machines down *)
   RETURN( v )
END In16 ;


(*
   Out16 - sends a 16 bit value, Value, to port, Port.
           Value is actually a 32 bit entity but the top
           16 bits are ignored.
*)

PROCEDURE Out16 (Port: CARDINAL; Value: WORD) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;  (* linux idea for slowing fast machines down *)
   ASM VOLATILE('movl %[port], %%edx ;  movl %[Value], %%eax ; outw %%ax,%%dx'
                :: [port] "rm" (Port), [Value] "rm" (Value) : "eax", "edx") ;
   ASM VOLATILE('outb %al,$0x80')    (* linux idea for slowing fast machines down *)
END Out16 ;



(*
   InS8 - reads, n, bytes in from port, Port, to address, a.
*)

PROCEDURE InS8 (Port: CARDINAL; a: ADDRESS; n: CARDINAL) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;   (* linux idea for slowing fast machines down *)
   ASM VOLATILE('movl %[port],%%edx ;  movl %[a],%%edi ; movl %[n],%%ecx ; cld ; rep ; insb'
                :: [port] "rm" (Port), [a] "rm" (a), [n] "rm" (n) : "edx", "edi", "ecx")
END InS8 ;


(*
   InS16 - reads, n, 16 bit words in from port, Port, to address, a.
*)

PROCEDURE InS16 (Port: CARDINAL; a: ADDRESS; n: CARDINAL) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;   (* linux idea for slowing fast machines down *)
   ASM VOLATILE('movl %[port],%%edx ;  movl %[a],%%edi ; movl %[n],%%ecx ; cld ; rep ; insw'
                :: [port] "rm" (Port), [a] "rm" (a), [n] "rm" (n) : "edx", "edi", "ecx")
END InS16 ;


(*
   InS32 - reads, n, 32 bit words in from port, Port, to address, a.
*)

PROCEDURE InS32 (Port: CARDINAL; a: ADDRESS; n: CARDINAL) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;   (* linux idea for slowing fast machines down *)
   ASM VOLATILE('movl %[port],%%edx ;  movl %[a],%%edi ; movl %[n],%%ecx ; cld ; rep ; insl'
                :: [port] "rm" (Port), [a] "rm" (a), [n] "rm" (n) : "edx", "edi", "ecx")
END InS32 ;


(*
   OutS8 - writes, n, bytes to port, Port, from address, a.
*)

PROCEDURE OutS8 (Port: CARDINAL; a: ADDRESS; n: CARDINAL) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;   (* linux idea for slowing fast machines down *)
   ASM('movl %[port],%%edx ;  movl %[a],%%esi ; movl %[n],%%ecx ; cld ; rep ; outsb'
       :: [port] "rm" (Port), [a] "rm" (a), [n] "rm" (n) : "edx", "esi", "ecx") ;
END OutS8 ;


(*
   OutS16 - writes, n, words to port, Port, from address, a.
*)

PROCEDURE OutS16 (Port: CARDINAL; a: ADDRESS; n: CARDINAL) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;   (* linux idea for slowing fast machines down *)
   ASM('movl %[port],%%edx ;  movl %[a],%%esi ; movl %[n],%%ecx ; cld ; rep ; outsw'
       :: [port] "rm" (Port), [a] "rm" (a), [n] "rm" (n) : "edx", "esi", "ecx") ;
END OutS16 ;


(*
   OutS32 - writes, n, 32 bit words to port, Port, from address, a.
*)

PROCEDURE OutS32 (Port: CARDINAL; a: ADDRESS; n: CARDINAL) ;
BEGIN
   ASM VOLATILE('outb %al,$0x80') ;   (* linux idea for slowing fast machines down *)
   ASM('movl %[port],%%edx ;  movl %[a],%%esi ; movl %[n],%%ecx ; cld ; rep ; outsl'
       :: [port] "rm" (Port), [a] "rm" (a), [n] "rm" (n) : "edx", "esi", "ecx") ;
END OutS32 ;


END PortIO.
