IMPLEMENTATION MODULE osinit ;

FROM OSParameters IMPORT ExtendedMemAddr, VideoAddrAddr,
                         OSSizeAddr, NoOfSectorsAddr,
                         DebuggingAddr, StackSizeAddr ;

FROM SYSTEM IMPORT BYTE ;


CONST
   KiloByte       = 1024 ;
   Meg1           = KiloByte * KiloByte ;
   BytesPerSector = 512 ;


(* ***************************************************
(*
   GetKey - waits for a key to be pressed.
*)

PROCEDURE GetKey ;
BEGIN
   ASM
      pushl %eax
      pushl %ebx
again:
      in    $0x60, %al     # get scan code
      movb  %al, %ah       # store scan code
      in    $0x61, %al     # send ack to kbd controller
      orb    $0x80, %al
      out   %al, $0x61
      andb  $0x7f, %al
      out   %al, $0x61
      cmpb  $0xf0, %ah     # this byte prefaces key releases
      je   finish          # key released

      # discard scan code of released key
      in    $0x61, %al     # send ack to kbd controller
      orb   $0x80, %al
      out   %al, $0x61
      andb  $0x7f, %al
      out   %al, $0x61
      jmp   again
finish:
      popl %eax
      popl %ebx
   END
END GetKey ;
********************************************************** *)

(*
   Get16 - returns a CARDINAL whose value is taken from a
           2 byte entity at address, a.
*)

PROCEDURE Get16 (a: ADDRESS) : CARDINAL ;
VAR
   shortPtr: POINTER TO BYTE ;
   result  : CARDINAL ;
BEGIN
   shortPtr := a ;
   result := VAL(CARDINAL, shortPtr^) ;
   INC(shortPtr) ;
   result := result + (0100H * VAL(CARDINAL, shortPtr^)) ;
   RETURN( result )
END Get16 ;


(*
   Put16 - places a 2 byte entity into address, a.
*)

PROCEDURE Put16 (a: ADDRESS; c: CARDINAL) ;
VAR
   shortPtr: POINTER TO BYTE ;
BEGIN
   shortPtr := a ;
   shortPtr^ := VAL(BYTE, c MOD 0100H) ;
   INC(shortPtr) ;
   shortPtr^ := VAL(BYTE, c DIV 0100H)
END Put16 ;


(*
   RetrieveOSParameters - collects values needed by the realtime
                          system.
*)

PROCEDURE RetrieveOSParameters (VAR ExtendedMem, Video,
                                    Size, NoOfSectors,
                                    Debugging, StackSize: CARDINAL) ;
BEGIN
   ExtendedMem  := Get16(ExtendedMemAddr) ;
   Video        := Get16(VideoAddrAddr) ;
   Size         := Get16(OSSizeAddr) ;
   NoOfSectors  := Get16(NoOfSectorsAddr) ;
   Debugging    := Get16(DebuggingAddr) ;
   StackSize    := Get16(StackSizeAddr)
END RetrieveOSParameters ;


(*
   ScreenAddress - returns the start address of the video memory.
*)

PROCEDURE ScreenAddress () : ADDRESS ;
BEGIN
   RETURN( ADDRESS(Video) )
END ScreenAddress ;


(*
   ExtendedMemoryEnd - returns the maximum extended memory address.
*)

PROCEDURE ExtendedMemoryEnd () : ADDRESS ;
BEGIN
   RETURN( ADDRESS(ExtendedMem) )
END ExtendedMemoryEnd ;


(*
   ExtendedMemoryStart - returns the start of the extended memory.
*)

PROCEDURE ExtendedMemoryStart () : ADDRESS ;
BEGIN
   RETURN( ADDRESS(Meg1) )
END ExtendedMemoryStart ;


(*
   BaseMemoryEnd - returns the maximum base memory address.
*)

PROCEDURE BaseMemoryEnd () : ADDRESS ;
BEGIN
   RETURN( ADDRESS(640*1024) )
END BaseMemoryEnd ;


(*
   BaseMemoryStart - returns the start of the base memory.
*)

PROCEDURE BaseMemoryStart () : ADDRESS ;
BEGIN
   RETURN( ADDRESS(OSSize)  (* should add the data size when we know it *) )
END BaseMemoryStart ;


(*
   SizeOfOS - returns the size of the realtime system.
*)

PROCEDURE SizeOfOS () : CARDINAL ;
BEGIN
   RETURN( OSSize )
END SizeOfOS ;


(*
   DebuggingWithGDB - returns TRUE if we are remote debugging with gdb.
*)

PROCEDURE DebuggingWithGDB () : BOOLEAN ;
BEGIN
   RETURN( Debugging#0 )
END DebuggingWithGDB ;


(*
   SetDebugging - sets the debugging variable.
*)

PROCEDURE SetDebugging (d: CARDINAL) ;
BEGIN
   Debugging := d
END SetDebugging ;


(*
   GetDebugging - returns the debugging variable.
                  0  means not debugging with GDB
                  1  means debugging via com1
                  2  means debugging via com2
*)

PROCEDURE GetDebugging () : CARDINAL ;
BEGIN
   RETURN( Debugging )
END GetDebugging ;


(*
   MainStackSize - returns the size of mains stack.
*)

PROCEDURE MainStackSize () : CARDINAL ;
BEGIN
   RETURN( StackSize )
END MainStackSize ;


VAR
   ExtendedMem, Video,
   OSSize, NoOfSectors,
   Debugging, StackSize: CARDINAL ;


(*
   Init - retrieve all parameters from second bootstage.
*)

PROCEDURE Init ;
BEGIN
   RetrieveOSParameters(ExtendedMem, Video, OSSize, NoOfSectors,
                        Debugging, StackSize) ;
   Video := Video*01000H ;
   ExtendedMem := ExtendedMem*KiloByte + Meg1 ;
   OSSize := OSSize * BytesPerSector ;
   StackSize := StackSize * KiloByte
END Init ;


(*
   ResetParameters - replaces the parameters back to the original position
                     ready for a reboot.
*)

PROCEDURE ResetParameters ;
BEGIN
   Put16(ExtendedMemAddr, (ExtendedMem-Meg1) DIV KiloByte) ;
   Put16(VideoAddrAddr, Video DIV 01000H) ;
   Put16(OSSizeAddr, OSSize DIV BytesPerSector) ;
   Put16(NoOfSectorsAddr, NoOfSectors) ;
   Put16(DebuggingAddr, Debugging) ; 
   Put16(StackSizeAddr, StackSize DIV KiloByte)
END ResetParameters ;


END osinit.
