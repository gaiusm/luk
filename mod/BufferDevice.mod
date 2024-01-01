IMPLEMENTATION MODULE BufferDevice ;


FROM SYSTEM IMPORT ADDRESS ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Debug IMPORT DebugString ;

FROM Executive IMPORT SEMAPHORE, Signal, Wait, InitSemaphore ;

CONST
   MaxBufferSize = 010H ;

TYPE
   Buffer = POINTER TO buffer ;
   buffer = RECORD
               ItemAvailable : SEMAPHORE ;
               SpaceAvailable: SEMAPHORE ;
               Mutex         : SEMAPHORE ;
               Buf           : ARRAY [0..MaxBufferSize] OF CHAR ;
               in            : CARDINAL ;
               out           : CARDINAL ;
            END ;


(*
   InitBuffer - creates, and initializes, a buffer and returns it.
*)

PROCEDURE InitBuffer () : Buffer ;
VAR
   b: Buffer ;
BEGIN
   DebugString('InitBuffer\n') ;
   NEW( b ) ;
   WITH b^ DO
      ItemAvailable := InitSemaphore(0, 'ItemAvailable') ;
      SpaceAvailable := InitSemaphore(MaxBufferSize, 'SpaceAvailable') ;
      Mutex := InitSemaphore(1, 'Mutex') ;
      in := 0 ;
      out := 0
   END ;
   RETURN( b )
END InitBuffer ;


(*
   KillBuffer - destroys the buffer and relinquishes all associated
                resources.
*)

PROCEDURE KillBuffer (VAR b: Buffer) ;
BEGIN
   WITH b^ DO
(*
   when we implement KillSemaphore
      KillSemaphore( ItemAvailable ) ;
      KillSemaphore( SpaceAvailable ) ;
      KillSemaphore( Mutex )
*)
   END ;
   DISPOSE( b )
END KillBuffer ;


(*
   ReadBuffer - reads a character, ch, from the buffer, b.
*)

PROCEDURE ReadBuffer (b: Buffer; VAR ch: CHAR) ;
BEGIN
   WITH b^ DO
      Wait( ItemAvailable ) ;
      Wait( Mutex ) ;
      ch := Buf[out] ;
      out := (out+1) MOD MaxBufferSize ;
      Signal( Mutex ) ;
      Signal( SpaceAvailable )
   END
END ReadBuffer ;


(*
   WriteBuffer - places a character, ch, into buffer, b.
*)

PROCEDURE WriteBuffer (b: Buffer; ch: CHAR) ;
BEGIN
   WITH b^ DO
      Wait( SpaceAvailable ) ;
      Wait( Mutex ) ;
      Buf[in] := ch ;
      in := (in+1) MOD MaxBufferSize ;
      Signal( Mutex ) ;
      Signal( ItemAvailable )
   END
END WriteBuffer ;


END BufferDevice.
