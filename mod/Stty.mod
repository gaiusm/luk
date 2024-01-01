IMPLEMENTATION MODULE Stty ;


FROM Environment IMPORT GetEnvironment ;
FROM DeviceConfiguration IMPORT Parity ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StrLib IMPORT StrEqual ;
FROM NumberIO IMPORT StrToCard ;
FROM CmdArgs IMPORT GetArg, Narg ;

(* IMPORT SerIOA ; *)
(* IMPORT SerIOB ; *)
(* IMPORT SerIOB4 ; *)


CONST
   MaxCmdLine = 79 ;

(*
   DoStty - sets the desired ports to a baud rate, parity and stop bits.
*)

PROCEDURE DoStty ;
VAR
   Device,
   a       : ARRAY [0..MaxCmdLine] OF CHAR ;
   p       : Parity ;
   Baud,
   DataBits,
   StopBits: CARDINAL ;
   Xoff,
   Done    : BOOLEAN ;
BEGIN
   GetEnvironment('cmd', a) ;
   IF Narg(a)=6
   THEN
      GetDevice(a, Device) ;
      Baud := GetBaud(a) ;
      DataBits := GetDataBits(a) ;
      StopBits := GetStopBits(a) ;
      Xoff := GetXoff(a) ;
      p := GetParity(a) ;
      Done := FALSE ;
(*
      IF StrEqual(Device, '/dev/tty1')
      THEN
         SerIOA.Init(Baud, StopBits, DataBits, p, Xoff, Done )
      ELSIF StrEqual(Device, '/dev/tty2')
      THEN
         SerIOB.Init(Baud, StopBits, DataBits, p, Done )
      ELSIF StrEqual(Device, '/dev/tty2a')
      THEN
         SerIOB4.Init1(Baud, StopBits, DataBits, p, Xoff, Done )
      ELSIF StrEqual(Device, '/dev/tty2b')
      THEN
         SerIOB4.Init2(Baud, StopBits, DataBits, p, Xoff, Done )
      ELSIF StrEqual(Device, '/dev/tty2c')
      THEN
         SerIOB4.Init3(Baud, StopBits, DataBits, p, Xoff, Done )
      ELSIF StrEqual(Device, '/dev/tty2d')
      THEN
         SerIOB4.Init4(Baud, StopBits, DataBits, p, Xoff, Done )
      END ;
*)
      IF NOT Done
      THEN
         WriteString('Incorrect parameters') ;
         WriteLn
      END
   ELSE
      WriteString('Usage: stty device baud databits stopbits parity [xoff|-xoff]') ;
      WriteLn
   END
END DoStty ;


PROCEDURE GetDevice (a: ARRAY OF CHAR; VAR b: ARRAY OF CHAR) ;
BEGIN
   IF GetArg(a, 1, b)
   THEN
   END
END GetDevice ;


PROCEDURE GetBaud (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   b: CARDINAL ;
BEGIN
   IF GetArg(a, 2, a)
   THEN
      StrToCard(a, b) ;
      RETURN( b )
   ELSE
      RETURN( 0 )
   END
END GetBaud ;


PROCEDURE GetDataBits (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   d: CARDINAL ;
BEGIN
   IF GetArg(a, 3, a)
   THEN
      StrToCard(a, d) ;
      RETURN( d )
   ELSE
      RETURN( 0 )
   END
END GetDataBits ;


PROCEDURE GetStopBits (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   s: CARDINAL ;
BEGIN
   IF GetArg(a, 4, a)
   THEN
      StrToCard(a, s) ;
      RETURN( s )
   ELSE
      RETURN( 0 )
   END
END GetStopBits ;


(*
   GetXoff - 
*)

PROCEDURE GetXoff (a: ARRAY OF CHAR) : BOOLEAN ;
BEGIN
   IF GetArg(a, 6, a)
   THEN
      IF StrEqual(a, '-xoff')
      THEN
         RETURN( FALSE )
      ELSIF StrEqual(a, 'xoff')
      THEN
         RETURN( TRUE )
      ELSE
         WriteString('expecting -xoff or xoff to indicate xon/xoff protocol') ; WriteLn ;
         RETURN( FALSE )
      END
   END
END GetXoff ;


PROCEDURE GetParity (a: ARRAY OF CHAR) : Parity ;
VAR
   p: Parity ;
BEGIN
   IF GetArg(a, 5, a)
   THEN
      IF StrEqual(a, 'none')
      THEN
         p := None
      ELSIF StrEqual(a, 'even')
      THEN
         p := Even
      ELSIF StrEqual(a, 'odd')
      THEN
         p := Odd
      ELSE
         WriteString('Parity must be one from: none, even or odd') ; WriteLn
      END ;
      RETURN( p )
   ELSE
      RETURN( None )
   END
END GetParity ;


END Stty.
