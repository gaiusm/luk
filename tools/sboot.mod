MODULE sboot ;

(*
    Title      : sboot
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Aug  7 14:15:50 1995
    Last edit  : Mon Aug  7 14:15:50 1995
    Description: provides a simple boot loader using the serial device.
*)

FROM FIO IMPORT File, OpenToWrite, OpenToRead, Close, WriteChar,
                ReadChar, IsNoError, GetUnixFileDescriptor ;

FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrCopy, StrEqual, StrConCat ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write, PushOutput ;
FROM Args IMPORT GetArg ;
FROM libc IMPORT exit, system, write ;
FROM SYSTEM IMPORT ADR, BYTE ;
FROM NumberIO IMPORT WriteCard, CardToStr ;

FROM Selective IMPORT Timeval, SetOfFd, InitTime, KillTime,
                      InitSet, KillSet, FdZero, FdIsSet, FdSet,
                      MaxFdsPlusOne, Select, ReadCharRaw, WriteCharRaw ;

FROM AOUT IMPORT Examine,
                 GetByteFromText, GetByteFromData,
                 GetTextPhysicalAddress, GetDataPhysicalAddress,
                 GetTextVirtualAddress, GetDataVirtualAddress,
                 GetDataPhysicalSize, GetDataVirtualSize,
                 GetTextPhysicalSize, GetTextVirtualSize ;


CONST
   RemoteDebugging=              TRUE ;  (* do you want to display all remote debugging after loader finished? *)
   MaxString      =              4096 ;
   MaxFrame       =               256 ;  (* 64..1024 *)
   EXE            =           'a.out' ;
   DEVICE         =      '/dev/ttys0' ;
   HeaderSequence =            'abcd' ;
   MaxHeader      =                 4 ;
   TimeoutSecs    =                 3 ;
   FrameDataLimit =        MaxFrame-3 ;  (* 2 for checksum 1 for # *)
   FramePulse     = 2048 DIV MaxFrame ;
   BaudRate       =             38400 ;

VAR
   Frame        : ARRAY [0..MaxFrame] OF CHAR ;
   DeviceName,
   ExeFileName  : ARRAY [0..MaxString] OF CHAR ;
   fi, fo       : File ;
   unixOut,
   unixIn       : INTEGER ;
   SeqNo        : CARDINAL ;
   TotalBytes,
   TotalChecksum: CARDINAL ;


(*
   LocalWrite - 
*)

PROCEDURE LocalWrite (ch: CHAR) ;
BEGIN
   WriteCharRaw(1, ch)
END LocalWrite ;


(*
   Download - 
*)

PROCEDURE Download ;
BEGIN
   fo := OpenToWrite(DeviceName) ;
   IF IsNoError(fo)
   THEN
      fi := OpenToRead(DeviceName) ;
      IF IsNoError(fi)
      THEN
         IF Examine(ExeFileName)
         THEN
            PushOutput(LocalWrite) ;
            unixIn := GetUnixFileDescriptor(fi) ;
            unixOut := GetUnixFileDescriptor(fo) ;
            SendExe
         ELSE
            WriteString('failed to find the executable: ') ; WriteString(ExeFileName) ; WriteLn ;
            exit(1)
         END
      ELSE
         WriteString('failed to open device: ') ; WriteString(DeviceName) ;
         WriteString(' for reading') ; WriteLn ;
         exit(1)
      END
   ELSE
      WriteString('failed to open device: ') ; WriteString(DeviceName) ;
      WriteString(' for writing') ; WriteLn ;
      exit(1)
   END
END Download ;


(*
   SendExe - 
*)

PROCEDURE SendExe ;
BEGIN
   WriteString('Will take approximately:') ;
   WriteCard((GetTextVirtualSize()+GetDataVirtualSize()) DIV ((BaudRate DIV (10*2)) * 60), 4) ; WriteString(' mins') ;
   WriteCard(((GetTextVirtualSize()+GetDataVirtualSize()) DIV (BaudRate DIV (10*2))) MOD 60, 3) ; WriteString(' secs') ; WriteLn ;
   SeqNo := 1 ;
   TotalChecksum := 0 ;
   TotalBytes := 0 ;
   SendHeader ;
   SendSize ;
   SendText ;
   SendData ;
   IF TotalBytes#GetTextVirtualSize()+GetDataVirtualSize()
   THEN
      WriteLn ;
      WriteString('Bug in sboot - total bytes sent does not equal text + data size') ; WriteLn ;
      WriteString('total bytes:') ; WriteCard(TotalBytes, 8) ; WriteLn ;
      WriteString('exe size   :') ; WriteCard(GetTextVirtualSize()+GetDataVirtualSize(), 8) ; WriteLn ;
   END ;
   SendTotalChecksum
END SendExe ;


(*
   SendSize - 
*)

PROCEDURE SendSize ;
BEGIN
   CreateSize(GetTextVirtualSize()+GetDataVirtualSize()) ;
   WriteLn ; WriteString('Total size of executable is: ') ; WriteCard(GetTextVirtualSize()+GetDataVirtualSize(), 8) ; WriteLn ;
   IF ExtractSize()#GetTextVirtualSize()+GetDataVirtualSize()
   THEN
      WriteString('checksum consistancy test failed') ; WriteLn ; HALT
   END ;
   SendFrame
END SendSize ;


(*
   SendTotalChecksum - 
*)

PROCEDURE SendTotalChecksum ;
VAR
   n : INTEGER ;
   in: SetOfFd ;
   t : Timeval ;
   ch: CHAR ;
BEGIN
   CreateSize(TotalChecksum) ;
   IF ExtractSize()#TotalChecksum
   THEN
      WriteString('total checksum consistancy test failed') ; WriteLn ; HALT
   END ;
   SendFrame ;
   t := InitTime(TimeoutSecs, 0) ;

   in := InitSet() ;
   FdZero(in) ;
   FdSet(unixIn, in) ;

   n := Select(MaxFdsPlusOne(unixIn, unixIn), in, NIL, NIL, t) ;
   WriteLn ;
   IF FdIsSet(unixIn, in)
   THEN
      ch := ReadCharRaw(unixIn) ;
      IF ch='p'
      THEN
         WriteString('checksum passed - all successfully loaded') ; WriteLn
      ELSIF ch='v'
      THEN
         WriteString('different crt0.S or Descriptor.S cannot execute program (overlay problem)') ; WriteLn
      ELSIF ch='f'
      THEN
         WriteString('checksum failed') ; WriteLn
      ELSIF ch='t'
      THEN
         WriteString('checksum packet failed to reach destination') ; WriteLn
      ELSE
         WriteString('unknown packet: ') ; Write(ch) ; WriteLn
      END
   ELSE
      WriteString('loader timed out - unsuccessful load') ; WriteLn
      (* timeout *)
   END ;
   t := KillTime(t) ;
   in := KillSet(in)
END SendTotalChecksum ;


(*
   SendHeader - 
*)

PROCEDURE SendHeader ;
VAR
   HeaderString: ARRAY [0..MaxHeader] OF CHAR ;
   i           : CARDINAL ;
   n           : INTEGER ;
   in          : SetOfFd ;
   t           : Timeval ;
   ch          : CHAR ;
BEGIN
   StrCopy(HeaderSequence, HeaderString) ;
   i := 0 ;
   WriteString('waiting for remote boot loader to respond...') ;
   ch := '-' ;
   REPEAT
      i := 0 ;
      WHILE HeaderString[i]#nul DO
         WriteCharRaw(unixOut, HeaderString[i]) ;
         INC(i)
      END ;
      t := InitTime(TimeoutSecs, 0) ;

      in := InitSet() ;
      FdZero(in) ;
      FdSet(unixIn, in) ;

      n := Select(MaxFdsPlusOne(unixIn, unixIn), in, NIL, NIL, t) ;
      IF FdIsSet(unixIn, in)
      THEN
         ch := ReadCharRaw(unixIn) ;
      ELSE
         Write('.')
         (* timeout *)
      END ;
      t := KillTime(t) ;
      in := KillSet(in) ;
   UNTIL ch='+' ;
   WriteString(' accepted') ; WriteLn
END SendHeader ;


(*
   CreateCheckSum - 
*)

PROCEDURE CreateCheckSum ;
VAR
   c, i: CARDINAL ;
BEGIN
   c := 0 ;
   i := 0 ;
   WHILE (i<=MaxFrame) AND (Frame[i]#'#') DO
      INC(c, VAL(CARDINAL, Frame[i])) ;
      INC(i)
   END ;
   IF (Frame[i]='#') AND (i<MaxFrame)
   THEN
      INC(i) ;
      IF i<MaxFrame
      THEN
         Frame[i]   := ToChar(c MOD 010H) ;
         Frame[i+1] := ToChar(c DIV 010H)
      ELSE
         WriteString('frame is too large..') ; WriteLn ; HALT
      END
   ELSE
      WriteString('frame is too large..') ; WriteLn ; HALT
   END
END CreateCheckSum ;


(*
   SendFrame - sends the current frame
*)

PROCEDURE SendFrame ;
VAR
   i : INTEGER ;
   n : INTEGER ;
   in: SetOfFd ;
   t : Timeval ;
   ch: CHAR ;
BEGIN
   Frame[0] := ToChar(SeqNo) ;
   CreateCheckSum ;
   REPEAT
      WriteCharRaw(unixOut, '$') ; (* start of frame *)
      i := 0 ;
      WHILE Frame[i]#'#' DO
         (* WriteCharRaw(unixOut, Frame[i]) ; *)
         INC(i)
      END ;
      INC(i, 3) ;  (* '#' 'c1' 'c2' *)
      IF write(unixOut, ADR(Frame), i)#i
      THEN
         WriteString('frame cannot be sent') ; WriteLn ; HALT
      END ;
      t := InitTime(TimeoutSecs, 0) ;

      in := InitSet() ;
      FdZero(in) ;
      FdSet(unixIn, in) ;

      n := Select(MaxFdsPlusOne(unixIn, unixIn), in, NIL, NIL, t) ;
      IF FdIsSet(unixIn, in)
      THEN
         ch := ReadCharRaw(unixIn) ;
         IF ch#ToChar(SeqNo)
         THEN
            Write('e') ; Write('[') ; Write(ch) ; Write(']')
         END
      ELSE
         Write('.') ;
         ch := '.' ;
         (* timeout *)
      END ;
      t := KillTime(t) ;
      in := KillSet(in)
   UNTIL ch=ToChar(SeqNo) ;
   SeqNo := 1-SeqNo
END SendFrame ;


(*
   CreateSize - fills the size of the program to be downloaded into the frame.
*)

PROCEDURE CreateSize (Size: CARDINAL) ;
BEGIN
   Frame[1] := ToChar(Size DIV 0000001H) ;
   Frame[2] := ToChar(Size DIV 0000010H) ;
   Frame[3] := ToChar(Size DIV 0000100H) ;
   Frame[4] := ToChar(Size DIV 0001000H) ;
   Frame[5] := ToChar(Size DIV 0010000H) ;
   Frame[6] := ToChar(Size DIV 0100000H) ;
   Frame[7] := '#'
END CreateSize ;


(*
   ExtractSize - returns the size of the program to be downloaded.
*)

PROCEDURE ExtractSize () : CARDINAL ;
BEGIN
   RETURN( 
          ToNum(Frame[1]) * 0000001H +
          ToNum(Frame[2]) * 0000010H +
          ToNum(Frame[3]) * 0000100H +
          ToNum(Frame[4]) * 0001000H +
          ToNum(Frame[5]) * 0010000H +
          ToNum(Frame[6]) * 0100000H
         )
END ExtractSize ;


(*
   ToNum - 
*)

PROCEDURE ToNum (ch: CHAR) : CARDINAL ;
BEGIN
   RETURN( ORD(ch-'A') MOD 010H )
END ToNum ;


(*
   ToChar - 
*)

PROCEDURE ToChar (c: CARDINAL) : CHAR ;
BEGIN
   RETURN( VAL(CHAR, c MOD 010H)+'A' )
END ToChar ;


(*
   AddToTotalChecksum - 
*)

PROCEDURE AddToTotalChecksum (b: BYTE) ;
BEGIN
   TotalChecksum := (TotalChecksum + VAL(CARDINAL, b)) MOD 0100000H ;
   INC(TotalBytes)
END AddToTotalChecksum ;


(*
   SendText - scans the input file for the text segment and
              writes the text to the output file.
*)

PROCEDURE SendText ;
VAR
   VirtSize,
   PhysSize: CARDINAL ;
   i, f, c : CARDINAL ;
   b       : BYTE ;
   ok      : BOOLEAN ;
BEGIN
   PhysSize := GetTextPhysicalSize() ;
   ok := TRUE ;
   i := 0 ;
   c := 0 ;
   f := 1 ;
   WriteString('Text') ; WriteLn ;
   WHILE (i<PhysSize) AND ok DO
      ok := GetByteFromText(b) ;
      IF ok
      THEN
         AddToTotalChecksum(b) ;
         IF f>=FrameDataLimit-1
         THEN
            Frame[f] := '#' ;
            SendFrame ;
            IF c=FramePulse
            THEN
               c := 0 ;
               Write('#')
            ELSE
               INC(c)
            END ;
            f := 1 ;
         END ;
         Frame[f] := ToChar(VAL(CARDINAL, b) MOD 010H) ;
         INC(f) ;
         Frame[f] := ToChar(VAL(CARDINAL, b) DIV 010H) ;
         INC(f) ;
         INC(i)
      END
   END ;
   VirtSize := GetTextVirtualSize() ;
   b := 0 ;
   c := 0 ;
   WHILE (i<VirtSize) AND ok DO
      ok := GetByteFromText(b) ;
      IF ok
      THEN
         AddToTotalChecksum(b) ;
         IF f>=FrameDataLimit-1
         THEN
            Frame[f] := '#' ;
            SendFrame ;
            IF c=FramePulse
            THEN
               c := 0 ;
               Write('#')
            ELSE
               INC(c)
            END ;
            f := 1 ;
         END ;
         Frame[f] := ToChar(VAL(CARDINAL, b) MOD 010H) ;
         INC(f) ;
         Frame[f] := ToChar(VAL(CARDINAL, b) DIV 010H) ;
         INC(f) ;
         INC(i)
      END
   END ;
   IF f>1
   THEN
      Frame[f] := '#' ;
      SendFrame
   END   
END SendText ;


(*
   SendData - scans the input file for the data segment and sends it down the serial link
*)

PROCEDURE SendData ;
VAR
   VirtSize,
   PhysSize: CARDINAL ;
   i, f, c : CARDINAL ;
   b       : BYTE ;
   ok      : BOOLEAN ;
BEGIN
   PhysSize := GetDataPhysicalSize() ;
   ok := TRUE ;
   i := 0 ;
   c := 0 ;
   f := 1 ;
   WriteLn ;
   WriteString('Data') ; WriteLn ;
   WHILE (i<PhysSize) AND ok DO
      ok := GetByteFromData(b) ;
      IF ok
      THEN
         AddToTotalChecksum(b) ;
         IF f>=FrameDataLimit-1
         THEN
            Frame[f] := '#' ;
            SendFrame ;
            IF c=FramePulse
            THEN
               c := 0 ;
               Write('#')
            ELSE
               INC(c)
            END ;
            f := 1 ;
         END ;
         Frame[f] := ToChar(VAL(CARDINAL, b) MOD 010H) ;
         INC(f) ;
         Frame[f] := ToChar(VAL(CARDINAL, b) DIV 010H) ;
         INC(f) ;
         INC(i)
      END
   END ;
   VirtSize := GetDataVirtualSize() ;
   b := 0 ;
   c := 0 ;
   WHILE (i<VirtSize) AND ok DO
      ok := GetByteFromData(b) ;
      IF ok
      THEN
         AddToTotalChecksum(b) ;
         IF f>=FrameDataLimit-1
         THEN
            Frame[f] := '#' ;
            SendFrame ;
            IF c=FramePulse
            THEN
               c := 0 ;
               Write('#')
            ELSE
               INC(c)
            END ;
            f := 1 ;
         END ;
         Frame[f] := ToChar(VAL(CARDINAL, b) MOD 010H) ;
         INC(f) ;
         Frame[f] := ToChar(VAL(CARDINAL, b) DIV 010H) ;
         INC(f) ;
         INC(i)
      END
   END ;
   IF f>1
   THEN
      Frame[f] := '#' ;
      SendFrame
   END   
END SendData ;


(*
   ContinueDebuggingRemote - displays any characters comming from the remote machine
                             after the program has been downloaded.
                             This allows us to see the output of the remote machine
                             on this console. Useful if you have to share a monitor
                             between two machines - as I'm doing now.
*)

PROCEDURE ContinueDebuggingRemote ;
BEGIN
   WriteString('remote machines debugging...') ; WriteLn ;
   LOOP
      Write(ReadCharRaw(unixIn))
   END
END ContinueDebuggingRemote ;


(*
   Init - 
*)

PROCEDURE Init ;
VAR
   i   : CARDINAL ;
   a, b: ARRAY [0..MaxString] OF CHAR ;
BEGIN
   i := 1 ;
   StrCopy(EXE, ExeFileName) ;
   StrCopy(DEVICE, DeviceName) ;
   WHILE GetArg(a, i) DO
      IF StrEqual(a, '-o')
      THEN
         INC(i) ;
         IF NOT GetArg(DeviceName, i)
         THEN
            WriteString('expecting device name after -o option') ; WriteLn ;
            exit(1)
         END
      ELSIF StrEqual(a, '-h')
      THEN
         WriteString('Usage: sboot [-o devicemame] [executable]') ; WriteLn ;
         WriteString('       default device    : ') ; WriteString(DEVICE) ; WriteLn ;
         WriteString('       default executable: ') ; WriteString(EXE) ; WriteLn ;
         exit(0)
      ELSE
         StrCopy(a, ExeFileName)
      END ;
      INC(i)
   END ;
   StrCopy('stty raw -icanon -inlcr -icrnl -echo cs8 -parenb -crtscts ixoff ', a) ;
   CardToStr(BaudRate, 0, b) ;
   StrConCat(a, b, a) ;
   StrConCat(a, ' < ', a) ;
   StrConCat(a, DeviceName, a) ;
   IF system(ADR(a))<0
   THEN
      WriteString('failed to execute: ') ; WriteString(a) ; WriteLn ;
      exit(1)
   END ;
(*
   StrCopy('stty raw -icanon -inlcr -icrnl -echo', a);
   IF system(ADR(a))<0
   THEN
      WriteString('failed to execute: ') ; WriteString(a) ; WriteLn ;
      exit(1)
   END ;
*)
   Download ;
   IF RemoteDebugging
   THEN
      ContinueDebuggingRemote
   END
END Init ;


BEGIN
   Init
END sboot.
