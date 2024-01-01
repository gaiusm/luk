MODULE od ;

FROM Args IMPORT GetArg, Narg ;
FROM NumberIO IMPORT WriteCard, WriteHex, StrToCard ;
FROM StrLib IMPORT StrEqual, StrCopy ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM FIO IMPORT OpenToRead, File, Close, ReadChar, Exists,
                IsNoError, EOF ;


(*
   ScanFile - scans a file specified by fname.
*)

PROCEDURE ScanFile (fname: ARRAY OF CHAR) ;
VAR
   f: File ;
   i: CARDINAL ;
   ch: CHAR ;
BEGIN
   WriteString('File: ') ; WriteString(fname) ;
   IF Exists(fname)
   THEN
      f := OpenToRead(fname) ;
      IF IsNoError(f)
      THEN
      	 i := 0 ;
      	 WHILE NOT EOF(f) DO
      	    ch := ReadChar(f) ;
      	    IF (i MOD 16) = 0
      	    THEN
      	       WriteLn ;
      	       WriteHex(i, 4) ; WriteString('    ')
      	    END ;
      	    Write(' ') ; WriteHex(VAL(CARDINAL, ch), 2) ;
      	    INC(i)
      	 END ;
      	 Close(f) ;
      	 WriteLn
      ELSE
      	 WriteString('error while trying to open ') ; WriteString(fname) ; WriteLn
      END
   ELSE
      WriteString('cannot open ') ; WriteString(fname) ; WriteLn
   END
END ScanFile ;


(*
   Init - foreach argument print the hex of the file.
*)

PROCEDURE Init ;
VAR
   a: ARRAY [0..200] OF CHAR ;
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE GetArg(a, i) DO
      ScanFile(a) ;
      INC(i)
   END
END Init ;


BEGIN
   Init
END od.
