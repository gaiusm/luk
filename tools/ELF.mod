IMPLEMENTATION MODULE ELF ;

(* NOTE: linux elf structures defined in /usr/src/linux/include/linux/elf.h *)

FROM Debug IMPORT Halt ;
FROM SYSTEM IMPORT TSIZE, ADR ;
FROM Storage IMPORT ALLOCATE ;
FROM FIO IMPORT OpenForRandom, ReadNBytes, Close, ReadChar,
                SetPositionFromBeginning, FindPosition, Exists,
                WriteNBytes, OpenToWrite, FlushBuffer, StdOut ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, WriteHex ;

FROM WordSizes IMPORT SHORTWORD, LONGWORD, WriteLong, WriteShort,
                      LongWordToCardinal, ShortWordToCardinal,
                      CardinalToLongWord,
                      FlipLongWord, FlipShortWord ;
FROM libc IMPORT memset ;

CONST
   MaxElfIdent  =   16 ;
   Debugging    = TRUE ;

   (* elf machine constants *)
   EMNONE       =    0 ;
   EMM32        =    1 ;
   EMSPARC      =    2 ;
   EM386        =    3 ;
   EM68K        =    4 ;
   EM88K        =    5 ;
   EM486        =    6 ;
   EM860        =    7 ;

   (* elf type constants *)

   ETNONE       =    0 ;
   ETREL        =    1 ;
   ETEXEC       =    2 ;
   ETDYN        =    3 ;
   ETCORE       =    4 ;
   ETLOPROC     =    5 ;
   ETHIPROC     =    6 ;

   (* These constants are for the segment types stored in the image headers *)
   PTNULL       =    0 ;
   PTLOAD       =    1 ;
   PTDYNAMIC    =    2 ;
   PTINTERP     =    3 ;
   PTNOTE       =    4 ;
   PTSHLIB      =    5 ;
   PTPHDR       =    6 ;
   PTTLS        =    7 ;       (* Thread-local storage segment *)
   PTNUM        =    8 ;       (* Number of defined types *)
   PTLOOS       = 60000000H ;  (* Start of OS-specific *)
   PTGNUEHFRAME = 6474E550H ;  (* GCC .eh_frame_hdr segment *)
   PTGNUSTACK   = 6474E551H ;  (* Indicates stack executability *)
   PTGNURELRO   = 6474E552H ;  (* Read-only after relocation *)
   PTSUNWBSS    = 06FFFFFAH ;  (* Sun Specific segment *)
   PTSUNWSTACK  = 06FFFFFBH ;  (* Stack segment *)
   PTHISUNW     = 6FFFFFFFH ;
   PTHIOS       = 6FFFFFFFH ;  (* End of OS-specific *)
   PTLOPROC     = 70000000H ;  (* Start of processor-specific *)
   PTHIPROC     = 7FFFFFFFH ;  (* End of processor-specific *)

   (* PFLAG constants found in /usr/src/linux/include/linux/mmap.h *)
   PROTREAD     =    0 ;      (* page can be read         *)
   PROTWRITE    =    1 ;      (* page can be written      *)
   PROTEXEC     =    2 ;      (* page can be executed     *)
   PROTNONE     =    0 ;      (* page can not be accessed *)


TYPE
   ElfExec = RECORD
                ident    : ARRAY [0..MaxElfIdent-1] OF CHAR ;
                type     : SHORTWORD ;
                machine  : SHORTWORD ;
                version  : LONGWORD ;
                entry    : LONGWORD ;  (* entry point *)
                phoff    : LONGWORD ;
                shoff    : LONGWORD ;
                flags    : LONGWORD ;
                ehsize   : SHORTWORD ;
                phentsize: SHORTWORD ;
                phnum    : SHORTWORD ;
                shentsize: SHORTWORD ;
                shnum    : SHORTWORD ;
                shstrndx : SHORTWORD ;
             END ;

   ELFPhdr = RECORD
                ptype  : LONGWORD ;
                poffset: LONGWORD ;
                pvaddr : LONGWORD ;
                ppaddr : LONGWORD ;
                pfilesz: LONGWORD ;
                pmemsz : LONGWORD ;
                pflags : LONGWORD ;
                palign : LONGWORD ;
             END ;

   PtrToPHeader = POINTER TO ELFPhdr ;

VAR
   x              : ElfExec ;   (* the current ELF executable header *)
   StartOfCode,
   EndOfCode,
   TextLoadAddress,
   DataLoadAddress,
   StartOfBss,
   EndOfBss,
   StartOfData,
   EndOfData      : CARDINAL ;


(*
   Max -
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   ExamineELF -
*)

PROCEDURE ExamineELF (f: File; c: ConfirmElf; t, d, b: InitStateProc) : BOOLEAN ;
BEGIN
   SetPositionFromBeginning(f, 0) ;
   IF ReadNBytes(f, TSIZE(ElfExec), ADR(x)) # TSIZE(ElfExec)
   THEN
      Halt('failed to read the elf header',
           __FILE__, __FUNCTION__, __LINE__)
   END ;

   IF Debugging
   THEN
      WriteLn ;
      WriteString('type        ') ; WriteHex(ShortWordToCardinal(x.type), 2) ; WriteLn ;
      WriteString('machine     ') ; WriteHex(ShortWordToCardinal(x.machine), 2) ; WriteLn ;
      WriteString('version     ') ; WriteHex(LongWordToCardinal(x.version), 2) ; WriteLn ;
      WriteString('entry (pc)  ') ; WriteLong(x.entry) ; WriteLn ;
      WriteString('phoff       ') ; WriteLong(x.phoff) ; WriteLn ;
      WriteString('shoff       ') ; WriteLong(x.shoff) ; WriteLn ;
      WriteString('flags       ') ; WriteLong(x.flags) ; WriteLn ;
      WriteString('ehsize      ') ; WriteShort(x.ehsize) ; WriteLn ;
      WriteString('phentsize   ') ; WriteShort(x.phentsize) ; WriteLn ;
      WriteString('phnum       ') ; WriteShort(x.phnum) ; WriteLn ;
      WriteString('shentsize   ') ; WriteShort(x.shentsize) ; WriteLn ;
      WriteString('shnum       ') ; WriteShort(x.shnum) ; WriteLn ;
      WriteString('shstrndx    ') ; WriteShort(x.shstrndx) ; WriteLn ;
      FlushBuffer(StdOut)
   END ;

   WriteElfType(ShortWordToCardinal(x.type)) ;
   WriteElfMachine(ShortWordToCardinal(x.machine)) ;
   IF (LongWordToCardinal(x.entry)#0) AND (LongWordToCardinal(x.entry)#1024*1024)
   THEN
      WriteString(' Expecting entry to be zero or at 1MB') ; WriteLn
   END ;
   IF ShortWordToCardinal(x.type)=ETEXEC
   THEN
      c('elf executable')
   ELSE
      c('elf file') ;
      WriteString(' warning expecting an executable file') ; WriteLn
   END ;
   IF (ShortWordToCardinal(x.machine)#EM386) AND (ShortWordToCardinal(x.machine)#EM486)
   THEN
      WriteString(' warning expecting i386 or i486') ; WriteLn ;
      RETURN( FALSE )
   END ;
   FlushBuffer(StdOut) ;
   ReadRestOfHeaderInfo(f) ;
   IF StartOfData=StartOfCode
   THEN
      t(TextLoadAddress, EndOfCode-StartOfCode, EndOfCode-StartOfCode, StartOfCode, StartOfCode) ;
      d(DataLoadAddress, EndOfData-EndOfCode, EndOfData-EndOfCode, EndOfCode, EndOfCode) ;
      b(0, EndOfBss-EndOfData, 0, EndOfData, EndOfData)
   ELSE
      t(TextLoadAddress, StartOfData-StartOfCode, EndOfCode-StartOfCode, StartOfCode, StartOfCode) ;
      d(DataLoadAddress, EndOfData-StartOfData, EndOfData-StartOfData, StartOfData, StartOfData) ;
      b(0, EndOfBss-EndOfData, 0, EndOfData, EndOfData)
   END ;
   RETURN( TRUE )
END ExamineELF ;


(*
   ReadRestOfHeaderInfo - reads the header info.
*)

PROCEDURE ReadRestOfHeaderInfo (f: File) ;
VAR
   p             : PtrToPHeader ;
   LengthOfHeader: CARDINAL ;
BEGIN
   LengthOfHeader := ShortWordToCardinal(x.phentsize)*ShortWordToCardinal(x.phnum) ;
   IF Debugging
   THEN
      WriteString('phentsize = ') ; WriteShort(x.phentsize) ; WriteLn ;
      WriteString('phnum     = ') ; WriteShort(x.phnum) ; WriteLn ;
      WriteString('phnum * phentsize = ') ;
      WriteLong(CardinalToLongWord(LengthOfHeader)) ; WriteLn ;
      FlushBuffer(StdOut)
   END ;
   ALLOCATE(p, LengthOfHeader) ;
   IF memset(p, 255, LengthOfHeader)=NIL
   THEN
   END ;
   SetPositionFromBeginning(f, LongWordToCardinal(x.phoff)) ;
   IF ReadNBytes(f, LengthOfHeader, p) # LengthOfHeader
   THEN
      Halt('failed to read the length of elf header',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   DecypherPHeader(p, LengthOfHeader)
END ReadRestOfHeaderInfo ;


(*
   DecypherPHeader -
*)

PROCEDURE DecypherPHeader (p: PtrToPHeader; LengthOfHeader: CARDINAL) ;
VAR
   b       : BITSET ;
   len,
   pmemsz,
   pfilesz,
   pvaddr,
   n, i, j : CARDINAL ;
BEGIN
   len := 0 ;
   i := 0 ;
   n := ShortWordToCardinal(x.phnum) ;
   StartOfCode     := 0 ;
   EndOfCode       := 0 ;
   TextLoadAddress := 0 ;
   DataLoadAddress := 0 ;
   StartOfBss      := 0 ;
   EndOfBss        := 0 ;
   StartOfData     := 0 ;
   EndOfData       := 0 ;
   WHILE (i<n) AND (len<LengthOfHeader) DO
      WritePHeader(p) ;
      CASE LongWordToCardinal(p^.ptype) OF

      PTNULL:  |
      PTLOAD:  (* physical size vs. virtual size allows us to work out the BSS *)
               TextLoadAddress := LongWordToCardinal(p^.poffset) ;
               DataLoadAddress := LongWordToCardinal(p^.poffset) ;
               pmemsz := LongWordToCardinal(p^.pmemsz) ;
               pfilesz := LongWordToCardinal(p^.pfilesz) ;
               pvaddr := LongWordToCardinal(p^.pvaddr) ;
               StartOfBss := pfilesz ;
               EndOfBss := Max(pfilesz, pmemsz) ;
               EndOfData := pfilesz

      ELSE
      END ;
      IF Debugging
      THEN
         WriteString('code = ') ; WriteHex(StartOfCode, 8) ; WriteString('..') ;
         WriteHex(EndOfCode, 8) ; WriteCard(EndOfCode-StartOfCode, 8) ; WriteLn ;
         WriteString('data = ') ; WriteHex(StartOfData, 8) ; WriteString('..') ;
         WriteHex(EndOfData, 8) ; WriteCard(EndOfData-StartOfData, 8) ; WriteLn ;
         WriteString('bss  = ') ; WriteHex(EndOfData  , 8) ; WriteString('..') ;
         WriteHex(EndOfBss , 8) ; WriteCard(EndOfBss-EndOfData   , 8) ; WriteLn ;
         FlushBuffer(StdOut)
      END ;
      INC(i) ;
      INC(p, TSIZE(ELFPhdr)) ;
      INC(len, TSIZE(ELFPhdr))
   END
END DecypherPHeader ;


(*
   WritePHeader - writes the PHeader information.
*)

PROCEDURE WritePHeader (p: PtrToPHeader) ;
BEGIN
   IF Debugging
   THEN
      WITH p^ DO
         WriteString('ptype  : ') ; WritePType(ptype) ; WriteLn ;
         WriteString('poffset: ') ; WriteLong(poffset) ; WriteLn ;
         WriteString('pvaddr : ') ; WriteLong(pvaddr) ; WriteLn ;
         WriteString('ppaddr : ') ; WriteLong(ppaddr) ; WriteLn ;
         WriteString('pfilesz: ') ; WriteLong(pfilesz) ; WriteLn ;
         WriteString('pmemsz : ') ; WriteLong(pmemsz) ; WriteLn ;
         WriteString('pflags : ') ; WritePFlags(pflags) ; WriteLn ;
         WriteString('palign : ') ; WriteLong(palign) ; WriteLn ;
         FlushBuffer(StdOut)
      END
   END
END WritePHeader ;


(*
   WritePFlags - writes a textual description of the pflags field.
*)

PROCEDURE WritePFlags (f: LONGWORD) ;
VAR
   c: CARDINAL ;
   b: BITSET ;
BEGIN
   c := LongWordToCardinal(f) ;
   IF c=PROTNONE
   THEN
      WriteString('---')
   ELSE
      b := BITSET(c) ;
      IF PROTREAD IN b
      THEN
         WriteString('r')
      ELSE
         WriteString('-')
      END ;
      IF PROTWRITE IN b
      THEN
         WriteString('w')
      ELSE
         WriteString('-')
      END ;
      IF PROTEXEC IN b
      THEN
         WriteString('x')
      ELSE
         WriteString('-')
      END
   END
END WritePFlags ;


(*
   WritePType - writes a textual description of ptype.
*)

PROCEDURE WritePType (p: LONGWORD) ;
BEGIN
   CASE LongWordToCardinal(p) OF

   PTNULL       :  WriteString('ptnull') |
   PTLOAD       :  WriteString('ptload') |
   PTDYNAMIC    :  WriteString('ptdynamic') |
   PTINTERP     :  WriteString('ptinerp') |
   PTNOTE       :  WriteString('ptnote') |
   PTSHLIB      :  WriteString('ptshlib') |
   PTTLS        :  WriteString('pttls') |
   PTNUM        :  WriteString('ptnum') |
   PTLOOS       :  WriteString('ptloos') |
   PTGNUEHFRAME :  WriteString('gnuehframe') |
   PTGNUSTACK   :  WriteString('gnustack') |
   PTGNURELRO   :  WriteString('gnurelocation') |
   PTSUNWBSS    :  WriteString('sunwbss') |
   PTSUNWSTACK  :  WriteString('sunstack') |
   PTHIOS       :  WriteString('endofosspecific') |
   PTLOPROC     :  WriteString('start proc specific') |
   PTHIPROC     :  WriteString('end proc specific')

   ELSE
      WriteString('unknown ') ; WriteLong(p) ;
      FlushBuffer(StdOut) ;
      Halt('fatal error: ptype is unknown',
           __FILE__, __FUNCTION__, __LINE__)
   END
END WritePType ;


(*
   WriteElfType -
*)

PROCEDURE WriteElfType (t: CARDINAL) ;
BEGIN
   IF Debugging
   THEN
      WriteString('type: ') ;
      CASE t OF

      ETNONE  :   WriteString('none') |
      ETREL   :   WriteString('relative') |
      ETEXEC  :   WriteString('executable') |
      ETDYN   :   WriteString('dynamic') |
      ETCORE  :   WriteString('core dump') |
      ETLOPROC:   WriteString('lo proc') |
      ETHIPROC:   WriteString('hi proc')

      ELSE
         WriteString(' unknown type (error)')
      END ;
      WriteLn
   END
END WriteElfType ;


(*
   WriteElfMachine -
*)

PROCEDURE WriteElfMachine (m: CARDINAL) ;
BEGIN
   IF Debugging
   THEN
      WriteString('machine: ') ;
      CASE m OF

      EMNONE :  WriteString('none') |
      EMM32  :  WriteString('32 bit') |
      EMSPARC:  WriteString('sparc') |
      EM386  :  WriteString('386') |
      EM68K  :  WriteString('68k') |
      EM88K  :  WriteString('88k') |
      EM486  :  WriteString('486') |
      EM860  :  WriteString('i860')

      ELSE
         WriteString(' unknown machine (error)')
      END ;
      WriteLn
   END
END WriteElfMachine ;


END ELF.
