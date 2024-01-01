IMPLEMENTATION MODULE AOUT ;


FROM SYSTEM IMPORT ADR, TSIZE ;
FROM FIO IMPORT OpenForRandom, File, ReadNBytes, Close, ReadChar,
                SetPositionFromBeginning, FindPosition, Exists,
                WriteNBytes, OpenToWrite ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, WriteHex ;
FROM WordSizes IMPORT SHORTWORD, LONGWORD, WriteLong, WriteShort,
                      LongWordToCardinal, ShortWordToCardinal ;
FROM ELF IMPORT ExamineELF ;
FROM Args IMPORT GetArg ;
FROM Debug IMPORT Halt ;
FROM MemUtils IMPORT MemZero ;


CONST
   NMagicOffset = 1024 ;
   ZMagicOffset = 4096 ;

   OMAGIC    = 0407B ;  (* code indicating object file or impure exe *)
   NMAGIC    = 0410B ;  (* code indicating pure exe                  *)
   ZMAGIC    = 0413B ;  (* code indicating demand paged exe          *)
   QMAGIC    = 0314B ;  (* demand paged exe, header in text, with    *)
                        (* first page unmapped to help with *NULL    *)
   CMAGIC    = 0421B ;  (* core file                                 *)
   IMAGIC    = 086A3H ; (* i8086 executable, bde generated           *)
   BSDAOUT   = 00301H ; (* BSD a.out.h being used.                   *)
   SMPAOUT   = 00B18H ; (* SMP a.out executable                      *)
   COFFMAGIC = 0014CH ; (* coff i386 magic executable                *)
   ELFMAGIC  = 0457FH ; (* elf magic executable                      *)

   Debug  = TRUE ;

   MSUN2  =    0 ;
   M68010 =    1 ;
   M68020 =    2 ;
   MSPARC =    3 ;
   M386   =  100 ;

   BytesPerSeg = 512 ;

TYPE
   GnuExec = RECORD
                magic      : SHORTWORD ;  (* contains magic number   *)
                type       : BYTE ;
                flags      : BYTE ;
                text       : LONGWORD ;   (* length of text in bytes *)
                data       : LONGWORD ;   (* length of data in bytes *)
                bss        : LONGWORD ;   (* length of bss in bytes  *)
                syms       : LONGWORD ;   (* length of syms in bytes *)
                entry      : LONGWORD ;   (* start address           *)
                textrelsize: LONGWORD ;   (* text reloc size         *)
                datarelsize: LONGWORD ;   (* data reloc size         *)
             END ;

   BsdExec = RECORD
                magic      : SHORTWORD ;  (* contains magic number   *)
                flags      : BYTE ;       (* flags, see below        *)
                cpu        : BYTE ;       (* cpu id                  *)
                hdrlen     : BYTE ;       (* length of header        *)
                unused     : BYTE ;       (* reserved for future use *)
                version    : SHORTWORD ;  (* version stamp (not used at present) *)
                text       : LONGWORD ;   (* size of text segement in bytes *)
                data       : LONGWORD ;   (* size of data segment in bytes *)
                bss        : LONGWORD ;   (* size of bss segment in bytes *)
                entry      : LONGWORD ;   (* entry point *)
                total      : LONGWORD ;   (* total memory allocated *)
                syms       : LONGWORD ;   (* size of symbol table *)
             END ;
   (* SHORT FORM ENDS HERE *)

   BsdRelocation = RECORD
                      trsize: LONGWORD ;  (* text relocation size *)
                      drsize: LONGWORD ;  (* data relocation size *)
                      tbase : LONGWORD ;  (* text relocation base *)
                      dbase : LONGWORD ;  (* data relocation base *)
                   END ;

   MinixObject = RECORD
                    Magic       : SHORTWORD ;    (* BDE as object magic           *)
                    NoOfModules : SHORTWORD ;    (* number of modules             *)
                    Checksum    : BYTE ;         (* checksum for previous 4 bytes *)
                    StartOfText : LONGWORD ;     (* offset to start of text       *)
                    TextSize    : LONGWORD ;     (* text size in bytes            *)
                    StringLength: SHORTWORD ;    (* length of string area         *)
                    Class       : BYTE ;
                    Revision    : BYTE ;
                 END ;

   HeaderFormat = (BsdExe, BsdObj, MinixObj, GnuExe, GnuObj, ElfObj) ; (* different kinds *)
                                                                       (* of object file  *)

CONST
(* BSD  CPU Id of TARGET machine (byte order coded in low order two bits) *)
   BsdNONE  = 000H ;       (* unknown *)
   BsdI8086 = 004H ;       (* intel i8086/8088 *)
   BsdM68K  = 00BH ;       (* motorola m68000 *)
   BsdNS16K = 00CH ;       (* national semiconductor 16032 *)
   BsdI80386= 010H ;       (* intel i80386 *)
   BsdSPARC = 017H ;       (* Sun SPARC *)


TYPE
   State = RECORD
              OffsetIntoFile,             (* where segment starts in file         *)
              VirtualLength,              (* total bytes of this segment          *)
              PhysicalLength,             (* total bytes of this segment in file  *)
              Index,                      (* current position through segment     *)
              VirtualStart,               (* Virtual address of the segment start *)
              PhysicalStart: CARDINAL ;   (* Physical address of the segment      *)
           END ;

   Segment = (None, Text, Data, Bss) ;

VAR
   fi            : File ;                  (* input file  *)
   TextState,
   DataState,
   BssState      : State ;
   KindOfFile    : HeaderFormat ;
   BytePosition  : CARDINAL ;
   CurrentSegment: Segment ;
   DebugFlag,
   Is16Bit       : BOOLEAN ;


(*
   Examine - opens a binary object file. It returns true if
             successful and false if not.
*)

PROCEDURE Examine (File: ARRAY OF CHAR) : BOOLEAN ;
VAR
   x: SHORTWORD ;
BEGIN
   IF Exists(File)
   THEN
      DebugFlag := FALSE ;
      BytePosition := 0 ;
      Is16Bit := FALSE ;
      CurrentSegment := None ;
      fi := OpenForRandom(File, FALSE, FALSE) ;
      IF ReadNBytes( fi, TSIZE(SHORTWORD), ADR(x) ) # TSIZE(SHORTWORD)
      THEN
         Halt('failed to read magic word',
              __FILE__, __FUNCTION__, __LINE__)
      END ;
      SetPositionFromBeginning(fi, 0) ;
      CASE ShortWordToCardinal(x) OF

      BSDAOUT  : RETURN( ExamineBSD(x) ) |
      IMAGIC   : RETURN( ExamineMinix(x) ) |
      ZMAGIC   : RETURN( ExamineZGNU(x) ) |
      QMAGIC   : RETURN( ExamineQGNU(x) ) |
      COFFMAGIC: WriteString('AOUT: coff i386 found - but not understood') ; WriteLn |
      ELFMAGIC : RETURN( ExamineELF(fi, SetElfFile, InitText, InitData, InitBss) )

      ELSE
         WriteString('AOUT: does not understand this type of file') ; WriteLn ;
         RETURN( FALSE )
      END ;
      RETURN( FALSE )
   ELSE
      RETURN( FALSE )
   END
END Examine ;


(*
   SetKindOfFile - sets the variable KindOfFile and write string, a,
                   if Debug is true.
*)

PROCEDURE SetKindOfFile (a: ARRAY OF CHAR; t: HeaderFormat) ;
BEGIN
   IF Debug
   THEN
      WriteString(a)
   END ;
   KindOfFile := t
END SetKindOfFile ;


(*
   SetElfFile - sets the variable KindOfFile to Elf and write string, a,
                if Debug is true.
*)

PROCEDURE SetElfFile (a: ARRAY OF CHAR) ;
BEGIN
   SetKindOfFile(a, ElfObj)
END SetElfFile ;


(*
   ExamineBSD -
*)

PROCEDURE ExamineBSD (m: SHORTWORD) : BOOLEAN ;
VAR
   x: BsdExec ;
   r: BsdRelocation ;
BEGIN
   IF ReadNBytes( fi, TSIZE(BsdExec), ADR(x) ) # TSIZE(BsdExec)
   THEN
      Halt('failed to read bsd header',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   IF Debug
   THEN
      WriteString('BSD Magic:')
   END ;
   CASE ShortWordToCardinal(x.magic) OF

   OMAGIC:  SetKindOfFile('linux object file or impure exe', BsdObj) |
   NMAGIC:  SetKindOfFile('linux pure exe', BsdExe) |
   ZMAGIC:  SetKindOfFile('linux demand paged exe', BsdExe) |
   QMAGIC:  SetKindOfFile('linux demand paged exe unmapped zero page', BsdExe) |
   IMAGIC:  SetKindOfFile('bde as object code', BsdObj) |
   BSDAOUT: SetKindOfFile('BSD as object code', BsdObj) |
   SMPAOUT: SetKindOfFile('SMP as object code', BsdObj)

   ELSE
      WriteString('   dont understand about this file type') ;
      WriteLn
   END ;
   IF Debug
   THEN
      WriteLn ;
      WriteString('cpu         ') ;
      CASE VAL(CARDINAL, x.cpu) OF

      BsdNONE  :  WriteString('unknown') |
      BsdI8086 :  WriteString('i8086') |
      BsdM68K  :  WriteString('mc68000') |
      BsdNS16K :  WriteString('ns16032') |
      BsdI80386:  WriteString('i80386') |
      BsdSPARC :  WriteString('sparc')

      ELSE
         WriteString('    unknown processor type')
      END ;
      WriteLn ;
      WriteString('flags       ') ; WriteHex(VAL(CARDINAL, x.flags), 2) ; WriteLn ;
      WriteString('cpu         ') ; WriteHex(VAL(CARDINAL, x.cpu), 2) ; WriteLn ;
      WriteString('hdrlen      ') ; WriteHex(VAL(CARDINAL, x.hdrlen), 2) ; WriteLn ;
      WriteString('text length ') ; WriteLong(x.text) ; WriteLn ;
      WriteString('data length ') ; WriteLong(x.data) ; WriteLn ;
      WriteString('bss  length ') ; WriteLong(x.bss) ; WriteLn ;
      WriteString('entry (pc)  ') ; WriteLong(x.entry) ; WriteLn ;
      WriteString('total       ') ; WriteLong(x.total) ; WriteLn ;
      WriteString('syms        ') ; WriteLong(x.syms) ; WriteLn ;
   END ;
   Is16Bit := VAL(CARDINAL, x.cpu)=BsdI8086 ;
   IF x.hdrlen=BYTE(030H)
   THEN
      IF ReadNBytes( fi, TSIZE(BsdRelocation), ADR(r) ) # TSIZE(BsdRelocation)
      THEN
         Halt('failed to read bsd relocation',
              __FILE__, __FUNCTION__, __LINE__)
      END ;

      IF Debug
      THEN
         WITH r DO
            WriteString('text reloc ') ; WriteLong(trsize) ; WriteLn ;
            WriteString('data reloc ') ; WriteLong(drsize) ; WriteLn ;
            WriteString('text base  ') ; WriteLong(tbase) ; WriteLn ;
            WriteString('data base  ') ; WriteLong(dbase) ; WriteLn
         END
      END ;
      WriteString('need to complete this code') ; WriteLn ; HALT
   ELSIF x.hdrlen=BYTE(020H)
   THEN
      InitState(TextState, VAL(CARDINAL, x.hdrlen),
                LongWordToCardinal(x.text), LongWordToCardinal(x.text), 0, 0) ;
      InitState(DataState, VAL(CARDINAL, x.hdrlen)+LongWordToCardinal(x.text),
                LongWordToCardinal(x.data), LongWordToCardinal(x.data), 0, 0) ;
   ELSE
      WriteString('error - expecting header length to be 030H or 020H') ; WriteLn ;
      HALT
   END ;
   RETURN( TRUE )
END ExamineBSD ;


(*
   ExamineZGNU -
*)

PROCEDURE ExamineZGNU (m: SHORTWORD) : BOOLEAN ;
VAR
   x: GnuExec ;
BEGIN
   IF ReadNBytes( fi, TSIZE(GnuExec), ADR(x) ) # TSIZE(GnuExec)
   THEN
      Halt('failed to read gnu exec header',
           __FILE__, __FUNCTION__, __LINE__)
   END ;

   IF Debug
   THEN
      WriteString('GNU Magic:')
   END ;
   CASE ShortWordToCardinal(x.magic) OF

   OMAGIC:  SetKindOfFile('linux object file or impure exe', GnuObj) |
   NMAGIC:  SetKindOfFile('linux pure exe', GnuExe) |
   ZMAGIC:  SetKindOfFile('linux demand paged exe', GnuExe) |
   QMAGIC:  SetKindOfFile('linux demand paged exe unmapped zero page', GnuExe)

   ELSE
      WriteString('   dont understand about this file type') ;
      WriteLn
   END ;
   IF Debug
   THEN
      WriteString('type        ') ;
      CASE VAL(CARDINAL, x.type) OF

      MSUN2 :  WriteString('sun 2') |
      M68010:  WriteString('mc68010') |
      M68020:  WriteString('mc68020') |
      MSPARC:  WriteString('sparc') |
      M386  :  WriteString('i386')

      ELSE
         WriteString('    unknown processor type')
      END ;
      WriteLn ;
      WriteString('flags       ') ; WriteHex(VAL(CARDINAL, x.flags), 2) ; WriteLn ;
      WriteString('type        ') ; WriteHex(VAL(CARDINAL, x.type), 2) ; WriteLn ;
      WriteString('text length ') ; WriteLong(x.text) ; WriteLn ;
      WriteString('bss  length ') ; WriteLong(x.bss) ; WriteLn ;
      WriteString('data length ') ; WriteLong(x.data) ; WriteLn ;
      WriteString('entry (pc)  ') ; WriteLong(x.entry) ; WriteLn ;
      WriteString('syms        ') ; WriteLong(x.syms) ; WriteLn ;
      WriteString('text rel sz ') ; WriteLong(x.textrelsize) ; WriteLn ;
      WriteString('data rel sz ') ; WriteLong(x.datarelsize) ; WriteLn
   END ;
   (* work out the start of the code within the executable file *)
   CASE ShortWordToCardinal(x.magic) OF

   ZMAGIC:  InitState(TextState, ZMagicOffset,
                      LongWordToCardinal(x.text), LongWordToCardinal(x.text), 0, 0) ;
            InitState(DataState, ZMagicOffset+LongWordToCardinal(x.text),
                      LongWordToCardinal(x.data), LongWordToCardinal(x.data), 0, 0) ;
            InitState(BssState, 0,
                      LongWordToCardinal(x.bss), LongWordToCardinal(x.bss), 0, 0)

   ELSE
      InitState(TextState, NMagicOffset,
                LongWordToCardinal(x.text), LongWordToCardinal(x.text), 0, 0) ;
      InitState(DataState, NMagicOffset+LongWordToCardinal(x.text),
                LongWordToCardinal(x.data), LongWordToCardinal(x.data), 0, 0) ;
      InitState(BssState, 0,
                LongWordToCardinal(x.bss), LongWordToCardinal(x.bss), 0, 0)

   END ;
   IF LongWordToCardinal(x.entry)#0
   THEN
      Halt('expecting entry to be zero',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   RETURN( TRUE )
END ExamineZGNU ;


(*
   ExamineQGNU -
*)

PROCEDURE ExamineQGNU (m: SHORTWORD) : BOOLEAN ;
VAR
   x: GnuExec ;
BEGIN
   IF ReadNBytes( fi, TSIZE(GnuExec), ADR(x) ) # TSIZE(GnuExec)
   THEN
      Halt('failed to read qgnu header',
           __FILE__, __FUNCTION__, __LINE__)
   END ;

   IF Debug
   THEN
      WriteString('GNU Magic:')
   END ;
   CASE ShortWordToCardinal(x.magic) OF

   OMAGIC:  SetKindOfFile('linux object file or impure exe', GnuObj) |
   NMAGIC:  SetKindOfFile('linux pure exe', GnuExe) |
   ZMAGIC:  SetKindOfFile('linux demand paged exe', GnuExe) |
   QMAGIC:  SetKindOfFile('linux demand paged exe unmapped zero page', GnuExe)

   ELSE
      WriteString('   dont understand about this file type') ;
      WriteLn
   END ;
   IF Debug
   THEN
      WriteString('type        ') ;
      CASE VAL(CARDINAL, x.type) OF

      MSUN2 :  WriteString('sun 2') |
      M68010:  WriteString('mc68010') |
      M68020:  WriteString('mc68020') |
      MSPARC:  WriteString('sparc') |
      M386  :  WriteString('i386')

      ELSE
         WriteString('    unknown processor type')
      END ;
      WriteLn ;
      WriteString('flags       ') ; WriteHex(VAL(CARDINAL, x.flags), 2) ; WriteLn ;
      WriteString('type        ') ; WriteHex(VAL(CARDINAL, x.type), 2) ; WriteLn ;
      WriteString('text length ') ; WriteLong(x.text) ; WriteLn ;
      WriteString('bss  length ') ; WriteLong(x.bss) ; WriteLn ;
      WriteString('data length ') ; WriteLong(x.data) ; WriteLn ;
      WriteString('entry (pc)  ') ; WriteLong(x.entry) ; WriteLn ;
      WriteString('syms        ') ; WriteLong(x.syms) ; WriteLn ;
      WriteString('text rel sz ') ; WriteLong(x.textrelsize) ; WriteLn ;
      WriteString('data rel sz ') ; WriteLong(x.datarelsize) ; WriteLn
   END ;
   CASE ShortWordToCardinal(x.magic) OF

   ZMAGIC:  InitState(TextState, ZMagicOffset,
                      LongWordToCardinal(x.text), LongWordToCardinal(x.text), 0, 0) ;
            InitState(DataState, ZMagicOffset+LongWordToCardinal(x.text),
                      LongWordToCardinal(x.data), LongWordToCardinal(x.data), 0, 0) ;
            InitState(BssState, 0,
                      LongWordToCardinal(x.bss), LongWordToCardinal(x.bss), 0, 0) ;

   ELSE
      InitState(TextState, TSIZE(GnuExec),
                LongWordToCardinal(x.text), LongWordToCardinal(x.text), 0, 0) ;
      InitState(DataState, TSIZE(GnuExec)+LongWordToCardinal(x.text),
                LongWordToCardinal(x.data), LongWordToCardinal(x.data), 0, 0) ;
      InitState(BssState, 0,
                LongWordToCardinal(x.bss), LongWordToCardinal(x.bss), 0, 0)
   END ;
   IF LongWordToCardinal(x.entry)#0
   THEN
      Halt('expecting entry to be zero',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   RETURN( TRUE )
END ExamineQGNU ;


(*
   Sum4 -
*)

PROCEDURE Sum4 (a, b, c, d: BYTE) : CARDINAL ;
VAR
   result: CARDINAL ;
BEGIN
   result := VAL (CARDINAL, a) ;
   INC (result, VAL (CARDINAL, b)) ;
   INC (result, VAL (CARDINAL, c)) ;
   INC (result, VAL (CARDINAL, d)) ;
   RETURN result MOD 0100H
END Sum4 ;


(*
   ExamineMinix -
*)

PROCEDURE ExamineMinix (m: SHORTWORD) : BOOLEAN ;
VAR
   x: MinixObject ;
BEGIN
   Is16Bit := TRUE ;
   IF ReadNBytes( fi, TSIZE(MinixObject), ADR(x) ) # TSIZE(MinixObject)
   THEN
      Halt('failed to read minix object header',
           __FILE__, __FUNCTION__, __LINE__)
   END ;
   IF Debug
   THEN
      WriteString('Magic : BDE as object code') ; WriteLn ;
      WriteString('no of modules ') ; WriteShort(x.NoOfModules) ; WriteLn ;
      WriteString('start of text ') ; WriteLong(x.StartOfText) ; WriteLn ;
      WriteString('text length   ') ; WriteLong(x.TextSize) ; WriteLn ;
      WriteString('string length ') ; WriteShort(x.StringLength) ; WriteLn
   END ;
   SetKindOfFile('Magic : BDE as object code', MinixObj) ;
   WITH x DO
      IF Sum4 (Magic.ByteLo, Magic.ByteHi, NoOfModules.ByteLo, NoOfModules.ByteHi) # VAL(CARDINAL, Checksum)
      THEN
         WriteString(' object header checksum failed') ;
         WriteLn ;
         RETURN( FALSE )
      END
   END ;
   InitState(TextState,
             LongWordToCardinal(x.StartOfText),
             LongWordToCardinal(x.TextSize), LongWordToCardinal(x.TextSize),
             0, 0) ;
   InitState(DataState, 0, 0, 0, 0, 0) ;   (* fill this in *)
   RETURN( TRUE )
END ExamineMinix ;


(*
   Error -

PROCEDURE Error (a: ARRAY OF CHAR) ;
BEGIN

END Error ;
*)


(*
   GetByte - returns a byte from the appropriate segment of
             the binary file.

PROCEDURE GetByte () : BYTE ;
BEGIN

END GetByte ;
*)


(*
   GetByteFromData - returns a byte from the appropriate segment of
                     the binary file.
*)

PROCEDURE GetByteFromData (VAR b: BYTE) : BOOLEAN ;
BEGIN
   WITH DataState DO
      IF CurrentSegment#Data
      THEN
         SetPositionFromBeginning(fi, OffsetIntoFile+Index) ;
         CurrentSegment := Data
      END ;
      IF Index<PhysicalLength
      THEN
         IF ReadNBytes(fi, TSIZE(BYTE), ADR(b))#TSIZE(BYTE)
         THEN
            Halt('failed to read byte from data segment',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;
         IF DebugFlag
         THEN
            b := VAL(BYTE, (BytePosition DIV BytesPerSeg) MOD 0100H) ;
            INC(BytePosition)
         END
      ELSIF Index<VirtualLength
      THEN
         (* virtual > physical hence zero filled *)
         b := BYTE(0)
      ELSE
         RETURN( FALSE )
      END ;
      INC(Index)
   END ;
   RETURN( TRUE )
END GetByteFromData ;


(*
   PushBackDataByte - pushes a byte back onto the input stream.
                      It also returns this byte.

PROCEDURE PushBackDataByte (b: BYTE) : BYTE ;
BEGIN

END PushBackDataByte ;
*)

(*
   FillFromData - fills in a region of memory, Start..Start+Size-1 with
                  bytes from the data region.
                  The total number of bytes filled is returned.
*)

PROCEDURE FillFromData (Start: ADDRESS; Size: CARDINAL) : CARDINAL ;
VAR
   total,
   tsize: CARDINAL ;
BEGIN
   total := 0 ;
   WITH DataState DO
      IF CurrentSegment#Data
      THEN
         SetPositionFromBeginning(fi, OffsetIntoFile+Index) ;
         CurrentSegment := Data
      END ;
      IF Index<PhysicalLength
      THEN
         tsize := Min(Size, PhysicalLength-Index) ;
         IF ReadNBytes(fi, tsize, Start) # tsize
         THEN
            Halt('failed to read data bytes',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;
         INC(total, tsize) ;
         INC(Index, tsize) ;
         INC(Start, tsize) ;
         DEC(Size, tsize)
      END ;
      IF Index<VirtualLength
      THEN
         (* virtual > physical hence zero filled *)
         MemZero(Start, Size) ;
         INC(Index, Size) ;
         INC(total, Size)
      END
   END ;
   RETURN( total )
END FillFromData ;


(*
   GetByteFromText - returns a byte from the appropriate segment of
                     the binary file.
*)

PROCEDURE GetByteFromText (VAR b: BYTE) : BOOLEAN ;
BEGIN
   WITH TextState DO
      IF CurrentSegment#Text
      THEN
         SetPositionFromBeginning(fi, OffsetIntoFile+Index) ;
         CurrentSegment := Text
      END ;
      IF Index<PhysicalLength
      THEN
         IF ReadNBytes(fi, TSIZE(BYTE), ADR(b)) # TSIZE(BYTE)
         THEN
            Halt('failed to read a byte',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;
         IF DebugFlag
         THEN
            b := VAL(BYTE, (BytePosition DIV BytesPerSeg) MOD 0100H) ;
            INC(BytePosition)
         END
      ELSIF Index<VirtualLength
      THEN
         (* virtual > physical hence zero filled *)
         b := BYTE(0)
      ELSE
         RETURN( FALSE )
      END ;
      INC(Index)
   END ;
   RETURN( TRUE )
END GetByteFromText ;


(*
   Min - returns the least of two CARDINALs.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   FillFromText - fills in a region of memory, Start..Start+Size-1 with
                  bytes from the text region.
                  The total number of bytes filled is returned.
*)

PROCEDURE FillFromText (Start: ADDRESS; Size: CARDINAL) : CARDINAL ;
VAR
   total,
   tsize: CARDINAL ;
BEGIN
   total := 0 ;
   WITH TextState DO
      IF CurrentSegment#Text
      THEN
         SetPositionFromBeginning(fi, OffsetIntoFile+Index) ;
         CurrentSegment := Text
      END ;
      IF Index<PhysicalLength
      THEN
         tsize := Min(Size, PhysicalLength-Index) ;
         IF ReadNBytes(fi, tsize, Start) # tsize
         THEN
            Halt('failed to read text bytes',
                 __FILE__, __FUNCTION__, __LINE__)
         END ;
         INC(total, tsize) ;
         INC(Index, tsize) ;
         INC(Start, tsize) ;
         DEC(Size, tsize)
      ELSIF Index<VirtualLength
      THEN
         (* virtual > physical hence zero filled *)
         MemZero(Start, Size) ;
         INC(Index, Size) ;
         INC(total, Size)
      END
   END ;
   RETURN( total )
END FillFromText ;


(*
   PushBackTextByte - pushes a byte back onto the input stream.
                      It also returns this byte.

PROCEDURE PushBackTextByte (b: BYTE) : BYTE ;
BEGIN

END PushBackTextByte ;
*)

(*
   GetTextPhysicalSize - returns the physical size of the
                         text.
*)

PROCEDURE GetTextPhysicalSize () : CARDINAL ;
BEGIN
   RETURN( TextState.PhysicalLength )
END GetTextPhysicalSize ;


(*
   GetTextVirtualSize - returns the physical size of the
                         text.
*)

PROCEDURE GetTextVirtualSize () : CARDINAL ;
BEGIN
   RETURN( TextState.VirtualLength )
END GetTextVirtualSize ;


(*
   GetDataPhysicalSize - returns the physical data size.
*)

PROCEDURE GetDataPhysicalSize () : CARDINAL ;
BEGIN
   RETURN( DataState.PhysicalLength )
END GetDataPhysicalSize ;


(*
   GetDataVirtualSize - returns the virtual data size.
*)

PROCEDURE GetDataVirtualSize () : CARDINAL ;
BEGIN
   RETURN( DataState.VirtualLength )
END GetDataVirtualSize ;


(*
   GetBssVirtualSize - returns the physical size of the
                       bss.
*)

PROCEDURE GetBssVirtualSize () : CARDINAL ;
BEGIN
   RETURN( BssState.VirtualLength )
END GetBssVirtualSize ;


(*
   GetBssVirtualAddress - returns the physical size of the
                          bss.
*)

PROCEDURE GetBssVirtualAddress () : CARDINAL ;
BEGIN
   RETURN( BssState.VirtualStart )
END GetBssVirtualAddress ;


(*
   GetTextPhysicalAddress - returns the physical address of the
                            text.
*)

PROCEDURE GetTextPhysicalAddress () : CARDINAL ;
BEGIN
   RETURN( TextState.PhysicalStart )
END GetTextPhysicalAddress ;


(*
   GetTextVirtualAddress - returns the physical size of the
                           text.
*)

PROCEDURE GetTextVirtualAddress () : CARDINAL ;
BEGIN
   RETURN( TextState.VirtualStart )
END GetTextVirtualAddress ;


(*
   GetDataPhysicalAddress - returns the physical address of the data.
*)

PROCEDURE GetDataPhysicalAddress () : CARDINAL ;
BEGIN
   RETURN( DataState.PhysicalStart )
END GetDataPhysicalAddress ;


(*
   GetDataVirtualAddress - returns the virtual address of the data.
*)

PROCEDURE GetDataVirtualAddress () : CARDINAL ;
BEGIN
   RETURN( DataState.VirtualStart )
END GetDataVirtualAddress ;


(*
   InitState - initializes state, s.
*)

PROCEDURE InitState (VAR s: State;
                     Offset, TotalVirtual, TotalPhysical,
                     Virtual, Physical: CARDINAL) ;
BEGIN
   WriteString('************ setting state *************') ; WriteLn ;
   WITH s DO
      OffsetIntoFile := Offset ;
      VirtualLength := TotalVirtual ;
      PhysicalLength := TotalPhysical ;
      Index := 0 ;
      VirtualStart := Virtual ;
      PhysicalStart := Physical
   END
END InitState ;


(*
   InitText -
*)

PROCEDURE InitText (Offset, TotalVirtual, TotalPhysical, Virtual, Physical: CARDINAL) ;
BEGIN
   InitState(TextState, Offset, TotalVirtual, TotalPhysical, Virtual, Physical)
END InitText ;


(*
   InitData -
*)

PROCEDURE InitData (Offset, TotalVirtual, TotalPhysical, Virtual, Physical: CARDINAL) ;
BEGIN
   InitState(DataState, Offset, TotalVirtual, TotalPhysical, Virtual, Physical)
END InitData ;


(*
   InitBss -
*)

PROCEDURE InitBss (Offset, TotalVirtual, TotalPhysical, Virtual, Physical: CARDINAL) ;
BEGIN
   InitState(BssState, Offset, TotalVirtual, TotalPhysical, Virtual, Physical)
END InitBss ;


(*
   IsProcessor16Bit - returns TRUE if the processor specified in
                      the exec header is a 16 bit processor.
*)

PROCEDURE IsProcessor16Bit () : BOOLEAN ;
BEGIN
   RETURN( Is16Bit )
END IsProcessor16Bit ;


(*
   Test -
*)

PROCEDURE Test ;
VAR
   FileName: ARRAY [0..100] OF CHAR ;
BEGIN
   IF GetArg(FileName, 1)
   THEN
      IF Examine(FileName)
      THEN
      END
   END
END Test ;


(*
   DebugFile - places the current analysis of the current file into
               debug mode.
*)

PROCEDURE DebugFile ;
BEGIN
   DebugFlag := TRUE
END DebugFile ;


END AOUT.
