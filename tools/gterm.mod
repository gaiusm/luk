MODULE gterm[7] ;

(*
   Author     : Gaius Mulley
   Title      : gterm
   System     : Logitech M-2
   Date       : 18/10/92
   Description: Provides a graphical terminal front end to Morloc.
*)

IMPORT DebugPMD ;
FROM SYSTEM IMPORT ENABLE, DISABLE, ADDRESS ;
FROM AdvMap IMPORT Rooms, Line, DoorStatus, Door, Room, Treasure,
                   ActualNoOfRooms, MaxNoOfTreasures, MaxNoOfRooms,
                   NoOfRoomsToHidePlayers, NoOfRoomsToSpring,
                   NoOfRoomsToHideCoal, NoOfRoomsToHideGrenade,
                   ReadAdvMap, Adjacent, IncPosition,
                   FileName, MaxLengthOfFileName ;

FROM StdIO IMPORT ReadReady ;
FROM TTIO IMPORT Read ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM SpriteManipulation IMPORT Sprite, InitSprite, DownSprite, NilSprite,
                               BlankSprite,
                               PixColours, Bitmask, SpriteX, SpriteY,
                               CopySprite, TransparentSprite,
                               GetSprite, PutSprite,
                               MergeSpriteTop, MergeSpriteBottom ;
FROM SpriteIO IMPORT ReadSprite ;
FROM VGA IMPORT DivideX, DivideY, ClearScreen ;
FROM FileSystem IMPORT Lookup, Close, File, Response, ReadChar ;
FROM ASCII IMPORT nul, EOL, esc ;
FROM NumberIO IMPORT StrToInt, StrToCard ;
FROM Storage IMPORT ALLOCATE ;
FROM Kernel IMPORT PtrToEvent, SetEvent, WaitOn,
                   SEMAPHORE, Wait, Signal, InitSemaphore,
                   InitProcess, PtrToProcDes, ProcType, Resume, Cancel ;
IMPORT MonStrIO ;
IMPORT DebugIO ;


CONST
   XPixcelsPerSquare = 6 ;
   YPixcelsPerSquare = 8 ;
   North             = 0 ;
   East              = 1 ;
   South             = 2 ;
   West              = 3 ;
   Compass           = 4 ;
   MaxPlayers        = 2 ;
   MaxMovements      = 10 ;
   MaxTime           = 65535 ;

TYPE
   SequenceInfo = POINTER TO SequenceDesc ;
   SequenceDesc = RECORD
                     Picture : Sprite ;
                     Duration: CARDINAL ;
                     DeltaX,
                     DeltaY  : INTEGER ;
                     Next    : SequenceInfo ;
                  END ;

   ManState   = (draw, stand, turn) ;

   PtrToAction = POINTER TO Action ;
   Action      = RECORD
                    PlayerN: CARDINAL ;
                    Time   : CARDINAL ;
                    Forever: BOOLEAN ;
                    X, Y   : INTEGER ;
                    PFrame : SequenceInfo ;
                    Next   : PtrToAction ;
                 END ;

   MovementInfo = RECORD
                     X, Y   : CARDINAL ;
                     Forever: BOOLEAN ;
                     Move   : SequenceInfo ;
                  END ;

   PlayerInfo = RECORD
                   x, y     : CARDINAL ;
                   Room     : CARDINAL ;
                   Direction: CARDINAL ;
                   State    : ManState ;
                   Movement : ARRAY [0..MaxMovements] OF MovementInfo ;
                   Head,
                   Tail     : CARDINAL ;
                   Available: SEMAPHORE ;
                   Space    : SEMAPHORE ;
                   Mutex    : SEMAPHORE ;
                   Action   : PtrToAction ;
                END ;

VAR
   BlankSquare,
   TreasureObj,
   WallHorizontalObj, WallVerticalObj,
   DoorHorizontalObj, DoorVerticalObj: Sprite ;
   Walk, Kill, Hit, Draw,
   Stand,
   Sheth, Parry, Thrust,
   Arrow                             : ARRAY [North..West] OF SequenceInfo ;
   ScreenOffsetX, ScreenOffsetY,
   ScreenWidth, ScreenHeight         : CARDINAL ;
   Players                           : ARRAY [0..MaxPlayers] OF PlayerInfo ;
   HeadOfActionList                  : PtrToAction ;
   ActionMutex, ForAction            : SEMAPHORE ;
   FreeActions                       : PtrToAction ;
   ch                                : CHAR ;
   CurrentEvent                      : PtrToEvent ;
   CancelledPlayer                   : CARDINAL ;
   OnForever                         : BOOLEAN ;


(*
   InitializeActions - initialize everything to do with actions.
*)

PROCEDURE InitializeActions ;
BEGIN
   InitSemaphore(ActionMutex, 1) ;
   InitSemaphore(ForAction, 0) ;
   FreeActions := NIL ;
   HeadOfActionList := NIL ;
   CurrentEvent := PtrToEvent(NIL)
END InitializeActions ;


(*
   PutSequence - places a sequence into a players buffer.
                 The flag, f, indicates whether this frame should be
                 displayed forever or until it is cancelled.
*)

PROCEDURE PutSequence (m: CARDINAL; s: SequenceInfo; f: BOOLEAN) ;
BEGIN
   WITH Players[m] DO
      IF (Action#NIL) AND (Action^.Forever)
      THEN
         Action^.Forever := FALSE ;
         IF Action^.PFrame=NIL
         THEN
            Action^.Time := 0
         ELSE
            Action^.Time := Action^.PFrame^.Duration
         END
      END ;
      IF (CurrentEvent#PtrToEvent(NIL)) AND OnForever
      THEN
         CancelledPlayer := m ;
         Cancel(CurrentEvent, f) ;
         CurrentEvent := PtrToEvent(NIL) ;
         IF NOT f THEN HALT END ;
      END ;
      Wait(Space) ;
      Wait(Mutex) ;
      WITH Movement[Tail] DO
         Move := s ;
         Forever := f ;
         X := Players[m].x ;
         Y := Players[m].y
      END ;
      Tail := (Tail+1) MOD MaxMovements ;
      Signal(Mutex) ;
      Signal(Available) ;
      Signal(ForAction)
   END
END PutSequence ;


(*
   GetSequence - fetches a sequence from a players buffer.
*)

PROCEDURE GetSequence (m: CARDINAL; VAR s: SequenceInfo;
                       VAR i, j: CARDINAL; VAR f: BOOLEAN) ;
BEGIN
   WITH Players[m] DO
      Wait(Available) ;
      Wait(Mutex) ;
      WITH Movement[Head] DO
         f := Forever ;
         s := Move ;
         i := X ;
         j := Y
      END ;
      Head := (Head+1) MOD MaxMovements ;
      Signal(Mutex) ;
      Signal(Space) ;
   END
END GetSequence ;


(*
   IsSequenceAvailable - returns true if a sequence is available from
                         player, m.
*)

PROCEDURE IsSequenceAvailable (m: CARDINAL) : BOOLEAN ;
VAR
   IsAvail: BOOLEAN ;
BEGIN
   WITH Players[m] DO
      Wait(Mutex) ;
      IsAvail := (Head#Tail) ;
      Signal(Mutex)
   END ;
   RETURN( IsAvail )
END IsSequenceAvailable ;


(*
   FindUnderSprite - fills in the sprite which containing the
                     info for square, x, y.
*)

PROCEDURE FindUnderSprite (Head: PtrToAction; s: Sprite; x, y: CARDINAL) ;
BEGIN
   CopySprite(BlankSprite(), s)
END FindUnderSprite ;


(*
   CreateAction - places a new action entry onto the head of action
                  list.
*)

PROCEDURE CreateAction (s: SequenceInfo; x, y: INTEGER;
                        f: BOOLEAN; n: CARDINAL) : PtrToAction ;
VAR
   a: PtrToAction ;
   m: Bitmask ;
   c: PixColours ;
BEGIN
   NewAction(a) ;
   WITH a^ DO
      PlayerN := n ;
      Time := 0 ;
      Forever := f ;
      X := (x-INTEGER(ScreenOffsetX)) * XPixcelsPerSquare ;
      Y := (y-INTEGER(ScreenOffsetY)) * YPixcelsPerSquare ;
      PFrame := s ;
      Next := HeadOfActionList
   END ;
   HeadOfActionList := a ;
   RETURN( a )
END CreateAction ;


(*
   ChooseSmallestDeltaTime - returns the smallest delta time on
                             the action list.
*)

PROCEDURE ChooseSmallestDeltaTime (Head: PtrToAction) : PtrToAction ;
VAR
   p: PtrToAction ;
   try,
   dt : CARDINAL ;
BEGIN
   p := Head ;
   IF Head=NIL
   THEN
      MonStrIO.WriteString('\n  Severe error head = NIL in ChooseSmallest\n') ;
   END ;
   IF Head^.Next#NIL
   THEN
      IF p^.PFrame=NIL
      THEN
         dt := 0
      ELSIF Head^.Forever AND (Head^.PFrame^.Next=NIL)
      THEN
         try := MaxTime
      ELSE
         dt := p^.PFrame^.Duration-p^.Time
      END ;
      Head := Head^.Next ;
      WHILE Head#NIL DO
         IF Head^.PFrame=NIL
         THEN
            try := 0
         ELSIF Head^.Forever AND (Head^.PFrame^.Next=NIL)
         THEN
            try := MaxTime
         ELSE
            try := Head^.PFrame^.Duration-Head^.Time
         END ;
         IF try<dt
         THEN
            dt := try ;
            p := Head
         END ;
         Head := Head^.Next
      END
   END ;
   RETURN( p )
END ChooseSmallestDeltaTime ;


(*
   ConstructActionList - constructs an action list based on the available
                         sequences. If an action is created then
                         TRUE is returned.
*)

PROCEDURE ConstructActionList (VAR a: PtrToAction) : BOOLEAN ;
VAR
   s   : SequenceInfo ;
   i, j,
   p   : CARDINAL ;
   f   : BOOLEAN ;
BEGIN
   FOR p := 0 TO MaxPlayers DO
      WITH Players[p] DO
         IF (Action#NIL) AND (Action^.Forever) AND IsSequenceAvailable(p)
         THEN
            Action^.Forever := FALSE
         ELSIF Action=NIL
         THEN
            IF IsSequenceAvailable(p)
            THEN
               GetSequence(p, s, i, j, f) ;
               Action := CreateAction(s, i, j, f, p) ;
               a := Action ;
               RETURN( TRUE )
            END
         END
      END
   END ;
   RETURN( FALSE )
END ConstructActionList ;


(*
   Animation - runs the action sequences.
*)

PROCEDURE Animation ;
VAR
   OldSprite,
   WorkSprite: Sprite ;
   c, bc     : PixColours ;
   m         : Bitmask ;
   a         : PtrToAction ;
   i, j      : CARDINAL ;
BEGIN
   MonStrIO.WriteString('\nInitSprite\n') ;
   FOR j := 0 TO SpriteY DO
      FOR i := 0 TO SpriteX DO
         bc[i, j] := 0
      END
   END ;
   WorkSprite := InitSprite(m, c) ;
   OldSprite := InitSprite(m, c) ;
   MonStrIO.WriteString('\nWait ForAction\n') ;
   Wait(ForAction) ;
   LOOP
      Wait(ActionMutex) ;
      WHILE ConstructActionList(a) DO
         IF a^.PFrame#NIL
         THEN
            a^.X := a^.X + a^.PFrame^.DeltaX ;
            a^.Y := a^.Y + a^.PFrame^.DeltaY
         END ;
         WorkOutFrameSprite(HeadOfActionList, WorkSprite, a^.X, a^.Y) ;
         DownSprite(WorkSprite, NilSprite(), a^.X, a^.Y) ;
      END ;
      a := ChooseSmallestDeltaTime(HeadOfActionList) ;
      Signal(ActionMutex) ;
      IF FrameExpired(a)
      THEN
      END ;
      Wait(ActionMutex) ;
      i := a^.X ;
      j := a^.Y ;
      IF a^.PFrame#NIL
      THEN
         WorkOutFrameSprite(HeadOfActionList, OldSprite, i, j) ;
         a^.PFrame := a^.PFrame^.Next
      END ;
      IF a^.PFrame=NIL
      THEN
         DeleteAction(HeadOfActionList, a) ;
         DisposeAction(a) ;
         WorkOutFrameSprite(HeadOfActionList, WorkSprite, i, j) ;
         DownSprite(WorkSprite, NilSprite(), i, j) ;
         Signal(ActionMutex) ;
         Wait(ForAction)
      ELSE
         GetSprite(OldSprite, m, c) ;
         PutSprite(OldSprite, m, bc) ;
         a^.X := a^.X + a^.PFrame^.DeltaX ;
         a^.Y := a^.Y + a^.PFrame^.DeltaY ;
         WorkOutFrameSprite(HeadOfActionList, WorkSprite, a^.X, a^.Y) ;
         MergeSpriteBottom(WorkSprite, OldSprite, a^.X, a^.Y, i, j) ;
         DownSprite(WorkSprite, NilSprite(), a^.X, a^.Y) ;
         (*
            work out what should be under i,j
         *)
         WorkOutFrameSprite(HeadOfActionList, WorkSprite, i, j) ;
         MergeSpriteBottom(WorkSprite, OldSprite, i, j, i, j) ;
         DownSprite(WorkSprite, NilSprite(), i, j) ;
         Signal(ActionMutex) ;
      END
   END
END Animation ;


(*
   FrameExpired - returns true if a frame expires without being cancelled.
*)

PROCEDURE FrameExpired (a: PtrToAction) : BOOLEAN ;
VAR
   p : PtrToAction ;
   c : BOOLEAN ;
   dt: CARDINAL ;
BEGIN
   c := FALSE ;
   IF (a#NIL) AND (a^.PFrame#NIL)
   THEN
      IF a^.Forever AND (a^.PFrame^.Next=NIL)
      THEN
         dt := MaxTime ;
         OnForever := TRUE ;
      ELSE
         dt := a^.PFrame^.Duration-a^.Time
      END ;
      SetEvent(CurrentEvent, dt) ;
      WaitOn(CurrentEvent, c) ;
      CurrentEvent := PtrToEvent(NIL) ;
      IF c AND OnForever
      THEN
         IF CancelledPlayer=a^.PlayerN
         THEN
            a^.Forever := FALSE ;
            a^.PFrame := NIL
         END ;
         dt := 0
      END ;
      OnForever := FALSE ;
      p := HeadOfActionList ;
      WHILE p#NIL DO
         IF p#a
         THEN
            INC(p^.Time, dt)
         END ;
         p := p^.Next
      END ;
      a^.Time := 0
   END ;
   RETURN( NOT c )
END FrameExpired ;


(*
   WorkOutFrameSprite - works out what needs to be drawn at
                        position, x, y.
*)

PROCEDURE WorkOutFrameSprite (p: PtrToAction; s: Sprite; x, y: CARDINAL) ;
BEGIN
   IF p=NIL
   THEN
      TransparentSprite(s)
      (* CopySprite(BlankSprite(), s) *)
   ELSE
      IF p^.PFrame#NIL
      THEN
         WorkOutFrameSprite(p^.Next, s, x, y) ;
         MergeSpriteTop(s, p^.PFrame^.Picture, x, y, p^.X, p^.Y)
      END
   END
END WorkOutFrameSprite ;


(*
   MoveToNextFrame - moves the acton sequence, a, onto the next frame.
                     WorkSprite is filled in according to what should
                     be displayed at a^.X a^.Y.
*)
(*
PROCEDURE MoveToNextFrame (VAR Head: PtrToAction; a, b: PtrToAction;
                           WorkSprite: Sprite) ;
BEGIN
   IF a=b
   THEN
      CopySprite(a^.Under, WorkSprite) ;
      a^.PFrame := a^.PFrame^.Next ;
      IF a^.PFrame=NIL
      THEN
         DeleteAction(Head, a) ;
         DisposeAction(a)
      ELSE
         MergeSprite(WorkSprite, a^.PFrame^.Picture, a^.X, a^.Y, a^.X, a^.Y)
      END
   ELSE
      MoveToNextFrame(Head, a, b^.Next, WorkSprite) ;
      IF b^.PFrame#NIL
      THEN
         MergeSprite(WorkSprite, b^.PFrame^.Picture, a^.X, a^.Y, b^.X, b^.Y)
      END
   END
END MoveToNextFrame ;
*)

(*
   DeleteAction - deletes the action, a, from list, Head.
*)

PROCEDURE DeleteAction (VAR Head: PtrToAction; a: PtrToAction) ;
VAR
   p, q: PtrToAction ;
   i   : CARDINAL ;
BEGIN
   IF Head=a
   THEN
      Head := Head^.Next
   ELSE
      p := Head ;
      q := Head^.Next ;
      WHILE (q#NIL) AND (q#a) DO
         p := q ;
         q := q^.Next
      END ;
      IF q=a
      THEN
         p^.Next := q^.Next
      END
   END ;
   FOR i := 0 TO MaxPlayers DO
      WITH Players[i] DO
         IF Action=a
         THEN
            Action := NIL
         END
      END
   END
END DeleteAction ;


(*
   NewAction - creates a new action.
*)

PROCEDURE NewAction (VAR a: PtrToAction) ;
BEGIN
   IF FreeActions=NIL
   THEN
      NEW(a) ;
      IF a=NIL THEN HALT END
   ELSE
      a := FreeActions ;
      FreeActions := a^.Next
   END
END NewAction ;


(*
   DisposeAction - destroys an action.
*)

PROCEDURE DisposeAction (a: PtrToAction) ;
BEGIN
   a^.Next := FreeActions ;
   FreeActions := a
END DisposeAction ;


(*
   InitializeObjects - initializes the objects wall, door, treasures etc.
*)

PROCEDURE InitializeObjects ;
BEGIN
   InitializeBlank ;
   WallHorizontalObj := InitObject('wallh') ;
   WallVerticalObj := InitObject('wallv') ;
   DoorHorizontalObj := InitObject('doorh') ;
   DoorVerticalObj := InitObject('doorv') ;
   TreasureObj := InitObject('treasure')
END InitializeObjects ;


(*
   InitializeBlank - creates a blank square.
*)

PROCEDURE InitializeBlank ;
VAR
   c: PixColours ;
   m: Bitmask ;
   x, y: CARDINAL ;
BEGIN
   FOR y := 0 TO SpriteY DO
      m[y] := {}
   END ;
   FOR y := SpriteY-YPixcelsPerSquare TO SpriteY DO
      FOR x := 0 TO XPixcelsPerSquare-1 DO
         INCL(m[y], x) ;
         c[x, y] := 0
      END
   END ;
   BlankSquare := InitSprite(m, c)
END InitializeBlank ;


(*
   InitObject - reads in a single object from a file, a.
                It returns a sprite.
*)

PROCEDURE InitObject (a: ARRAY OF CHAR) : Sprite ;
VAR
   f: File ;
   c: PixColours ;
   m: Bitmask ;
   s: Sprite ;
BEGIN
   s := InitSprite(m, c) ;
   Lookup(f, a, FALSE) ;
   IF (f.res#done) OR (NOT ReadSprite(f, s))
   THEN
      WriteString('Failed to read ') ; WriteString(a) ; WriteLn ; HALT
   ELSE
      Close(f)
   END ;
   RETURN( s )
END InitObject ;


(*
   Init - initializes the data structures.
*)

PROCEDURE Init ;
BEGIN
   ScreenWidth := DivideX(1, 1) DIV XPixcelsPerSquare ;
   ScreenHeight := DivideY(1, 1) DIV YPixcelsPerSquare ;
   ScreenOffsetX := 60 ;
   ScreenOffsetY := 40 ;
   InitializePlayers ;
   InitializeObjects ;
   InitializeSequences ;
   InitializeActions
END Init ;


(*
   InitializePlayers - initialize the player data structure.
*)

PROCEDURE InitializePlayers ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 0 TO MaxPlayers DO
      WITH Players[i] DO
         x := 64 ;
         y := 44 ;
         Direction := East ;
         Room := 0 ;
         State := stand ;
         InitSemaphore(Available, 0) ;
         InitSemaphore(Space, MaxMovements-1) ;
         InitSemaphore(Mutex, 1) ;
         Head := 0 ;
         Tail := 0 ;
         Action := NIL
      END
   END
END InitializePlayers ;


(*
   InitializeSequences - initializes the animation sequences.
*)

PROCEDURE InitializeSequences ;
BEGIN
   (* East *)

   Walk[East] := InitSequence('walkr') ;
   Kill[East] := InitSequence('killr') ;
   Hit[East] := InitSequence('hitr') ;
   Draw[East] := InitSequence('drawr') ;
   Sheth[East] := InitSequence('shethr') ;
   Parry[East] := InitSequence('parryr') ;
   Thrust[East] := InitSequence('thrustr') ;
   Arrow[East] := InitSequence('arrowr') ;
   Stand[East] := InitSequence('standr') ;

   (* West *)

   Walk[West] := InitSequence('walkl') ;
   Kill[West] := InitSequence('killl') ;
   Hit[West] := InitSequence('hitl') ;
   Draw[West] := InitSequence('drawl') ;
   Sheth[West] := InitSequence('shethl') ;
   Parry[West] := InitSequence('parryl') ;
   Thrust[West] := InitSequence('thrustl') ;
   Arrow[West] := InitSequence('arrowl') ;
   Stand[West] := InitSequence('standl') ;

   (* North *)

   Stand[North] := InitSequence('standn') ;

   (* South *)

   Stand[South] := InitSequence('stands') ;
END InitializeSequences ;


(*
   InitSequence - loads an animation sequence.
*)

PROCEDURE InitSequence (a: ARRAY OF CHAR) : SequenceInfo ;
VAR
   f: File ;
   s: SequenceInfo ;
BEGIN
   MonStrIO.WriteString(a) ; MonStrIO.WriteLn ;
   Lookup(f, a, FALSE) ;
   s := NIL ;
   IF f.res=done
   THEN
      WHILE ReadSequence(f, s) DO
      END ;
      Close(f) ;
   END ;
   RETURN( s )
END InitSequence ;


(*
   ReadSequence - reads an Sequence, from a file, f, it puts this Sequence
                  on the end of the List.
*)

PROCEDURE ReadSequence (VAR f: File; VAR List: SequenceInfo) : BOOLEAN ;
VAR
   m   : Bitmask ;
   c   : PixColours ;
   a   : ARRAY [0..20] OF CHAR ;
   o, e: SequenceInfo ;
BEGIN
   NEW(o) ;
   o^.Picture := InitSprite(m, c) ;
   IF ReadSprite(f, o^.Picture)
   THEN
      ReadStringFromFile(f, a) ;
      WITH o^ DO
         StrToCard(a, Duration) ;
         ReadStringFromFile(f, a) ;
         StrToInt(a, DeltaX) ;
(*
         MonStrIO.WriteString('dx = ') ; MonStrIO.WriteString(a) ;
         MonStrIO.WriteString('  ') ; MonStrIO.WriteInt(DeltaX, 4) ;MonStrIO.WriteLn ;
         DebugIO.Read(ch) ;
*)
         ReadStringFromFile(f, a) ;
         StrToInt(a, DeltaY) ;
         Next := NIL
      END ;
      IF List=NIL
      THEN
         List := o
      ELSE
         e := List ;
         WHILE e^.Next#NIL DO
            e := e^.Next
         END ;
         e^.Next := o
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END ReadSequence ;


(*
   ReadStringFromFile - reads a string from a file.
*)

PROCEDURE ReadStringFromFile (VAR f: File; VAR a: ARRAY OF CHAR) ;
VAR
   i: CARDINAL ;
BEGIN
   i := 0 ;
   REPEAT
      ReadChar(f, a[i]) ;
      INC(i)
   UNTIL (i>HIGH(a)) OR (a[i-1]=EOL) ;
   IF i<=HIGH(a)
   THEN
      a[i-1] := nul
   END
END ReadStringFromFile ;


(*
   Obey - obey the commands of the user.
*)

PROCEDURE Obey ;
VAR
   ch: CHAR ;
   p : PtrToProcDes ;
BEGIN
   InitProcess(p, Animation, 0, 2, 10000, 0, User, NIL, 'Animate') ;
   ClearScreen ;
   MonStrIO.WriteString('\nbefore resume') ;
   Resume(p) ;
   MonStrIO.WriteString('after resume\n') ;
   REPEAT
      Read(ch) ;
      MonStrIO.WriteString(ch) ;
      Interpret(ch)
   UNTIL ch=esc
END Obey ;


(*
   Interpret - turn the characters into movements.
*)

PROCEDURE Interpret (ch: CHAR) ;
BEGIN
   CASE ch OF

   'p' : DoParry |
   't' : DoThrust |
   'f' : DoArrow |
   'm' : DoWalk |
   'k' : DoKill |
   'h' : DoHit |
   'v' : DoVault |
   'r' : DoRight

   ELSE
   END
END Interpret ;


(*
   DoRight - works out the right turn sequence of movements.
*)

PROCEDURE DoRight ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      MakeState(n, stand, FALSE) ;
      WITH Players[n] DO
         CreateManMovement(n, Stand[GetManDirection(n)], TRUE) ;
         Direction := (Direction+1) MOD Compass
      END
   END
END DoRight ;


(*
   DoVault - works out the vault sequence of movements.
*)

PROCEDURE DoVault ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      MakeState(n, stand, FALSE) ;
      WITH Players[n] DO
         CreateManMovement(n, Stand[GetManDirection(n)], FALSE) ;
         Direction := (Direction+1) MOD Compass ;
         CreateManMovement(n, Stand[GetManDirection(n)], TRUE) ;
         Direction := (Direction+1) MOD Compass
      END
   END
END DoVault ;


(*
   DoKill - works out the Kill sequence of movements.
*)

PROCEDURE DoKill ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      MakeState(n, draw, FALSE) ;
      CreateManMovement(n, Kill[GetManDirection(n)], FALSE) ;
      Players[n].State := stand ;
      CreateManMovement(n, Stand[GetManDirection(n)], TRUE)
   END
END DoKill ;


(*
   DoHit - works out the Hit sequence of movements.
*)

PROCEDURE DoHit ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      MakeState(n, stand, FALSE) ;
      CreateManMovement(n, Hit[GetManDirection(n)], FALSE) ;
      MakeState(n, stand, TRUE) ;
   END
END DoHit ;


(*
   DoParry - works out the parry sequence of movements.
*)

PROCEDURE DoParry ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      MakeState(n, draw, FALSE) ;
      CreateManMovement(n, Parry[GetManDirection(n)], TRUE) ;
      MakeState(n, stand, TRUE) ;
      IF NOT ReadReady()
      THEN
         MakeState(n, stand, TRUE)
      END
   END
END DoParry ;


(*
   MakeState - alter a mans state to s.
*)

PROCEDURE MakeState (n: CARDINAL; s: ManState; Forever: BOOLEAN) ;
BEGIN
   WITH Players[n] DO
      IF s#State
      THEN
         IF (State=stand) AND (s=draw)
         THEN
            CreateManMovement(n, Draw[GetManDirection(n)], Forever)
         ELSIF (State=draw) AND (s=stand)
         THEN
            CreateManMovement(n, Sheth[GetManDirection(n)], Forever)
         END ;
         State := s
      END
   END
END MakeState ;


(*
   DoThrust - works out the thrust sequence of movements.
*)

PROCEDURE DoThrust ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      MakeState(n, draw, FALSE) ;
      CreateManMovement(n, Thrust[GetManDirection(n)], FALSE) ;
      IF NOT ReadReady()
      THEN
         MakeState(n, stand, TRUE)
      END
   END
END DoThrust ;


(*
   DoArrow - works out the arrow sequence of movements.
*)

PROCEDURE DoArrow ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      MakeState(n, stand, FALSE) ;
      CreateManMovement(n, Arrow[GetManDirection(n)], TRUE) ;
      MakeState(n, stand, TRUE)
   END
END DoArrow ;


(*
   DoWalk - works out the walk sequence of movements.
*)

PROCEDURE DoWalk ;
VAR
   ch: CHAR ;
   n : CARDINAL ;
BEGIN
   Read(ch) ;
   IF IsLegalPlayer(ch)
   THEN
      n := GetPlayer(ch) ;
      IF Players[n].Room#1
      THEN
         Players[n].Room := 1 ;
         DrawRoom(1)
      END ;
      MakeState(n, stand, FALSE) ;
      CreateManMovement(n, Walk[GetManDirection(n)], TRUE) ;
      CASE GetManDirection(n) OF

      East: INC(Players[n].x) |
      West: DEC(Players[n].x) |
      North: INC(Players[n].y) |
      South: DEC(Players[n].y)

      END ;
      MakeState(n, stand, TRUE) ;
   END
END DoWalk ;


(*
   IsLegalPlayer - returns true if ch indicates a legal player.
*)

PROCEDURE IsLegalPlayer (ch: CHAR) : BOOLEAN ;
BEGIN
   IF (ch<'0') OR (ch>CHR(ORD('0')+MaxPlayers))
   THEN
      RETURN( FALSE )
   ELSE
      RETURN( TRUE )
   END
END IsLegalPlayer ;


(*
   GetPlayer - returns the player number from, ch.
*)

PROCEDURE GetPlayer (ch: CHAR) : CARDINAL ;
BEGIN
   RETURN( ORD(ch)-ORD('0') )
END GetPlayer ;


(*
   GetManDirection - returns the direction of man, n.
*)

PROCEDURE GetManDirection (n: CARDINAL) : CARDINAL ;
BEGIN
   IF n>MaxPlayers
   THEN
      RETURN( North )
   ELSE
      RETURN( Players[n].Direction )
   END
END GetManDirection ;


(*
   CreateManMovement - creates a movement sequence associated with
                       man, n.
*)

PROCEDURE CreateManMovement (m: CARDINAL; s: SequenceInfo; Forever: BOOLEAN) ;
BEGIN
   PutSequence(m, s, Forever)
END CreateManMovement ;


(*
   Delay - delays the process by n/20 ths of a second.
*)

PROCEDURE Delay (n: CARDINAL) ;
VAR
   e: PtrToEvent ;
   c: BOOLEAN ;
BEGIN
   SetEvent(e, n) ;
   WaitOn(e, c)
END Delay ;


(*
   RunAnimation - runs the animation sequence defined so far.
*)
(*
PROCEDURE RunAnimation (m: CARDINAL) ;
VAR
   o   : SequenceInfo ;
   ch  : CHAR ;
   i, j,
   x, y: INTEGER ;
BEGIN
   o := Players[m].Movement ;
   x := (Players[m].x-ScreenOffsetX) * XPixcelsPerSquare ;
   y := (Players[m].y-ScreenOffsetY) * YPixcelsPerSquare ;
   IF o#NIL
   THEN
      DownSprite(BlankSprite(), NilSprite(), x, y)
   END ;
   WHILE o#NIL DO
      WITH o^ DO
         DownSprite(Picture, NilSprite(), x, y) ;
         Delay(Duration) ;
         IF Next#NIL
         THEN
            i := x ;
            j := y ;
            x := x+Next^.DeltaX ;
            y := y+Next^.DeltaY ;
            DownSprite(BlankSprite(), NilSprite(), i, j)
         END
      END ;
      o := o^.Next
   END
END RunAnimation ;
*)

(*
   DrawRoom - draws the room, r, which is visable on the screen.
*)

PROCEDURE DrawRoom (r: CARDINAL) ;
VAR
   ok            : BOOLEAN ;
   x1, y1, x2, y2,
   t, i          : CARDINAL ;
   ds            : DoorStatus ;
BEGIN
   t := Rooms[r].NoOfWalls ;
   FOR i := 1 TO t DO
      WITH Rooms[r].Walls[i] DO
         x1 := X1 ;
         y1 := Y1 ;
         x2 := X2 ;
         y2 := Y2
      END ;
      Clip( x1, y1, x2, y2, ScreenOffsetX, ScreenOffsetY, ok ) ;

      IF ok
      THEN
         WLine( x1, y1, x2, y2 )
      END
   END ;

   t := Rooms[r].NoOfDoors ;
   FOR i := 1 TO t DO
      WITH Rooms[r].Doors[i] DO
         x1 := Position.X1 ;
         y1 := Position.Y1 ;
         x2 := Position.X2 ;
         y2 := Position.Y2 ;
         ds := StateOfDoor
      END ;
      IF ds#Secret
      THEN
         Clip( x1, y1, x2, y2, ScreenOffsetX, ScreenOffsetY, ok ) ;
         IF ok
         THEN
            DLine( x1, y1, x2, y2, ds )
         END
      END
   END ;

   FOR i := 1 TO MaxNoOfTreasures DO
      WITH Rooms[r] DO
         IF i IN Treasures
         THEN
            x1 := Treasure[i].Xpos ;
            y1 := Treasure[i].Ypos ;
            ClipPoint( x1, y1, ScreenOffsetX, ScreenOffsetY, ok ) ;
            IF ok
            THEN
               DownSprite(TreasureObj, NilSprite(), x1*XPixcelsPerSquare, y1*YPixcelsPerSquare)
            END
         END
      END
   END
END DrawRoom ;


(*
   WLine - draws a wall from x1, y1 to x2, y2.
*)

PROCEDURE WLine (x1, y1, x2, y2: CARDINAL) ;
VAR
   x, ys: CARDINAL ;
BEGIN
   IF y1=y2
   THEN
      y1 := y1 * YPixcelsPerSquare ;
      FOR x := x1 TO x2 DO
         DownSprite(WallHorizontalObj, NilSprite(), x*XPixcelsPerSquare, y1)
      END
   ELSE
      ys := y1 ;
      x1 := x1 * XPixcelsPerSquare ;
      (* y2 is largest val of y coord *)
      DownSprite(WallVerticalObj, NilSprite(), x1, ys*YPixcelsPerSquare) ;
      WHILE ys<y2 DO
         INC( ys ) ;
         DownSprite(WallVerticalObj, NilSprite(), x1, ys*YPixcelsPerSquare)
      END
   END ;
END WLine ;


PROCEDURE DLine (x1, y1, x2, y2: CARDINAL ; ds: DoorStatus) ;
VAR
   x, ys: CARDINAL ;
   s    : Sprite ;
BEGIN
   IF y1=y2
   THEN
      CASE ds OF

      Secret : s := WallHorizontalObj |
      Open   : s := BlankSquare |
      Closed : s := DoorHorizontalObj

      END ;
      y1 := y1 * YPixcelsPerSquare ;
      FOR x := x1 TO x2 DO
         DownSprite(s, NilSprite(), x*XPixcelsPerSquare, y1)
      END
   ELSE
      CASE ds OF

      Secret : s := WallVerticalObj |
      Open   : s := BlankSquare |
      Closed : s := DoorVerticalObj

      END ;
      ys := y1 ;
      x1 := x1 * XPixcelsPerSquare ;
      (* y2 is largest val of y coord *)
      DownSprite(s, NilSprite(), x1, ys*YPixcelsPerSquare) ;
      WHILE ys<y2 DO
         INC( ys ) ;
         DownSprite(s, NilSprite(), x1, ys*YPixcelsPerSquare)
      END
   END
END DLine ;


(*
   ClipPoint - clips the point x, y.
*)

PROCEDURE ClipPoint (VAR x, y: CARDINAL ; Sx, Sy: CARDINAL ; VAR ok: BOOLEAN) ;
BEGIN
   IF ( (x>=Sx) AND (x<=Sx+ScreenWidth) AND
        (y>=Sy) AND (y<=Sy+ScreenHeight) )
   THEN
      DEC( x, Sx ) ;
      DEC( y, Sy ) ;
      ok := TRUE
   ELSE
      ok := FALSE
   END
END ClipPoint ;



PROCEDURE Clip (VAR x1, y1, x2, y2: CARDINAL ;
                            Sx, Sy: CARDINAL ;
                            VAR ok: BOOLEAN) ;
BEGIN
   IF (Sx>x2) OR (Sx+ScreenWidth<x1)
   THEN
      ok := FALSE ;
   ELSIF (Sy>y2) OR (Sy+ScreenHeight<y1)
   THEN
      ok := FALSE
   ELSE
      ok := TRUE ;
      IF Sx>x1
      THEN
         x1 := 0
      ELSE
         DEC( x1, Sx )
      END ;
      IF Sy>y1
      THEN
         y1 := 0
      ELSE
         DEC( y1, Sy )
      END ;
      IF x2-Sx>ScreenWidth
      THEN
         x2 := ScreenWidth
      ELSE
         DEC( x2, Sx )
      END ;
      IF y2-Sy>ScreenHeight
      THEN
         y2 := ScreenHeight
      ELSE
         DEC( y2, Sy )
      END
   END
END Clip ;

      
BEGIN
   Init ;
   ReadAdvMap ;
   Obey
END gterm.
