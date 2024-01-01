
#define ExecutiveC
#include "../../ch/luk-types.h"
#include "../../ch/Storage.h"
#include "../../ch/Debug.h"
#include "../../ch/SYSTEM.h"
#include "../../ch/NumberIO.h"
#include "../../ch/StrLib.h"
#include "../../ch/gdb.h"

#define MaxCharsInName    15
#define IdleStackSize   5000

#define SafeStr(X)  X, strlen(X)


typedef enum {
  idle, lo, hi, maxpri,
} Priority;   /* process run priority          */

typedef enum {
  Runnable, Suspended, WaitOnSem, WaitOnInt
} State;

typedef char EntityName[MaxCharsInName + 1];


/* defines dijkstra's semaphores */

typedef struct Semaphore {
  unsigned int       Value;         /* semaphore value               */
  EntityName         SemName;       /* semaphore name for debugging  */
  struct Descriptor  *Who;           /* queue of waiting processes    */
  struct SemQueue {
    struct Semaphore *Right,
                     *Left;
  }                   ExistsQ;      /* list of existing semaphores   */
} Semaphore;

/*
 *  the process descriptor
 */

typedef struct Descriptor {
  PROCESS              Volatiles;    /* process volatile environment  */
  struct DesQueue {
    struct Descriptor *Right, *Left;
  }                    ReadyQ,       /* queue of ready processes      */
                       ExistsQ,      /* queue of existing processes   */
                       SemaphoreQ;   /* queue of waiting processes    */
  Semaphore           *Which;        /* which semaphore are we waiting*/
  EntityName           RunName;      /* process name for debugging    */
  State                Status;       /* state of process              */
  Priority             RunPriority;  /* runtime priority of process   */
  unsigned int         Size;         /* Maximum stack size            */
  void                *Start;        /* Stack start                   */
  boolean              Debugged;     /* Does user want to debug a     */
                                     /* deadlocked process?           */
} Descriptor;

#include "../../ch/Executive.h"


/*
 *  prototypes for functions.
 */

static void        Reschedule           (void);
static Descriptor *SubFromSemaphoreTop  (Descriptor **Head);
static Descriptor *NextReady            (void);
static void        DisplayProcess       (Descriptor *p);
static void        WriteNSpaces         (unsigned int n);
static void        AddToExists          (Descriptor *Item);
static void        AddToSemaphore       (Descriptor **Head, Descriptor *Item);
static Descriptor *SubFromSemaphoreTop  (Descriptor **Head);
static void        AddToSemaphoreExists (Semaphore *Item);
static void        AddToReady           (Descriptor *Item);
static void        SubFromReady         (Descriptor *Item);
static void        SubFromSemaphore     (Descriptor **Head, Descriptor *Item);
static void        InitQueue            (struct DesQueue *q);


/* global variables */

static Descriptor *ExistsQueue;       /* List of existing processes    */
static Descriptor *RunQueue[maxpri];  /* List of runnable processes    */
static Descriptor *CurrentProcess;
static Semaphore  *AllSemaphores;     /* List of all semaphores        */
static Descriptor *IdleProcess;       /* Idle process always runnable  */


/*
 *  simple utility function
 */

static int strlen (char *s)
{
  int i=0;

  while (s[i] != (char)0) {
    i++;
  }
  return i;
}


/*
 *  Assert -
 */

static void Assert (boolean c,
		    char *file, unsigned int file_high, unsigned int line)
{
  char db[81];

  if (! c) {
    Debug_DebugCString("IdleProcess = ");
    NumberIO_CardToStr((unsigned int)IdleProcess, 0, db, 80);
    Debug_DebugString(db, 80);
    Debug_DebugCString("\\n");

    Debug_DebugCString("Vols  = ");
    NumberIO_CardToStr((unsigned int)IdleProcess->Volatiles, 0, db, 80);
    Debug_DebugString(db, 80);
    Debug_DebugCString("\\n");
    Executive_Ps();
    Debug_Halt(SafeStr("Assert failed"), line, file, file_high);
  }
}


/*
 *  InitProcess - initializes a process which is held in the suspended
 *                state. When the process is resumed it will start executing
 *                procedure, p. The process has a maximum stack size of,
 *                StackSize, bytes and its textual name is, Name.
 *                The StackSize should be at least 5000 bytes.
 */

Descriptor *Executive_InitProcess (void (*p)(void), unsigned int StackSize,
				   char *Name, unsigned int Name_High)
{
  Descriptor *d;
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);                            /* disable interrupts */
  Storage_ALLOCATE((void **)&d, sizeof(Descriptor));
  d->Size = StackSize;

  Storage_ALLOCATE((void **)&d->Start, StackSize);
                                                 /* allocate space for this processes stack */
  SYSTEM_NEWPROCESS(p, d->Start, StackSize, &d->Volatiles);         /* create volatiles     */
  InitQueue(&d->ReadyQ);                         /* not on the ready queue as suspended     */
  AddToExists(d);                                /* add process to the exists queue         */
  InitQueue(&d->SemaphoreQ);                     /* not on a semaphore queue yet            */
  d->Which = NULL;                               /* not on a semaphore queue yet            */
  StrLib_StrCopy(Name, Name_High, (char *)&d->RunName, MaxCharsInName);
                                                 /* copy name into descriptor for debugging */
  d->Status = Suspended;                         /* this process will be suspended          */
  d->RunPriority = lo;                           /* all processes start off at lo priority  */
  d->Debugged = FALSE;                           /* no need to debug deadlock yet!          */
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);                     /* restore interrupts */
  return d;                                      /* and return a descriptor to the caller   */
}


/*
 *  Resume - resumes a suspended process. If all is successful then the process, p,
 *           is returned. If it fails then NIL is returned.
 */

Descriptor *Executive_Resume (Descriptor *d)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);          /* disable interrupts */

  /* your code needs to go here */
  if (d->Status != Suspended) {                                                  /* remove for student */
    /* we are trying to Resume a process which is */                             /* remove for student */
    Debug_DebugCString("trying to resume a process which is not suspended");     /* remove for student */
    return NULL;            /* not held in a Suspended state - error      */     /* remove for student */
  }                                                                              /* remove for student */
                                                /* legal state transition */     /* remove for student */
  d->Status = Runnable;                             /* change status      */     /* remove for student */
  AddToReady(d);                                    /* add to run queue   */     /* remove for student */
  RunQueue[d->RunPriority] = d;                     /* make d at top of q */     /* remove for student */
  Reschedule();   /* check whether this process has a higher run priority */     /* remove for student */
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
  return d;
}


/*
 *  Suspend - suspend the calling process.
 *            The process can only continue running if another process
 *            Resumes it.
 */

void Executive_Suspend(void)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);          /* disable interrupts */
  CurrentProcess->Status = Suspended;
  SubFromReady(CurrentProcess);
  Reschedule();
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
}


/*
 *  InitSemaphore - creates a semaphore whose initial value is, v, and
 *                  whose name is, Name.
 */

Semaphore *Executive_InitSemaphore (unsigned int v,
				    char *Name, unsigned int Name_HIGH)
{
  Semaphore *s;
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);          /* disable interrupts */
  Storage_ALLOCATE((void **)&s, sizeof(Semaphore));
  s->Value = v;                   /* initial value of semaphore           */
  StrLib_StrCopy(Name, Name_HIGH, (char *)&s->SemName, MaxCharsInName);
                                  /* save the name for future debugging   */
  s->Who = NULL;                  /* no one waiting on this semaphore yet */
  AddToSemaphoreExists(s);        /* add semaphore to exists list         */
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
  return s;
}


/*
 *  Wait - performs dijkstra's P operation on a semaphore.
 *         A process which calls this procedure will
 *         wait until the value of the semaphore is > 0
 *         and then it will decrement this value.
 */

void Executive_Wait (Semaphore *s)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);          /* disable interrupts */

  /* your code needs to go here */
  if (s->Value > 0)                                                          /* remove for student */
    s->Value--;                                                              /* remove for student */
  else {                                                                     /* remove for student */
    SubFromReady(CurrentProcess);                   /* remove from run q  */ /* remove for student */
    AddToSemaphore((Descriptor **)&s->Who, CurrentProcess);                  /* remove for student */
                                                    /* add to semaphore q */ /* remove for student */
    CurrentProcess->Status = WaitOnSem;             /* set new status     */ /* remove for student */
    CurrentProcess->Which  = s;                     /* debugging aid      */ /* remove for student */
    Reschedule();                                   /* find next process  */ /* remove for student */
  }                                                                          /* remove for student */
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
}


/*
 *  Signal - performs dijkstra's V operation on a semaphore.
 *           A process which calls the procedure will increment
 *           the semaphores value.
 */

void Executive_Signal (Semaphore *s)
{
  OnOrOff ToOldState;
  Descriptor *d;

  ToOldState = SYSTEM_TurnInterrupts(Off);   /* disable interrupts */
  if (s->Who == NULL)   /* no process waiting */
    s->Value++;
  else {
    d = SubFromSemaphoreTop(&s->Who);  /* remove process from semaphore q  */
    d->Which = NULL;                   /* no longer waiting on semaphore   */
    d->Status = Runnable;              /* set new status                   */
    AddToReady(d);                     /* add process to the run queue     */
                                       /* find out whether there is a      */
                                       /* higher priority to run.          */
    Reschedule();
  }
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
}


/*
 *  WaitForIO - waits for an interrupt to occur on vector, VectorNo.
 */

void Executive_WaitForIO (unsigned int VectorNo)
{
  Descriptor *Calling;
  PROCESS Next;
  OnOrOff ToOldState;
  Descriptor *WITH;

  ToOldState = SYSTEM_TurnInterrupts(Off);
  SubFromReady(CurrentProcess);   /* remove process from run queue */
  /*
     alter run priority to hi as all processes waiting for an interrupt
     are scheduled to run at the highest priority.
  */
  CurrentProcess->Status = WaitOnInt; /* it will be blocked waiting for an interrupt.  */
  CurrentProcess->RunPriority = hi;   /* this (hopefully) allows it to run as soon as  */
                                      /* the interrupt occurs.                         */
  Calling = CurrentProcess;                 /* process which called WaitForIO          */
  CurrentProcess = NextReady();             /* find next process to run while we wait  */
  Next = CurrentProcess->Volatiles;
  /*
     This is quite complicated. We transfer control to the next process saving
     our volatile environment into the Calling process descriptor volatiles.
     When an interrupt occurs the calling process will be resumed and the
     interrupted process volatiles will be placed into Next.
  */
  SYSTEM_IOTRANSFER(&Calling->Volatiles, &Next, VectorNo);

  /*
     At this point the interrupt has just occurred and the volatiles of
     the interrupted process are in Next. Next is the current process
     and so we must save them before picking up the Calling descriptor.
  */
  CurrentProcess->Volatiles = Next;              /* carefully stored away */
  CurrentProcess = Calling;                      /* update CurrentProcess */
  CurrentProcess->Status = Runnable;             /* add to run queue      */
  AddToReady(CurrentProcess);
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
}


/*
 *  Ps - displays a process list together with relevant their status.
 */

void Executive_Ps(void)
{
  OnOrOff     ToOldState;
  Descriptor *p;
  Semaphore  *s;
  char        a[6];

  ToOldState = SYSTEM_TurnInterrupts(Off);   /* disable interrupts */
  p = ExistsQueue;
  if (p != NULL) {
    do {
      DisplayProcess(p);
      p = p->ExistsQ.Right;
    } while (p != ExistsQueue);
  }
  s = AllSemaphores;
  if (s != NULL) {
    do {
      Debug_DebugString(s->SemName, MaxCharsInName);
      WriteNSpaces(MaxCharsInName - StrLib_StrLen(s->SemName, MaxCharsInName));
      NumberIO_CardToStr(s->Value, 0, a, 5);
      Debug_DebugString(a, 5);
      Debug_DebugCString("\\n");
      s = s->ExistsQ.Right;
    } while (s != AllSemaphores);
  }
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
}


/*
 *  WriteNSpaces - writes, n, spaces.
 */

static void WriteNSpaces(unsigned int n)
{
  while (n > 0) {
    Debug_DebugCString(" ");
    n--;
  }
}


/*
 *  DisplayProcess - displays the process, p, together with its status.
 */

static void DisplayProcess(Descriptor *p)
{
  char a[5];

  Debug_DebugString(p->RunName, MaxCharsInName);
  WriteNSpaces(MaxCharsInName - StrLib_StrLen(p->RunName, MaxCharsInName));
  switch (p->RunPriority) {

  case idle:
    Debug_DebugCString(" idle ");
    break;

  case lo:
    Debug_DebugCString(" lo   ");
    break;

  case hi:
    Debug_DebugCString(" hi   ");
    break;

  default:
    break;
  }
  switch (p->Status) {

  case Runnable:
    Debug_DebugCString("runnable ");
    break;

  case Suspended:
    Debug_DebugCString("suspended");
    break;

  case WaitOnSem:
    Debug_DebugCString("waitonsem   (");
    Debug_DebugString(p->Which->SemName, MaxCharsInName);
    Debug_DebugCString(")");
    break;

  case WaitOnInt:
    Debug_DebugCString("waitonint");
    break;

  default:
    break;
  }
  Debug_DebugCString("  Stack usage (");
  if (p->Size == 0) {
    Debug_DebugCString("  0%)\\n");
    return;
  }
  NumberIO_CardToStr(((unsigned int)p->Volatiles - (unsigned int)p->Start) * 100 / p->Size, 3,
		     a, 4);
  Debug_DebugString(a, 4);
  Debug_DebugCString("%)\\n");
}


/*
 *  GetCurrentProcess - returns the descriptor of the current running
 *                      process.
 */

Descriptor *Executive_GetCurrentProcess (void)
{
  OnOrOff ToOldState;
  Descriptor *p;

  ToOldState = SYSTEM_TurnInterrupts(Off);   /* disable interrupts */
  p = CurrentProcess;
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
  return p;
}


/*
 *  RotateRunQueue - rotates the process run queue.
 */

void Executive_RotateRunQueue (void)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);   /* disable interrupts */
  /* we only need to rotate the lo priority processes as:
     idle - should only have one process (the idle process)
     hi   - are the device drivers which most of the time are performing
            WaitForIO
  */
  if (RunQueue[(int)lo] != NULL) {
    RunQueue[(int)lo] = RunQueue[(int)lo]->ReadyQ.Right;
  }
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
}


/*
 *  ProcessName - displays the name of process, d, through
 *                DebugString.
 */

void Executive_ProcessName (Descriptor *d)
{
  Debug_DebugString(d->RunName, MaxCharsInName);
}


/*
 *  DebugProcess -
 */

void Executive_DebugProcess (Descriptor *d)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);
  if (d->Status == WaitOnSem) {
    Debug_DebugCString("debugging process (");
    Debug_DebugString(d->RunName, MaxCharsInName);
    Debug_DebugCString(") was waiting on semaphore (");
    Debug_DebugString(d->Which->SemName, MaxCharsInName);
    Debug_DebugCString(")\\n");
    SubFromSemaphore(&d->Which->Who, d);
    AddToReady(d);
    d->Status = Runnable;
    d->Debugged = TRUE;
    Reschedule();
  } else {
    Debug_DebugCString("can only debug deadlocked processes (");
    Debug_DebugString(d->RunName, MaxCharsInName);
    Debug_DebugCString(") which are waiting on a semaphore\\n");
  }
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);
}


/*
 *  CheckDebugged - checks to see whether the debugged flag has
 *                  been set by the debugger.
 *                  TRUE  is returned if the process was debugged.
 *                  FALSE is returned if the process was not debugged.
 */

static boolean CheckDebugged (void)
{
  if (CurrentProcess->Debugged) {
    /*
      You will see this comment after you have enabled a
      deadlocked process to continue via the gdb command:

      print Executive_DebugProcess(d)
      
      debugger caused deadlocked process to continue
    */
    gdb_breakpoint();
    CurrentProcess->Debugged = FALSE;
    SubFromReady(CurrentProcess);
    AddToSemaphore(&CurrentProcess->Which->Who, CurrentProcess);
    CurrentProcess->Status = WaitOnSem;
    return TRUE;
  }
  return FALSE;
}


/*
 *  ScheduleProcess - finds the highest priority Runnable process and
 *                    then transfers control to it.
 */

static void ScheduleProcess(void)
{
  Descriptor *From, *Highest;

  Highest = NextReady();

  /* rotate ready Q to ensure fairness */
  RunQueue[Highest->RunPriority] = Highest->ReadyQ.Right;

  /* no need to transfer if Highest=CurrentProcess */
  if (Highest == CurrentProcess)
    return;
  From = CurrentProcess;
  /* alter CurrentProcess before we TRANSFER */
  CurrentProcess = Highest;
  SYSTEM_TRANSFER(&From->Volatiles, Highest->Volatiles);
}


/*
   Reschedule - reschedules to the highest runnable process.
*/

static void Reschedule(void)
{
  /*
   *  the repeat loop allows us to debug a process even when it is
   *  technically waiting on a semaphore. We run the process into
   *  a breakpoint and then back into this schedule routine.
   *  This is really useful when trying to find out why processes have
   *  deadlocked.
   */
  do {
    ScheduleProcess();
  } while (CheckDebugged());
}


/*
 *  NextReady - returns the highest priority Runnable process.
 */

static Descriptor *NextReady(void)
{
  Descriptor *Highest;
  Priority    Pri;

  Highest = NULL;
  for (Pri = idle; Pri <= hi; Pri = Pri + 1) {
    if (RunQueue[(int)Pri] != NULL)
      Highest = RunQueue[(int)Pri];
  }
  Assert(Highest != NULL, SafeStr(__FILE__), __LINE__);
  return Highest;
}


/*
 *  AddToExists - adds item, Item, to the exists queue.
 */

static void AddToExists (Descriptor *Item)
{
  if (ExistsQueue == NULL) {
    ExistsQueue = Item;                 /* Head is empty therefore make */
    Item->ExistsQ.Left = Item;          /* Item the only entry on this  */
    Item->ExistsQ.Right = Item;         /* queue.                       */
  } else {
    Item->ExistsQ.Right = ExistsQueue;  /* Add Item to the end of queue */
    Item->ExistsQ.Left  = ExistsQueue->ExistsQ.Left;
    ExistsQueue->ExistsQ.Left->ExistsQ.Right = Item;
    ExistsQueue->ExistsQ.Left = Item;
  }
}


/*
 *  AddToSemaphore - adds item, Item, to the semaphore queue defined by Head.
 */

static void AddToSemaphore(Descriptor **Head, Descriptor *Item)
{
  if (*Head == NULL) {
    *Head = Item;                    /* Head is empty therefore make */
    Item->SemaphoreQ.Left = Item;    /* Item the only entry on this  */
    Item->SemaphoreQ.Right = Item;   /* queue.                       */
    return;
  } else {
    Item->SemaphoreQ.Right = *Head;  /* Add Item to the end of queue */
    Item->SemaphoreQ.Left =  (*Head)->SemaphoreQ.Left;
    (*Head)->SemaphoreQ.Left->SemaphoreQ.Right = Item;
    (*Head)->SemaphoreQ.Left = Item;
  }
}


/*
 *  AddToSemaphoreExists - adds item, Item, to the semaphore exists queue.
 */

static void AddToSemaphoreExists (Semaphore *Item)
{
  if (AllSemaphores == NULL) {
    AllSemaphores = Item;       /* Head is empty therefore make */
    Item->ExistsQ.Left = Item;	/* Item the only entry on this  */
    Item->ExistsQ.Right = Item; /* queue.                       */
  } else {
    Item->ExistsQ.Right = AllSemaphores;   /* Add Item to the end of queue */
    Item->ExistsQ.Left  = AllSemaphores->ExistsQ.Left;
    AllSemaphores->ExistsQ.Left->ExistsQ.Right = Item;
    AllSemaphores->ExistsQ.Left = Item;
  }
}


/*
 *  AddToReadyQ - adds item, Item, to the ready queue defined by Head.
 */

static void AddToReadyQ(Descriptor **Head, Descriptor *Item)
{
  if (*Head == NULL) {
    *Head = Item;               /* Head is empty therefore make */
    Item->ReadyQ.Left = Item;	/* Item the only entry on this  */
    Item->ReadyQ.Right = Item;  /* queue.                       */
  } else {
    Item->ReadyQ.Right = *Head; /* Add Item to the end of queue */
    Item->ReadyQ.Left  = (*Head)->ReadyQ.Left;
    (*Head)->ReadyQ.Left->ReadyQ.Right = Item;
    (*Head)->ReadyQ.Left = Item;
  }
}


/*
 *  AddToReady - adds item, Item, to the ready queue.
 */

static void AddToReady (Descriptor *Item)
{
  AddToReadyQ(&RunQueue[Item->RunPriority], Item);
}


/*
 *  SubFromReadyQ - removes a process, Item, from a queue, Head.
 */

static void SubFromReadyQ (Descriptor **Head, Descriptor *Item)
{
  if ((Item->ReadyQ.Right == *Head) && (Item == *Head)) {
    *Head = NULL;
  } else {
    if (*Head == Item) {
      *Head = (*Head)->ReadyQ.Right;
    }
    Item->ReadyQ.Left->ReadyQ.Right = Item->ReadyQ.Right;
    Item->ReadyQ.Right->ReadyQ.Left = Item->ReadyQ.Left;
  }
}


/*
 *  SubFromReady - subtract process descriptor, Item, from the Ready queue.
 */

static void SubFromReady(Descriptor *Item)
{
  SubFromReadyQ(&RunQueue[Item->RunPriority], Item);
}


/*
 *  SubFromSemaphore - removes a process, Item, from a queue, Head.
 */

static void SubFromSemaphore (Descriptor **Head, Descriptor *Item)
{
  if ((Item->SemaphoreQ.Right == *Head) && (Item == *Head)) {
    *Head = NULL;
  } else {
    if (*Head == Item) {
      *Head = (*Head)->SemaphoreQ.Right;
    }
    Item->SemaphoreQ.Left->SemaphoreQ.Right = Item->SemaphoreQ.Right;
    Item->SemaphoreQ.Right->SemaphoreQ.Left = Item->SemaphoreQ.Left;
  }
}


/*
 *  SubFromSemaphoreTop - returns the first descriptor in the
 *                        semaphore queue.
 */

static Descriptor *SubFromSemaphoreTop (Descriptor **Head)
{
  Descriptor *Top;

  Top = *Head;
  SubFromSemaphore(Head, Top);
  return Top;
}


/*
 *  Idle - this process is only run whenever there is no other Runnable
 *         process. It should never be removed from the run queue.
 */

static void Idle(void)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(On);   /* enable interrupts */
  /*
     Listen for interrupts.
     We could solve chess endgames here or calculate Pi etc.
     We forever wait for an interrupt since there is nothing else
     to do...
  */
  while (TRUE) ;
  /* we must NEVER exit from the above loop */
}


/*
 *  InitIdleProcess - creates an idle process descriptor which
 *                    is run whenever no other process is Runnable.
 *                    The Idle process should be the only process which
 *                    has the priority idle.
 */


static void InitIdleProcess(void)
{
  char db[81];

  Storage_ALLOCATE((void **)&IdleProcess, sizeof(Descriptor));
  Debug_DebugCString("IdleProcess = ");
  NumberIO_CardToStr((unsigned int)IdleProcess, 0, db, 80);
  Debug_DebugString(db, 80);
  Debug_DebugCString("\\n");

  Storage_ALLOCATE(&IdleProcess->Start, (int)IdleStackSize);
  IdleProcess->Size = IdleStackSize;
  SYSTEM_NEWPROCESS(Idle, IdleProcess->Start,
		    IdleStackSize, &IdleProcess->Volatiles);
  InitQueue(&IdleProcess->SemaphoreQ);  /* not on a semaphore queue     */
  IdleProcess->Which = NULL;            /* at all.                      */
  StrLib_StrCopy("Idle", 3, (char *)&IdleProcess->RunName, MaxCharsInName);
                                        /* idle process's name          */
  IdleProcess->Status = Runnable;       /* should always be idle        */
  IdleProcess->RunPriority = idle;      /* lowest priority possible     */
  IdleProcess->Debugged = FALSE;        /* should never be debugging    */
  AddToReady(IdleProcess);              /* should be the only           */
                                        /* process at this run priority */
                                        /* process now exists..         */
  AddToExists(IdleProcess);
}


/*
 *  InitInitProcess - creates a descriptor for this running environment
 *                    so it too can be manipulated by the Reschedule.
 *
 *                    This concept is important to understand.
 *                    InitInitProcess is called by the startup code to this
 *                    module. It ensures that the current stack and processor
 *                    volatiles can be "housed" in a process descriptor and
 *                    therefore it can be manipulated just like any other
 *                    process.
 */

static void InitInitProcess(void)
{
  Storage_ALLOCATE((void **)&CurrentProcess, sizeof(Descriptor));
  CurrentProcess->Size = 0;                /* init uses the default stack and      */
  CurrentProcess->Start = NULL;            /* we don't need to know where it is.   */
  /*
      Volatiles - we will store the processor registers here when we
      context switch - through Reschedule.
  */
  InitQueue(&CurrentProcess->ReadyQ);      /* assign queues to NIL                 */
  InitQueue(&CurrentProcess->ExistsQ);
  InitQueue(&CurrentProcess->SemaphoreQ);  /* not waiting on a semaphore queue yet */
  CurrentProcess->Which = NULL;            /* at all.                              */
  StrLib_StrCopy("Init", 3, (char *)&CurrentProcess->RunName, MaxCharsInName);
                                              /* name for debugging purposes       */
  CurrentProcess->Status = Runnable;          /* currently running                 */
  CurrentProcess->RunPriority = lo;           /* default status                    */
  CurrentProcess->Debugged = FALSE;           /* not deadlock debugging yet        */
  AddToExists(CurrentProcess);
  AddToReady(CurrentProcess);
}


/*
 *  InitQueue - initializes a queue, q, to empty.
 */

static void InitQueue (struct DesQueue *q)
{
  q->Right = NULL;
  q->Left  = NULL;
}


/*
 *  Init - initializes all the global variables.
 */

static void Init(void)
{
  ExistsQueue         = NULL;
  RunQueue[(int)lo]   = NULL;
  RunQueue[(int)hi]   = NULL;
  RunQueue[(int)idle] = NULL;
  AllSemaphores       = NULL;
  InitInitProcess();
  InitIdleProcess();
}


void _M2_Executive_init (void)
{
  Init();
}

/* linker fodder - leave alone */

void _M2_Executive_finish (void)
{
}
