
#define TimerHandlerC

#include "../../ch/luk-types.h"
#include "../../ch/Storage.h"
#include "../../ch/Debug.h"
#include "../../ch/SYSTEM.h"
#include "../../ch/Executive.h"
#include "../../ch/ClockDevice.h"


#define SafeStr(X)  X, strlen(X)

#define MaxCharName     20



#define Divisor         59500    /* Divisor yields a clock freq of:        */

#define BaseTicksPerSec  20      /* 1/BaseTicksPerSec                      */
#define TimerIntNo      0x20     /* Timer Interrupt Number                 */
#define MaxQuantum      4        /* Maximum ticks a process may consume    */
                                 /* before being rescheduled.              */

#define TimerStackSize  50000    /* Reasonable sized stack for i?86 proc   */

#define Debugging       FALSE     /* Do you want lots of debugging info?   */


static unsigned int TotalTicks;   /* System up time tick count            */
static unsigned int CurrentQuanta; /* Currentprocess time quanta allowance */


typedef enum { active, dead, solo } QueueType;

typedef struct EventStruct {
  struct EventQueue {
    struct EventStruct *Left;
    struct EventStruct *Right;
  }                 EventQ;
  QueueType         WhichQ;
  Descriptor       *Process;
  unsigned int      NoOfTicks;
  unsigned int      WasCancelled;
} Event;

#include "../../ch/TimerHandler.h"


static Event *DeadQueue;     /* Free list of events.                 */
static Event *ActiveQueue;   /* Queue of outstanding timer requests  */


/*
 *  forward declarations ignore (C compiler fodder)
 */

static Event        *CreateSolo          (void);
static void          InitQueue           (struct EventQueue *q);
static void          OnDeadQueue         (Event *e);
static void          OnSoloQueue         (Event *e);
static void          OnActiveQueue       (Event *e);
static unsigned int IsOnSoloQueue        (Event *e);
static unsigned int IsOnDeadQueue        (Event *e);
static unsigned int IsOnActiveQueue      (Event *e);
static void          RemoveFromActive    (Event *e);
static void          DisplayActive       (void);
static void          CheckActiveQueue    (void);
static void          DisplayEvent        (Event *e);
static void          SubFrom             (Event **Head, Event *e);
static void          AddTo               (Event **Head, Event *e);
static void          RelativeAddToActive (Event *e);

/* end of forward declarations */


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
   GetTicks - returns the number of ticks since boottime.
*/

unsigned int TimerHandler_GetTicks (void)
{
  OnOrOff ToOldState;
  unsigned int CopyOfTicks;

  ToOldState = SYSTEM_TurnInterrupts(Off);          /* disable interrupts */
  CopyOfTicks = TotalTicks;
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
  return CopyOfTicks;
}


/*
   Sleep - suspends the current process for a time, t.
           The time is measured in ticks.
*/

void TimerHandler_Sleep (unsigned int t)
{
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);          /* disable interrupts */

  /* your code needs to go here */
  TimerHandler_WaitOn(TimerHandler_ArmEvent(t));    /* remove for student */

  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
}


/*
   More lower system calls to the timer procedures follow,
   they are necessary to allow handling multiple events.
*/


/*
   ArmEvent - initializes an event, e, to occur at time, t.
              The time, t, is measured in ticks.
              The event is NOT placed onto the event queue.
*/

Event *TimerHandler_ArmEvent (unsigned int t)
{
  Event *e;
  OnOrOff ToOldState;

  ToOldState = SYSTEM_TurnInterrupts(Off);           /* disable interrupts */
  e = CreateSolo();

  /* your code needs to go here */
  InitQueue(&e->EventQ);           /* not on a queue yet                   */      /* remove for student */
  e->WhichQ = solo;                /* and set the queue state accordingly  */      /* remove for student */
  e->Process = NULL;               /* no process waiting event yet         */      /* remove for student */
  e->NoOfTicks = t;                /* absolute number of ticks             */      /* remove for student */
  e->WasCancelled = FALSE;         /* has not been cancelled               */      /* remove for student */

  ToOldState = SYSTEM_TurnInterrupts(ToOldState);    /* restore interrupts */
  return e;
}


/*
   WaitOn - places event, e, onto the event queue and then the calling
            process suspends. It is resumed up by either the event
            expiring or the event, e, being cancelled.
            TRUE is returned if the event was cancelled
            FALSE is returned if the event expires.
*/

int TimerHandler_WaitOn (Event *e)
{
  OnOrOff ToOldState;
  unsigned int Cancelled;

  ToOldState = SYSTEM_TurnInterrupts(Off);       /* disable interrupts */
  if (e == NULL)
    Debug_Halt(SafeStr("event should never be NIL"),
	       __LINE__,
	       SafeStr(__FILE__));
  else {
    /* we will just check to see whether someone has cancelled this    */
    /* event before it ever got to the queue...                        */
    if (!e->WasCancelled) {
      /* right so it wasn't cancelled. Lets place it on the queue and */
      /* go to sleep.                                                 */
      e->Process = Executive_GetCurrentProcess();
	  /* so we know who is waiting  */
      OnActiveQueue(e);   /* add to the queue and then  */

      if (Debugging)   /* wait for Resume (we sleep) */
      {  /* debugging */
	DisplayActive();
      }

      Executive_Suspend();
    }
    /* At this point we have either been cancelled or not. We must     */
    /* check the event again as we might have been sleeping (Suspend)  */
    Cancelled = e->WasCancelled;
  }
  OnDeadQueue(e);   /* now it is safe to throw this event away */
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
  return Cancelled;
}


/*
   Cancel - cancels the event, e, on the event queue and makes
            the appropriate process runnable again.
            TRUE is returned if the event was cancelled and
            FALSE is returned is the event was not found or
                  no process was waiting on this event.
*/

int TimerHandler_Cancel (Event *e)
{
  OnOrOff ToOldState;
  unsigned int Cancelled;
  void *Private;

  ToOldState = SYSTEM_TurnInterrupts(Off);    /* disable interrupts */
  if (IsOnActiveQueue(e)) {
    Cancelled = !e->WasCancelled;
    if (e->WasCancelled)
      Debug_Halt(SafeStr("inconsistancy event has been cancelled and it is on queue"),
		 __LINE__,
		 SafeStr(__FILE__));
    OnSoloQueue(e);
    e->WasCancelled = TRUE;
    if (e->Process != NULL) {   /* double check that it has not     */
                                /* already been cancelled           */
      Private = e->Process;     /* we use our own Private variable  */
      e->Process = NULL;        /* as we need to set Process to NIL */
      e->Process = Executive_Resume(Private);
                                /* before we Resume. Otherwise      */
                                /* there is the possibility that it */
                                /* might be reused before we := NIL */
                                /* (because when we touch Resume    */
                                /* another process could run and..) */
    }
  } else
    Cancelled = FALSE;
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
  return Cancelled;
}


/*
   ReArmEvent - removes an event, e, from the event queue. A new time
                is given to this event and it is then re-inserted onto the
                event queue in the correct place.
                TRUE is returned if this occurred
                FALSE is returned if the event was not found.
*/

unsigned int TimerHandler_ReArmEvent (Event *e, unsigned int t)
{
  OnOrOff ToOldState;
  unsigned int ReArmed;

  ToOldState = SYSTEM_TurnInterrupts(Off);          /* disable interrupts */
  if (e->WasCancelled)
    ReArmed = FALSE;
  else if (IsOnActiveQueue(e) || IsOnSoloQueue(e)) {
    ReArmed = TRUE;
    OnSoloQueue(e);                                 /* remove from queue  */
    e->NoOfTicks = t;                               /* give it a new time */
                                                    /* back on queue      */
    OnActiveQueue(e);
  } else
    Debug_Halt(SafeStr("ReArm should not be asked to ReArm a dead event"),
	       __LINE__,
	       SafeStr(__FILE__));
  ToOldState = SYSTEM_TurnInterrupts(ToOldState);   /* restore interrupts */
  return ReArmed;
}


/*
   Timer  - is a process which serves the clock interrupt.
            Its function is fourfold:

            (i)   to maintain the timer event queue
            (ii)  to give some fairness to processes via round robin scheduling
            (iii) to keep a count of the total ticks so far  (time of day)
            (iv)  provide a heartbeat sign of life via the scroll lock LED
*/

static void Timer (void)
{
  unsigned int CurrentCount;
  OnOrOff ToOldState;
  unsigned int ScrollLED;

  ToOldState = SYSTEM_TurnInterrupts(Off);
  ScrollLED = FALSE;
  ClockDevice_StartClock((Divisor * BaseTicksPerSec) / TimerHandler_TicksPerSecond);
      /* Set clock to interrupt in 1/ClockFreq sec */
  while (TRUE) {
    Executive_WaitForIO((int)TimerIntNo);

    /* Get current clock count */
    CurrentCount = ClockDevice_LoadClock();

    /* Now compenstate for lost ticks */
    ClockDevice_StartClock(CurrentCount + (Divisor * BaseTicksPerSec) / TimerHandler_TicksPerSecond);

    /* your code needs to go here */
    TotalTicks++;                               /* (iii) */            /* remove for student */
    /* now pulse scroll LED */                                         /* remove for student */
    if (TotalTicks % TimerHandler_TicksPerSecond == 0) {               /* remove for student */
      ScrollLED = !ScrollLED;                   /* (iv)  */            /* remove for student */
      KeyBoardLEDs_SwitchScroll(ScrollLED);                            /* remove for student */
    }                                                                  /* remove for student */

    if ((TotalTicks & (MaxQuantum - 1)) == 0)   /* (ii)  */            /* remove for student */
    {                                                                  /* remove for student */
      Executive_RotateRunQueue();                                      /* remove for student */
    }                                                                  /* remove for student */

    CheckActiveQueue();                         /* (i)  */             /* remove for student */
  }
}


/*
   CheckActiveQueue - purpose is:

                      (i)   to remove all events which have expired
                      (ii)  resume all processes waiting on these events
                      (iii) decrement the first event with a non zero NoOfTicks
*/

static void CheckActiveQueue (void)
{
  Event *e;

  while (ActiveQueue != NULL && ActiveQueue->NoOfTicks == 0) {   /* (i)  */
    e = ActiveQueue;
    OnSoloQueue(e);
    /* note we do not put it onto the dead queue. The process
       waiting for the event will place, e, onto the dead queue */
    if (!e->WasCancelled && e->Process != NULL) {
      e->Process = Executive_Resume(e->Process);   /* (ii) */
      e->Process = NULL;
    }
  }
  if (ActiveQueue != NULL)   /* (iii) */
    ActiveQueue->NoOfTicks--;
}


/*
   CreateSolo - create a new event. It does this by either getting an event from
                the dead queue or (if the dead queue is empty) an event is created
                by using NEW.
*/

static Event *CreateSolo (void)
{
  Event *e;

  if (DeadQueue == NULL)
    Storage_ALLOCATE((void **)&e, sizeof(Event));
  else {
    e = DeadQueue;
    SubFrom(&DeadQueue, e);
  }
  e->WhichQ = solo;
  return e;
}


/*
   RemoveFromDead - removes event, e, from the dead queue.
*/

static void RemoveFromDead (Event *e)
{
  SubFrom(&DeadQueue, e);
}


/*
   OnDeadQueue - places an event onto the dead queue.
*/

static void OnDeadQueue (Event *e)
{
  if (e == NULL)
    return;
  OnSoloQueue(e);         /* put on solo queue first       */
  AddTo(&DeadQueue, e);   /* now safe to put on dead queue */
  e->WhichQ = dead;
}


/*
   OnSoloQueue - places an event onto the solo queue.
*/

static void OnSoloQueue (Event *e)
{
  if (e == NULL)
    return;
  if (IsOnActiveQueue(e))
    RemoveFromActive(e);
  else if (IsOnDeadQueue(e))
    RemoveFromDead(e);
  e->WhichQ = solo;
}


/*
   OnActiveQueue - places an event onto the active queue.
*/

static void OnActiveQueue (Event *e)
{
  if (e == NULL)
    return;
  if (IsOnDeadQueue(e)) {
    Debug_Halt(SafeStr("illegal state change"),
	       __LINE__,
	       SafeStr(__FILE__));
    return;
  }
  if (IsOnSoloQueue(e)) {
    RelativeAddToActive(e);
    e->WhichQ = active;
  }
}


/*
   IsOnSoloQueue - returns TRUE if event, e, is on the solo queue.
*/

static unsigned int IsOnSoloQueue (Event *e)
{
  return ((e != NULL) && (e->WhichQ == solo));
}


/*
   IsOnDeadQueue - returns TRUE if event, e, is on the dead queue.
*/

static unsigned int IsOnDeadQueue (Event *e)
{
  return ((e != NULL) && (e->WhichQ == dead));
}


/*
   IsOnActiveQueue - returns TRUE if event, e, is on the active queue.
*/

static unsigned int IsOnActiveQueue (Event *e)
{
  return ((e != NULL) && (e->WhichQ == active));
}


/*
   RemoveFromActive - removes an event, e, from the active queue.
*/

static void RemoveFromActive (Event *e)
{
  if (ActiveQueue == e) {
    SubFrom(&ActiveQueue, e);
    /* providing that the ActiveQueue is non empty we need to
       modify first event ticks as we have removed the first event, e. */
    if (ActiveQueue != NULL)
      ActiveQueue->NoOfTicks += e->NoOfTicks;
    return;
  }
  /* providing that event, e, is not the last event on the list then
     update the next event by the time of, e. */
  if (e->EventQ.Right != ActiveQueue)
    e->EventQ.Right->NoOfTicks += e->NoOfTicks;
  SubFrom(&ActiveQueue, e);
}


/*
   InsertBefore - insert an event, new, on a circular event queue BEFORE
                  event, pos.
*/

static void InsertBefore (Event **Head, Event *pos, Event *new)
{
  if (*Head == NULL) {
    /* empty queue */
    *Head = new;
    new->EventQ.Right = new;
    new->EventQ.Left = new;
    return;
  }
  if (*Head == pos) {
    /* insert before the first element on the queue */
    new->EventQ.Right = pos;
    new->EventQ.Left = pos->EventQ.Left;
    pos->EventQ.Left->EventQ.Right = new;
    pos->EventQ.Left = new;
    *Head = new;
    return;
  }
  /* insert before any other element */
  new->EventQ.Right = pos;
  new->EventQ.Left = pos->EventQ.Left;
  pos->EventQ.Left->EventQ.Right = new;
  pos->EventQ.Left = new;
}


/*
   InsertAfter - place an event, new, AFTER the event pos on any circular event queue.
*/

static void InsertAfter (Event *pos, Event *new)
{
  new->EventQ.Right = pos->EventQ.Right;
  new->EventQ.Left = pos;
  pos->EventQ.Right->EventQ.Left = new;
  pos->EventQ.Right = new;
}


/*
   RelativeAddToActive - the active event queue is an ordered queue of
                         relative time events.
                         The event, e, is inserted at the appropriate
                         position in the queue. The event, e, enters
                         this routine with an absolute NoOfTicks field which
                         is then used to work out the relative position
                         of the event. After the position is found then
                         the absolute NoOfTicks field is altered to a
                         relative value and inserted on the queue.
*/

static void RelativeAddToActive (Event *e)
{
  Event *t;
  unsigned int sum;

  if (ActiveQueue == NULL) {
    /* simple as the queue is empty (relative=absolute) */
    InsertBefore(&ActiveQueue, ActiveQueue, e);
    return;
  }

  /* at the end of the while loop sum will contain the total of all
     events up to but not including, t.
     If the value of sum is <  e^.NoOfTicks then e must be placed at the end
                            >= e^.NoOfTicks then e needs to be placed in the middle
  */

  sum = ActiveQueue->NoOfTicks;
  t = ActiveQueue->EventQ.Right;   /* second event */
  while (sum < e->NoOfTicks && t != ActiveQueue) {
    sum += t->NoOfTicks;
    t = t->EventQ.Right;
  }
  if (sum < e->NoOfTicks) {
    /* e will occur after all the current ActiveQueue has expired therefore
       we must add it to the end of the ActiveQueue. */

    e->NoOfTicks -= sum;
    InsertAfter(ActiveQueue->EventQ.Left, e);
  } else {
    /* as sum >= e^.NoOfTicks we know that e is scheduled to occur
       in the middle of the queue but before t^.Left
     */
    e->NoOfTicks += t->EventQ.Left->NoOfTicks - sum;
    InsertBefore(&ActiveQueue, t->EventQ.Left, e);
  }
  /* the first event after e must have its relative NoOfTicks altered */
  if (e->EventQ.Right != ActiveQueue)
    e->EventQ.Right->NoOfTicks -= e->NoOfTicks;
}


/*
   AddTo - adds an event to a specified queue.
*/

static void AddTo (Event  **Head, Event *e)
{
  if (*Head == NULL) {
    *Head = e;
    e->EventQ.Left = e;
    e->EventQ.Right = e;
    return;
  }
  e->EventQ.Right = *Head;
  e->EventQ.Left = (*Head)->EventQ.Left;
  (*Head)->EventQ.Left->EventQ.Right = e;
  (*Head)->EventQ.Left = e;
}


/*
   SubFrom - removes an event from a queue.
*/

static void SubFrom (Event **Head, Event *e)
{
  if ((e->EventQ.Left == *Head) && (e == *Head)) {
    *Head = NULL;
    return;
  }
  if (*Head == e)
    *Head = (*Head)->EventQ.Right;
  e->EventQ.Left->EventQ.Right = e->EventQ.Right;
  e->EventQ.Right->EventQ.Left = e->EventQ.Left;
}


/*
   DisplayActive - display the active queue.
*/

static void DisplayActive (void)
{
  Event *e;

  e = ActiveQueue;
  if (e == NULL)
    return;
  do {
    DisplayEvent(e);
    e = e->EventQ.Right;
  } while (e != ActiveQueue);
}


/*
   DisplayEvent - display a single event, e.
*/

static void DisplayEvent (Event *e)
{
  char a[MaxCharName+1];

  NumberIO_CardToStr(e->NoOfTicks, 6, a, MaxCharName);
  Debug_DebugString(a, MaxCharName);
  Debug_DebugString("  process (", 11);
  Executive_ProcessName(e->Process);
  Debug_DebugString(")", 1);
  if (e->WasCancelled)
    Debug_DebugString("  has been cancelled", 20);
  Debug_DebugString("\\n", 2);
}


/*
   InitQueue -
*/

static void InitQueue (struct EventQueue *q)
{
  q->Right = NULL;
  q->Left = NULL;
}


/*
   Init - starts the timer process and initializes some queues.
*/

static void Init (void)
{
  void *d;
  void (*TEMP)(void);

  TotalTicks = 0;
  CurrentQuanta = 0;
  ActiveQueue = NULL;
  DeadQueue = NULL;
  d = Executive_Resume(Executive_InitProcess((TEMP = Timer, TEMP),
					     TimerStackSize, "Timer", 5));
}


void _M2_TimerHandler_init(void)
{
  Init();
}


/*
 *  leave this function alone - it is expected by the linker.
 */

void _M2_TimerHandler_finish (void) {}
