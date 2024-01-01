
#if !defined(TimerHandlerH)
#   define TimerHandlerH


#   if defined(TimerHandlerC)
#      define EXTERN
#   else
#      define EXTERN extern
typedef void Event;
#   endif


/*
    Title      : TimerHandler
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Oct 23 17:24:49 1995
    Last edit  : Mon Oct 23 17:24:49 1995
    Description: provides a simple timer handler for the
                 Executive.  It also provides the Executive with a
                 basic round robin scheduler.
*/



#define TimerHandler_TicksPerSecond  100
    /* Number of ticks per second.         */



/*
   GetTicks - returns the number of ticks since boottime.
*/


EXTERN unsigned int TimerHandler_GetTicks (void);


/*
   Sleep - suspends the current process for a time, t.
           The time is measured in ticks.
*/

EXTERN void TimerHandler_Sleep (unsigned int t);


/*
   ArmEvent - initializes an event, e, to occur at time, t.
              The time, t, is measured in ticks.
              The event is NOT placed onto the event queue.
*/

EXTERN Event *TimerHandler_ArmEvent (unsigned int t);


/*
   WaitOn - places event, e, onto the event queue and then the calling
            process suspends. It is resumed up by either the event
            expiring or the event, e, being cancelled.
            TRUE is returned if the event was cancelled
            FALSE is returned if the event expires.
*/

EXTERN int TimerHandler_WaitOn (Event *e);


/*
   Cancel - cancels the event, e, on the event queue and makes
            the appropriate process runnable again.
            TRUE is returned if the event was cancelled and
            FALSE is returned is the event was not found or
                  no process was waiting on this event.
*/

EXTERN int TimerHandler_Cancel (Event *e);


/*
   ReArmEvent - removes an event, e, from the event queue. A new time
                is given to this event and it is then re-inserted onto the
                event queue in the correct place.
                TRUE is returned if this occurred
                FALSE is returned if the event was not found.
*/

EXTERN unsigned int TimerHandler_ReArmEvent (Event *e, unsigned int t);


#undef EXTERN

#endif
