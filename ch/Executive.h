
#if !defined(ExecutiveH)
#   define ExecutiveH


/*
    Title      : Executive
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Thu Aug 18 10:34:58 1994
    Last edit  : Thu Aug 18 10:34:58 1994
    Description: provides a simple multitasking executive.
*/



#if !defined(ExecutiveC)
   /* opaque type declaration */
typedef void Semaphore;      /* defines dijkstra's semaphores */
typedef void Descriptor;     /* handle onto a process         */
#endif


/*
   InitProcess - initializes a process which is held in the suspended
                 state. When the process is resumed it will start executing
                 procedure, p. The process has a maximum stack size of,
                 StackSize, bytes and its textual name is, Name.
                 The StackSize should be at least 5000 bytes.
*/


extern Descriptor *Executive_InitProcess (void (*p)(void), unsigned int StackSize,
					  char *Name, unsigned int Name_High);


/*
   Resume - resumes a suspended process. If all is successful then the process, p,
            is returned. If it fails then NIL is returned.
*/

extern Descriptor *Executive_Resume (Descriptor *d);


/*
   Suspend - suspend the calling process.
             The process can only continue running if another process
             Resumes it.
*/

extern void Executive_Suspend (void);


/*
   InitSemaphore - creates a semaphore whose initial value is, v, and
                   whose name is, Name.
*/

extern Semaphore *Executive_InitSemaphore (unsigned int v,
					   char *Name, unsigned int Name_high);


/*
   Wait - performs dijkstra's P operation on a semaphore.
          A process which calls this procedure will
          wait until the value of the semaphore is > 0
          and then it will decrement this value.
*/

extern void Executive_Wait (Semaphore *s);


/*
   Signal - performs dijkstra's V operation on a semaphore.
            A process which calls the procedure will increment
            the semaphores value.
*/

extern void Executive_Signal (Semaphore *s);


/*
   WaitForIO - waits for an interrupt to occur on vector, VectorNo.
*/

extern void Executive_WaitForIO (unsigned int VectorNo);


/*
   Ps - displays a process list together with process status.
*/

extern void Executive_Ps (void);


/*
   GetCurrentProcess - returns the descriptor of the current running
                       process.
*/

extern Descriptor *Executive_GetCurrentProcess (void);


/*
   RotateRunQueue - rotates the process run queue.
                    It does not call the scheduler.
*/

extern void Executive_RotateRunQueue (void);


/*
   ProcessName - displays the name of process, d, through
                 DebugString.
*/

extern void Executive_ProcessName (Descriptor *d);


/*
   DebugProcess - gdb debug handle to enable users to debug deadlocked
                  semaphore processes.
*/

extern void Executive_DebugProcess (Descriptor *d);


#endif

