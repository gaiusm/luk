#if !defined(ClockDeviceH)
#  define ClockDeviceH

/*
    Title      : ClockDevice
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Thu Aug 11 15:17:12 1994
    Last edit  : Thu Aug 11 15:17:12 1994
    Description: provides a simple interface to the 8253 timer chip
                 found on the IBM-PC.  It only provides access to counter0
                 in mode 0. Basically just enough for the TimerHandler.
*/


/*
 *   LoadClock - returns the clock device current count.
 */

unsigned int ClockDevice_LoadClock (void);


/*
 *  StartClock - sets the Count into clock.
 */

void ClockDevice_StartClock (unsigned int count);


#endif
