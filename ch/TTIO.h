
#if !defined(TTIOH)
#   define TTIOH


/*
    Title      : TTIO
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Thu Aug 25 13:25:50 1994
    Last edit  : Thu Aug 25 13:25:50 1994
    Description: provides a buffered Read from an interrupt
                 driven keyboard driver.
*/



extern void TTIO_Read (char *ch);


/*
   SwitchLeds - switch the keyboard LEDs to the state defined
                by the BOOLEAN variables. TRUE = ON.
*/

extern void TTIO_SwitchLeds (BOOLEAN NumLock, BOOLEAN CapsLock,
			     BOOLEAN ScrollLock);


/*
   SwitchScroll - switchs the scroll LED on or off.
*/

extern void TTIO_SwitchScroll (BOOLEAN Scroll);


/*
   SwitchNum - switches the Num LED on or off.
*/

extern void TTIO_SwitchNum (BOOLEAN Num);


/*
   SwitchCaps - switches the Caps LED on or off.
*/

extern void TTIO_SwitchCaps (BOOLEAN Caps);


#endif
