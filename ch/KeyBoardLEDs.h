#if !defined(KeyBoardLEDsH)
#   define KeyBoardLEDsH

/*
    Title      : KeyBoardLEDs
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Fri Aug 12 17:05:58 1994
    Last edit  : Fri Aug 12 17:05:58 1994
    Description: provides a simple packages to manipulate the keyboard
                 LEDs.
*/


/*
   SwitchLeds - switch the keyboard LEDs to the state defined
                by the BOOLEAN variables. TRUE = ON.
*/

extern void KeyBoardLEDs_SwitchLeds (BOOLEAN NumLock,
				     BOOLEAN CapsLock,
				     BOOLEAN ScrollLock);


/*
   SwitchScroll - switchs the scroll LED on or off.
*/

extern void KeyBoardLEDs_SwitchScroll (BOOLEAN Scroll);


/*
   SwitchNum - switches the Num LED on or off.
*/

extern void KeyBoardLEDs_SwitchNum (BOOLEAN Num);


/*
   SwitchCaps - switches the Caps LED on or off.
*/

extern void KeyBoardLEDs_SwitchCaps (BOOLEAN Caps);


#endif

