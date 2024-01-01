
#if !defined(StrLibH)
#define StrLibH

/*
   Author     : Gaius Mulley
   Date       : '84
   LastEdit   : 28/1/97
   Description: Provides string manipulation
*/



/*
   StrConCat - combines a and b into c.
*/

extern void StrLib_StrConCat (char *a, const int a_HIGH,
			      char *b, const int b_HIGH,
			      char *c, const int c_HIGH);


/*
   StrLess - returns TRUE if string, a, alphabetically occurs before
             string, b.
*/

extern boolean StrLib_StrLess (char *a, const int a_HIGH,
			       char *b, const int b_HIGH);


/*
   StrEqual - performs a = b on two strings.
*/

extern boolean StrLib_StrEqual (char *a, const int a_HIGH,
				char *b, const int b_HIGH);


/*
   StrLen - returns the length of string, a.
*/

extern unsigned int StrLib_StrLen (char *a, const int a_HIGH);


/*
   StrCopy - effectively performs b := a with two strings.
*/

extern void StrLib_StrCopy (char *a, const int a_HIGH,
			    char *b, const int b_HIGH);


/*
   IsSubString - returns true if b is a subcomponent of a.
*/

extern boolean StrLib_IsSubString (char *a, const int a_HIGH,
				   char *b, const int b_HIGH);


/*
   StrRemoveWhitePrefix - copies string, into string, b, excluding any white
                          space infront of a.
*/

extern void StrLib_StrRemoveWhitePrefix (char *a, const int a_HIGH,
					 char *b, const int b_HIGH);


#endif

