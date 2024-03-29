/* Header for module libg, generated by p2c */
#ifndef libgH
#define libgH


#ifdef libgG
# define vextern
#else
# define vextern extern
#endif

/*
    Title      : libg
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Sat Aug  5 20:14:54 1995
    Last edit  : Sat Aug  5 20:14:54 1995
    Description: provides very simple mechanism to communicate with GDB.
*/



/*
   Init - initializes the libg module.
*/

extern void libg_Init(void);


extern void libg_Read(Char *ch);
extern void libg_Write(Char ch);


#undef vextern

#endif /*libgH*/

/* End. */
