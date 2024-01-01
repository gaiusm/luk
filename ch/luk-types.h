#if !defined(LUK_TYPESH)
#    define LUK_TYPESH

/*
 *   luk-types - define some semi standard types and constants used in the microkernel.
 */

typedef unsigned char uchar;
typedef unsigned int boolean;

#define NULL (void *)0

#if !defined(TRUE)
#   define TRUE (1==1)
#endif

#if !defined(FALSE)
#   define FALSE (1==0)
#endif


#endif
