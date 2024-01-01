
/*
 *
 * this file is based on the linux kernel linkage.h
 *
 */

#if defined(UseUnderscoreForC)

    /* looks like non __ELF__ */

#   undef __ELF__
#   define SYMBOL_NAME_STR(X) "_"#X
#   if defined(__STDC__)
#      define SYMBOL_NAME(X) _##X
#      define SYMBOL_NAME_LABEL(X) _##X##:
#   else
#      define SYMBOL_NAME(X) _/**/X
#      define SYMBOL_NAME_LABEL(X) _/**/X/**/:
#   endif
#else

    /* looks like __ELF__ to me */

#   define SYMBOL_NAME_STR(X) #X
#   define SYMBOL_NAME(X) X
#   if defined(__STDC__)
#      define SYMBOL_NAME_LABEL(X) X##:
#   else
#      define SYMBOL_NAME_LABEL(X) X/**/:
#   endif
#endif

#if defined(__ELF__)
#   define __ALIGN .align 16,0x90
#   define __ALIGN_STR ".align 16,0x90"
#else  /* __ELF__ */
#   define __ALIGN .align 4,0x90
#   define __ALIGN_STR ".align 4,0x90"
#endif /* __ELF__ */

/*
 *
 * if __ASSEMBLY__ is specified then we need to define ENTRY
 *
 */

#if defined(__ASSEMBLY__)

#   define ALIGN __ALIGN
#   define ALIGN_STRING __ALIGN_STRING

#   define ENTRY(name) \
  .globl SYMBOL_NAME(name); \
  ALIGN; \
  SYMBOL_NAME_LABEL(name)

#endif

