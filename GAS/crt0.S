#include "../C/linkage.h"
	.code32
/*
 * code must start here - as the processor jumps here via Second.mod
 */

.text
ENTRY(start)
ENTRY(_start)
        cli			/* disable interrupts                    */
	cld			/* clear direction flag                  */
	movl	$0x10, %eax	/* now set up segment registers          */
	mov	%ax, %ds
	mov	%ax, %es
	mov	%ax, %fs
	mov	%ax, %gs
	mov	%ax, %ss

#if 0
	movw	$0x1144, %ax   /* display a blue box to the CGA top left */
	movl    $0xb8000, %ebx
	movw	%ax, 0(%ebx)
#endif

/*
 *
 *  we put the stack at the top of extended memory minus a few bytes (16)
 *
 */
	xorl    %eax, %eax
        movw    640*1024-2, %ax     /* extended memory parameter        */
	sall    $10, %eax           /* multiply by 1024                 */
        addl    $(1024*1024), %eax  /* add the first 1 Meg onto this    */
	andl    $0xfffffff0, %eax
        movl	%eax, %esp          /* set stack to something sane      */
	movl	%eax, %ebp          /* and activation record            */
	pushl   $0                  /* parameters to main...            */
	pushl	$0
	pushl	$0
	/* call    _M2_hello_finish	   */
	call	SYMBOL_NAME(start_kernel)   /* now call main            */

ENTRY(fltused)
	jmp	SYMBOL_NAME(fltused)
ENTRY(libc_abort)
	jmp	SYMBOL_NAME(libc_abort)

ENTRY(crt0_red)
	movw	$0x4444, %ax   /* display a red box to the CGA top left */
	movw	%ax, 0xb8000
	ret

ENTRY(crt0_blue)
	movw	$0x1144, %ax   /* display a red box to the CGA top left */
	movw	%ax, 0xb8000
	ret

/*
 *  RunProgram - runs the program held at address, CodeStart, length, Length.
 *               It moves the program to, PositionOfStart, and runs it from
 *               there. It also blows away any previous program
 *               (ie this program).
 *
 *
 * PROCEDURE RunProgram (CodeStart       : ADDRESS;
 *                       LengthOfTextData: CARDINAL;
 *                       LengthOfBss     : CARDINAL;
 *                       PositionOfStart : ADDRESS) ;
 *
 *
 */




/*
 *  This procedure never returns.
 *  It essentially pretends that the code at: CodeStart..CodeStart+Length
 *  was loaded from the floppydisk and proceeds to run it.
 *
 *  To do this we must:
 *
 *  (i)    reset the original parameters passed by second.mod
 *  (ii)   move new code to correct position
 *           (a)  this involves copying a copy routine to a "safe area"
 *           (b)  jumping to the new copy routine
 *  (iii)  copy the downloaded code to the correct position
 *  (vi)   jump to the new code
 *
 *        remember that the parameters to the realtime system
 *        are located at 0x90000..0x900012
 *        see osinit.def osinit.mod and OSParameters.def
 *        for details.
 *
 *        Note that these values must correspond with the above
 *        Modula-2 source.
 *
 *        We move our stack to a known position, reset the parameters
 *        (which are initially set by second.mod) and jump to the start of our
 *        downloaded code.
 *
 *        The start of the downloaded code will retrieve these parameters
 *        and move itself to the start location
 *        exactly the same as if it were executed after loaded from floppydisk.
 *
 */

ENTRY(crt0_RunProgram)
ENTRY(crt0_RunProgram_Start)
         cli                         /* disable interrupts                  */
         cld                         /* clear direction flag                */
         pushl  %ebp
         movl   %esp, %ebp           /* must create new frame so that we can obtain our parameters */

         call   SYMBOL_NAME(osinit_ResetParameters)            /*  (i)  */

         /*
          *  place the stack out of harms way
          */

         /*
          *  let %eax = TOP OF MEMORY
          */

         xorl    %eax, %eax
         movw    640*1024-2, %ax     /* extended memory parameter         */
         sall    $10, %eax           /* multiply by 1024                  */
         addl    $(1024*1024)-16, %eax
                                    /* add the first 1 Meg onto this      */

         subl    $ SYMBOL_NAME(crt0_RunProgram_End) - SYMBOL_NAME(crt0_RunProgram_Start), %eax
         movl    %eax, %esp
         pushl   %eax               /* leave four bytes free just in case */
         pushl   %eax

         /*
          *  let %ebx = CodeStart
          *  let %edx = LengthOfTextData
          *  let %ecx = LengthOfBss
          *  let %ebp = PositionOfStart
          */

         movl   8(%ebp), %ebx
         movl  12(%ebp), %edx
         movl  16(%ebp), %ecx
         pushl 20(%ebp)                 /* out of registers :-( */


         /*
          *  zero bss just in case the OS ever references an uninitialized
          *  variable
          */

         xorl  %eax, %eax
         movl  20(%ebp), %edi   # edi = start of text
         addl  %edx, %edi       # add length of text+data = start of bss
         cld
         rep
           stosb

         popl  %ebp             # ebp contains the start of text now
         popl  %eax

         /*
          *  now move the copy routine to top of memory (out of the way)
          */

         movl   $ SYMBOL_NAME(crt0_RunProgram_End) - SYMBOL_NAME(crt0_RunProgram_Start), %ecx
         movl   $ SYMBOL_NAME(crt0_RunProgram_Start), %esi
         movl   %eax, %edi
         rep
            movsb                  /* do copy */

         /*
          *  now the routine is safely copied to TOP OF MEMORY
          *  we jump to the copy of "next instruction"
          */

         addl   $ SYMBOL_NAME(crt0_RunProgram_DoCopy) - SYMBOL_NAME(crt0_RunProgram_Start), %eax
         jmp    *%eax

ENTRY(crt0_RunProgram_DoCopy)

         /*
          * when we arrive at this position - we will be executing code
          * at TOP OF MEMORY
          */

         cld
         movl   %ebx, %esi
         movl   %edx, %ecx
         movl   %ebp, %edi
         rep
            movsb

         /*
          *  all done, new code is in final position
          */

         jmp   *%ebp
ENTRY(crt0_RunProgram_End)


         /*
          *   _M2_crt0_init - and
	  *   _M2_crt0_finish - keep the Modula-2 linker happy
          */

ENTRY(_M2_crt0_init)
ENTRY(_M2_crt0_finish)
	ret
