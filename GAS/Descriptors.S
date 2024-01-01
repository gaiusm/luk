#include "../C/linkage.h"

.text

ENTRY(_M2_Descriptors_init)     /* these dummy functions allow us to    */
ENTRY(_M2_Descriptors_finish)   /* use the modula-2 linker.  These are  */
	ret                     /* similar to a constructor and         */
	                        /* deconstructor pair.                  */

ENTRY(Descriptors_SetupGDT)
	lgdt gdt_descr
	lidt idt_descr
	ljmp $0x08,$1f
1:	movl $0x10,%eax		/* reload all the segment registers      */
	mov %ax, %ds		/* after changing gdt.                   */
	mov %ax, %es
	mov %ax, %fs
	mov %ax, %gs
	mov %ax, %ss            /* this may seem dangerous but we        */
                                /* ALWAYS have a flat model so it        */
                                /* should make NO difference whatsoever! */
	ret                     /* Now put theory into practice          */

/*
 *  setup_idt
 *
 *  sets up a idt with 256 entries pointing to
 *  ignore_int, interrupt gates. It doesn't actually load
 *  idt.
 *  Interrupts are enabled elsewhere, when we can be relatively
 *  sure everything is ok.
 */

ENTRY(Descriptors_SetupIDT)
	lea ignore_int,%edx
	movl $0x00080000,%eax
	movw %dx,%ax		/* selector = 0x0008 = cs                */
	movw $0x8E00,%dx	/* interrupt gate - dpl=0, present       */

	lea SYMBOL_NAME(Descriptors_IDT),%edi
	mov $256,%ecx
rp_sidt:
	movl %eax,(%edi)
	movl %edx,4(%edi)
	addl $8,%edi
	dec %ecx
	jne rp_sidt
	ret


/*
 * The interrupt descriptor table has room for 256 idt's
 */

.align 4
.word 0
idt_descr:
	.word 256*8-1		/* idt contains 256 entries */
	.long 0x00000000+SYMBOL_NAME(Descriptors_IDT)

.align 4
ENTRY(Descriptors_IDT)
	.fill 256,8,0		/* idt is uninitialized     */

/*
 * The real GDT is also 256 entries long - no real reason
 */

.align 4
.word 0
gdt_descr:
	.word (4+2*64)*8-1
	.long 0x00000000+_gdt           /* physical address of _gdt         */
                                        /* if we move the kernel we also    */ 
                                        /* need to alter this base address  */

/*
 * This gdt setup gives the kernel a 1GB address space at virtual
 * address 0x00000000 - space enough for expansion, I hope.
 */

.align 4
_gdt:
	.quad 0x0000000000000000	/* NULL descriptor                  */
	.quad 0x00c09a000000ffff	/* 1GB at 0x00000000                */
	.quad 0x00c092000000ffff	/* 1GB                              */
	.quad 0x0000000000000000	/* TEMPORARY - don't use            */
	.fill 2*64,8,0 			/* space for LDT's and TSS's etc    */


/*
 * this procedure is part of the reinitialization sequence which MUST be after
 * the GDT and IDT tables. Reinitialization occurs after we have downloaded
 * some new code, which we assume has exactly the same crt0.S and
 * Descriptors.S EXCEPT the _start_kernel value will be different.
 */

ENTRY(Descriptors_AfterIDTGDT)

/*
 *      just testing
 *      pushl $0x41414141
 *      call SYMBOL_NAME(Scn_Write)
 */

       /* we got to here as well safely */

/*
 *
 *  we put the stack at the top of extended memory minus a few bytes (16)
 *
 */
	xorl    %eax, %eax
        movw    640*1024-2, %ax     /* extended memory parameter        */
	sall    $10, %eax           /* multiply by 1024                 */
        addl    $(1024*1024)-16, %eax
                                    /* add the first 1 Meg onto this    */
        movl	%eax, %esp          /* set stack to something sane      */
	movl	%eax, %ebp          /* and activation record            */
	pushl   $0                  /* parameters to main...            */
	pushl	$0
	pushl	$0

#if 0
	movl	$0x4444, %eax   /* display a red box to the CGA top left */
	movw	%ax, 0xb8000    
#endif

/*
 * the value of start_kernel might be different upon each load
 */
	call	SYMBOL_NAME(start_kernel)   /*  now call main */



/*
 * the default interrupt "handler" :-) (borrowed from linux)
 */

undefint_msg:
	.asciz "undefined interrupt occurred\n"
.align 2
ignore_int:
	cld
	pushl %eax
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%eax
	mov %ax,%ds
	mov %ax,%es
	mov %ax,%fs
	pushl $22                  /* HIGH(undefint_msg)                 */
	pushl $undefint_msg        /* ADR(undefint_msg)                  */
	call SYMBOL_NAME(StrIO_WriteString)
/*
 *      just testing
 *
 *	pushl $0x41414141
 *	call SYMBOL_NAME(Scn_Write)
 */
	popl %eax
	popl %eax                  /* remove the 2 parameters            */
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret
