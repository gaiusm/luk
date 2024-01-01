/*
 *   ld-script a script which is passed to GNU ld to produce the third
 *             stage of the microkernel image.
 */

MEMORY {
  /* can only use the first 512K of base memory for code + data + bss */
  base (rwx) : ORIGIN = 0x00000000, LENGTH = 512*1024
}

SECTIONS { 
  .text 0x00000000 : { *(.text) } >base      /* code starts at 0x00000000   */
  _etext = .;
  .rodata  : {
  _sdata = .;
 *(.data) } >base
  .data ALIGN(16) : { *(.data) } >base       /* align on a click (16 bytes) */
  _edata = . ;
  .bss :  { *(.bss)  *(COMMON) } >base
  _end = . ;
} 


