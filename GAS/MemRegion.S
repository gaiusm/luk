#include "../C/linkage.h"

         /*
          *  returns the end of the OS
          */

.text
ENTRY(MemRegion_EndOfOS)
         movl   _end, %eax
         ret

         /*
          *   _M2_MemRegion_init - and
          *   _M2_MemRegion_finish - keep the Modula-2 linker happy
          */

ENTRY(_M2_MemRegion_init)
ENTRY(_M2_MemRegion_finish)
         ret
