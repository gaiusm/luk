#include <stdio.h>
#include <bfd.h>

#if !defined(TRUE)
#    define TRUE (1==1)
#endif

#if !defined(FALSE)
#    define FALSE (1==1)
#endif

#define DEBUG
#define AOUTDEBUG  TRUE


#define LINETABLE(symtab)	(symtab)->linetable

typedef bfd_vma CORE_ADDR;

static char *target = NULL;

/* The symbol table.  */
asymbol **syms;

/* Number of symbols in `syms'.  */
long symcount = 0;

/* The sorted symbol table.  */
asymbol **sorted_syms;

/* Number of symbols in `sorted_syms'.  */
long sorted_symcount = 0;

/* The dynamic symbol table.  */
asymbol **dynsyms;

/* Number of symbols in `dynsyms'.  */
long dynsymcount = 0;

static int debug=TRUE;

static int discard_underscores = 1;


static void getsymtab (bfd *abfd, char *a_outname);
static asymbol **slurp_symtab (bfd *abfd) ;
static void print_symbols (bfd *abfd, asymbol **syms, unsigned long symcount);

struct symtab_and_line
{
  struct symtab *symtab;

  /* Line number.  Line numbers start at 1 and proceed through symtab->nlines.
     0 is never a valid line number; it is used to indicate that line number
     information is not available.  */
  int line;

  CORE_ADDR pc;
  CORE_ADDR end;
};


/* Each item represents a line-->pc (or the reverse) mapping.  This is
   somewhat more wasteful of space than one might wish, but since only
   the files which are actually debugged are read in to core, we don't
   waste much space.  */

struct linetable_entry
{
  int line;
  CORE_ADDR pc;
};

/* The order of entries in the linetable is significant.  They should
   be sorted by increasing values of the pc field.  If there is more than
   one entry for a given pc, then I'm not sure what should happen (and
   I not sure whether we currently handle it the best way).

   Example: a C for statement generally looks like this

   	10	0x100	- for the init/test part of a for stmt.
   	20	0x200
   	30	0x300
   	10	0x400	- for the increment part of a for stmt.

   */

struct linetable
{
  int nitems;

  /* Actually NITEMS elements.  If you don't like this use of the
     `struct hack', you can shove it up your ANSI (seriously, if the
     committee tells us how to do it, we can probably go along).  */
  struct linetable_entry item[1];
};

static asymbol **symbol_table=NULL;     /* the symbol table */


/* void resolve_sal_pc (struct symtab_and_line *sal); */

static void *xmalloc (long size)
{
  void *p=(void *)malloc(size);

  if (p == NULL) {
    fprintf(stderr, "vm exhausted\n");
  } else {
    return( (void *)malloc(size) );
  }
}


static void bfd_nonfatal (char *filename)
{
  CONST char *errmsg = bfd_errmsg (bfd_get_error ());

  if (filename) {
    fprintf (stderr, "%s: %s\n", filename, errmsg);
  } else {
    fprintf (stderr, "%s\n", errmsg);
  }
}


static void bfd_fatal (char *filename)
{
  CONST char *errmsg = bfd_errmsg (bfd_get_error ());

  if (filename) {
    fprintf (stderr, "%s: %s\n", filename, errmsg);
  } else {
    fprintf (stderr, "%s\n", errmsg);
  }
  exit(1);
}


/* After a false return from bfd_check_format_matches with
   bfd_get_error () == bfd_error_file_ambiguously_recognized, print the possible
   matching targets.  */

void list_matching_formats (char **p)
{
  fprintf(stderr, "matching formats: ");
  while (*p != NULL) {
    fprintf(stderr, " %s", *p);
    *p++;
  }
  fprintf(stderr, "\n");
}


static void display_bfd (bfd *abfd)
{
  char **matching;
  
  if (!bfd_check_format_matches (abfd, bfd_object, &matching)) {
    bfd_nonfatal (bfd_get_filename (abfd));
    if (bfd_get_error () == bfd_error_file_ambiguously_recognized) {
      list_matching_formats (matching);
      free (matching);
    }
    return;
  }

  printf ("\n%s:     file format %s\n", bfd_get_filename (abfd),
	  abfd->xvec->name);
}


static void print_sections (bfd *abfd, asymbol **symbol_table)
{
  const char *filename;
  const char *functionname;
  unsigned int line;
  asection *section;
  int i, size;

  for (section = abfd->sections;
       section != (asection *) NULL;
       section = section->next) {
    printf("section %s\n", bfd_get_section_name(abfd, section));
    size = bfd_section_size(abfd, section);
    for (i=0; i<size; i++) {
      filename     = NULL;
      functionname = NULL;
      if (bfd_find_nearest_line (abfd,
				 section,
				 symbol_table,
				 bfd_section_vma(abfd, section) + i,
				 &filename,
				 &functionname,
				 &line)) {
	printf("%s:%s:%d at location %d\n",
	       filename, functionname, line, bfd_section_vma(abfd, section)+i);
      }
    }
  }
}


#if 0

/* Find line number LINE in any symtab whose name is the same as
   SYMTAB.

   If found, return 1, set *LINETABLE to the linetable in which it was
   found, set *INDEX to the index in the linetable of the best entry
   found, and set *EXACT_MATCH nonzero if the value returned is an
   exact match.

   If not found, return 0.  */

static int find_line_symtab (struct symtab *symtab, int line,
			     struct linetable **linetable, int *index, int *exact_match)
{
  int exact;

  /* BEST_INDEX and BEST_LINETABLE identify the smallest linenumber > LINE
     so far seen.  */
  
  int best_index;
  struct linetable *best_linetable;

  /* First try looking it up in the given symtab.  */
  best_linetable = LINETABLE (symtab);
  best_index = find_line_common (best_linetable, line, &exact);
  if (best_index < 0 || !exact)
    {
      /* Didn't find an exact match.  So we better keep looking for
	 another symtab with the same name.  In the case of xcoff,
	 multiple csects for one source file (produced by IBM's FORTRAN
	 compiler) produce multiple symtabs (this is unavoidable
	 assuming csects can be at arbitrary places in memory and that
	 the GLOBAL_BLOCK of a symtab has a begin and end address).  */

      /* BEST is the smallest linenumber > LINE so far seen,
	 or 0 if none has been seen so far.
	 BEST_INDEX and BEST_LINETABLE identify the item for it.  */
      int best;

      struct objfile *objfile;
      struct symtab *s;

      if (best_index >= 0)
	best = best_linetable->item[best_index].line;
      else
	best = 0;

      ALL_SYMTABS (objfile, s)
	{
	  struct linetable *l;
	  int ind;

	  if (!STREQ (symtab->filename, s->filename))
	    continue;
	  l = LINETABLE (s);
	  ind = find_line_common (l, line, &exact);
	  if (ind >= 0)
	    {
	      if (exact)
		{
		  best_index = ind;
		  best_linetable = l;
		  goto done;
		}
	      if (best == 0 || l->item[ind].line < best)
		{
		  best = l->item[ind].line;
		  best_index = ind;
		  best_linetable = l;
		}
	    }
	}
    }
 done:
  if (best_index < 0)
    return 0;

  if (index)
    *index = best_index;
  if (linetable)
    *linetable = best_linetable;
  if (exact_match)
    *exact_match = exact;
  return 1;
}

/* Find the PC value for a given source file and line number.
   Returns zero for invalid line number.
   The source file is specified with a struct symtab.  */

CORE_ADDR find_line_pc (struct symtab *symtab, int line)
{

}


/* Helper function for break_command_1 and disassemble_command.  */

void resolve_sal_pc (struct symtab_and_line *sal)
{
  CORE_ADDR pc;

  if (sal->pc == 0 && sal->symtab != 0) {
    pc = find_line_pc (sal->symtab, sal->line);
    sal->pc = pc;
  }
}


static void print_line_numbers (bfd *abfd, asymbol **symbol_table)
{
  struct symtab_and_line sal;
  int i;
  
  for (i=1; i<20; i++) {
    sal.symtab = symbol_table;
    sal.line   = i;
    sal.pc     = 0;
    resolve_sal_pc (&sal);			/* Might error out */
    printf("line %d is at 0x%x\n", i, sal.pc);
  }
}
#endif

static void display_file (char *filename, char *target)
{
  bfd *abfd;

  abfd = bfd_openr (filename, target);
  if (abfd == NULL) {
    bfd_nonfatal (filename);
    return;
  }

  if (!bfd_check_format (abfd, bfd_object)) {
    fprintf (stderr, "%s: %s: bad format\n", __FILE__, filename);
  }

  /*  display_bfd (abfd); */
  symbol_table = slurp_symtab(abfd);
  print_symbols(abfd, symbol_table, symcount);
  /* print_sections(abfd, symbol_table); */
  /* print_line_numbers(abfd, symbol_table); */
  /* getsymtab(abfd, filename); */
  bfd_close (abfd);
}


/*
 *  funcsymbol - returns TRUE if symp is a function.
 */

static int funcsymbol (asymbol *symp, bfd *abfd)
{
  CONST char	*name;
  int i;
  char		symprefix;
  symbol_info	syminfo;

  /*
   *	must be a text symbol,
   *	and static text symbols don't qualify if aflag set.
   */

  if (!symp->section)
    return FALSE;

  symprefix = bfd_get_symbol_leading_char (abfd);
  bfd_get_symbol_info (abfd, symp, &syminfo);
  i = syminfo.type;
#if defined(DEBUG) && 0
  if (i != 'T' && i != 't')
    fprintf (stderr, "%s(%d):  %s is of class %c\n", __FILE__, __LINE__, symp->name, i);
#endif

  /*
   * Any external text symbol should be okay.  (Only problem would be
   * variables in the text section.)
   */

  if (i == 'T')
    return TRUE;

  /*
   * 't' is static text; -a says to ignore it.  So if it's not
   * a static text symbol, *or* it is and the user gave -a, we
   * ignore it.
   */


  /*
   *	can't have any `funny' characters in name,
   *	where `funny' includes	`.', .o file names
   *			and	`$', pascal labels.
   */
  if (!symp->name)
    return FALSE;

  for (name = symp->name; *name; name++) {
    if ( *name == '.' || *name == '$' ) {
      return FALSE;
    }
  }

  /* On systems where the C compiler adds an underscore to all names,
   * static names without underscores seem usually to be labels in
   * hand written assembler in the library.  We don't want these
   * names.  This is certainly necessary on a Sparc running SunOS 4.1
   * (try profiling a program that does a lot of division). I don't
   * know whether it has harmful side effects on other systems.
   * Perhaps it should be made configurable.
   */

  if (symprefix && symprefix != *symp->name
      /* Gcc may add special symbols to help gdb figure out the file
	 language.  We want to ignore these, since sometimes they
	 mask the real function.  (dj@ctron)  */
      || !strncmp (symp->name, "__gnu_compiled", 14)
      || !strncmp (symp->name, "___gnu_compiled", 15))
    return FALSE;

  return TRUE;
}


static asymbol **slurp_symtab (bfd *abfd)
{
  asymbol **sy = (asymbol **) NULL;
  long storage;

  if (!(bfd_get_file_flags (abfd) & HAS_SYMS))
    {
      printf ("no symbols in \"%s\".\n", bfd_get_filename (abfd));
      return NULL;
    }
#if 0
  if (!(bfd_get_file_flags (abfd) & HAS_DEBUG)) {
    printf ("hmm no debugging info in \"%s\".\n", bfd_get_filename (abfd));
  }
#endif
  storage = bfd_get_symtab_upper_bound (abfd);
  if (storage < 0)
    bfd_fatal (bfd_get_filename (abfd));

  if (storage)
    {
      sy = (asymbol **) xmalloc (storage);
    }
  symcount = bfd_canonicalize_symtab (abfd, sy);
  if (symcount < 0)
    bfd_fatal (bfd_get_filename (abfd));
  if (symcount == 0)
    fprintf (stderr, "%s: %s: No symbols\n",
	     __FILE__, bfd_get_filename (abfd));
  return sy;
}


static void getLimits (bfd *abfd, symbol_info *syminfo, asymbol *func, asymbol **syms,
		       unsigned long symcount,
		       unsigned int *low, unsigned int *high)
{
  asymbol **sym = syms, **end = syms + symcount;
  asection *section;
  symbol_info nextsyminfo;

  *low    = syminfo->value;
  section = func->section;
  *high   = bfd_section_size(abfd, section) + bfd_section_vma(abfd, section);  /* end of section */
  for (; sym < end; ++sym) {
    asymbol *p = *sym;
    if (p) {
      bfd_get_symbol_info (abfd, p, &nextsyminfo);
      if ((funcsymbol(p, abfd)) &&
	  ((*low)<nextsyminfo.value) &&
	  ((*high)>nextsyminfo.value-1)) {
	*high = nextsyminfo.value-1;
      }
    }
  }
}


static int isFuncSymbol (asymbol *sym, bfd *abfd, symbol_info *syminfo)
{
  return( (strcmp(syminfo->stab_name, "FUN")==0) );
}


static int isLineSymbol (asymbol *sym, bfd *abfd, symbol_info *syminfo)
{
  return( strcmp(syminfo->stab_name, "SLINE")==0 );
}


static void print_symbols (bfd *abfd, asymbol **syms, unsigned long symcount)
{
  asymbol **sym = syms, **end = syms + symcount;
  symbol_info syminfo;
  unsigned int low;
  unsigned int high;
  const char *functionname;
  const char *filename;
  int line;

  for (; sym < end; ++sym) {
    asymbol *p = *sym;
    if (p) {
      bfd_get_symbol_info (abfd, p, &syminfo);
#if 0
      printf ("%s  (type %d)  (stab name %s)  (value 0x%x)  (desc %d)", syminfo.name, syminfo.type,
	      syminfo.stab_name, syminfo.value, syminfo.stab_desc);
#endif
      if (bfd_find_nearest_line (abfd, bfd_get_section(p), sym,
				 bfd_asymbol_value(p)-bfd_asymbol_base(p), &filename,
				 &functionname, &line)) {
      }
      if ((functionname != NULL) && (syminfo.name != NULL) && (strlen(syminfo.name) > 0)) {
#if 0	
	printf("%s 0x%x (file %s function %s line %d) ",
	       syminfo.name, bfd_asymbol_value(p), filename, functionname, line);
#endif
	if (isFuncSymbol(p, abfd, &syminfo)) {
	  printf("%s 0x%x (file %s function %s line %d) ",
		 syminfo.name, bfd_asymbol_value(p), filename, functionname, line);
	  getLimits(abfd, &syminfo, p, syms, symcount, &low, &high);
	  printf("function start 0x%x and end 0x%x", low, high);
	  putchar ('\n');
	}
      }
#if 0
      if (isFuncSymbol(p, abfd, &syminfo)) {
	getLimits(abfd, &syminfo, p, syms, symcount, &low, &high);
	printf("function start 0x%x and end 0x%x\n", low, high);
      }
      if (isLineSymbol(p, abfd, &syminfo)) {
	printf("line number %d at 0x%x\n", syminfo.stab_desc, syminfo.value);
      }
#endif
    }
  }
}


main (int argc, char *argv[])
{
  if (argc == 2) {
    display_file(argv[1], target);
  }
  exit(0);
}


#if 0
		  CONST char *filename;
		  CONST char *functionname;
		  unsigned int line;

		  if (bfd_find_nearest_line (abfd,
					     section,
					     syms,
					     section->vma + i,
					     &filename,
					     &functionname,
					     &line))
		    {
		      if (functionname
			  && *functionname != '\0'
			  && (prev_function == NULL
			      || strcmp (functionname, prev_function) != 0))
			{
			  printf ("%s():\n", functionname);
			  if (prev_function != NULL)
			    free (prev_function);
			  prev_function = xmalloc (strlen (functionname) + 1);
			  strcpy (prev_function, functionname);
			}
		      if (!filename)
			filename = "???";
		      if (line && line != prevline)
			{
			  printf ("%s:%u\n", filename, line);
			  prevline = line;
			}
		    }
		}
#endif
/*  ../../../TSX11/binutils-2.5.2/opcodes/libopcodes.a  ../../../TSX11/binutils-2.5.2/bfd/libbfd.a ../../../TSX11/binutils-2.5.2/libiberty/libiberty.a -lm" */
/*  -I/usr/local/include -g stabs.c /usr/local/lib/libopcodes.a /usr/local/lib/libbfd.a /usr/local/lib/libiberty.a" */

/*
 * Local variables:
 * compile-command: "gcc -I../../../TSX11/smp-binutils-2.5.2/include -g stabs.c ../../../TSX11/smp-binutils-2.5.2/opcodes/libopcodes.a  ../../../TSX11/smp-binutils-2.5.2/bfd/libbfd.a ../../../TSX11/smp-binutils-2.5.2/libiberty/libiberty.a -lm"
 * End:
 */
