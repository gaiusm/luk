#if !defined(ASCIIH)
#   define ASCIIH

/*
   Author    : Gaius Mulley
   Date      : 12/12/85
   LastEdit  : 15/10/86
   Desciption: Defines all ascii constants (as in man ASCII)
               Note that lf, eof and EOL are added
*/

/* All the above are in order */

#define ASCII_nul       '\0'
#define ASCII_soh       '\001'
#define ASCII_stx       '\002'
#define ASCII_etx       '\003'
#define ASCII_eot       '\004'
#define ASCII_enq       '\005'
#define ASCII_ack       '\006'
#define ASCII_bel       '\007'
#define ASCII_bs        '\b'
#define ASCII_ht        '\t'
#define ASCII_nl        '\n'
#define ASCII_vt        '\013'
#define ASCII_np        '\f'
#define ASCII_cr        '\015'
#define ASCII_so        '\016'
#define ASCII_si        '\017'
#define ASCII_dle       '\020'
#define ASCII_dc1       '\021'
#define ASCII_dc2       '\022'
#define ASCII_dc3       '\023'
#define ASCII_dc4       '\024'
#define ASCII_nak       '\025'
#define ASCII_syn       '\026'
#define ASCII_etb       '\027'
#define ASCII_can       '\030'
#define ASCII_em        '\031'
#define ASCII_sub       '\032'
#define ASCII_esc       '\033'
#define ASCII_fs        '\034'
#define ASCII_gs        '\035'
#define ASCII_rs        '\036'
#define ASCII_us        '\037'
#define ASCII_sp        ' '   /* All the above are in order */
#define ASCII_lf        ASCII_nl
#define ASCII_ff        ASCII_np
#define ASCII_eof       ASCII_eot
#define ASCII_tab       ASCII_ht
#define ASCII_del       '\177'
#define ASCII_EOL       ASCII_nl


#endif

