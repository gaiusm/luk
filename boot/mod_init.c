main(argc, argv)
int   argc ;
char  *argv[];
{
   _M2_M2RTS_init(argc, argv) ;
   _M2_ASCII_init(argc, argv) ;
   _M2_SYSTEM_init(argc, argv) ;
   _M2_Util_init(argc, argv) ;
   _M2_libc_init(argc, argv) ;
   _M2_StrLib_init(argc, argv) ;
   _M2_IO_init(argc, argv) ;
   _M2_StdIO_init(argc, argv) ;
   _M2_testread_init(argc, argv) ;
   exit(0);
}
