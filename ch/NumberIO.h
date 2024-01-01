#if !defined(NumberIOH)
#   define NumberIOH

/*
   Author     : Gaius Mulley
   Date       : '84
   LastEdit   : Thu Nov 23 19:01:25 GMT 2000
   Description: Provides all the input/output of numbers, and also the conversion
                of numbers to strings and visa versa.
*/

extern void NumberIO_ReadCard (unsigned int *x);

extern void NumberIO_WriteCard (unsigned int x, unsigned int n);

extern void NumberIO_ReadHex (unsigned int *x);

extern void NumberIO_WriteHex (unsigned int x, unsigned int n);

extern void NumberIO_ReadInt (int *x);

extern void NumberIO_WriteInt (int x, unsigned int n);

extern void NumberIO_CardToStr (unsigned int x, unsigned int n,
				char *a, const int a_HIGH);

extern void NumberIO_StrToCard (const char *a, const int a_HIGH, unsigned int *x);

extern void NumberIO_HexToStr (unsigned int x, unsigned int n,
			       char *a, const int a_HIGH);

extern void NumberIO_StrToHex (const char *a, const int a_HIGH, unsigned int *x);

extern void NumberIO_IntToStr (int x, unsigned int n, char *a, const int a_HIGH);

extern void NumberIO_StrToInt (const char *a, const int a_HIGH, int *x);

extern void NumberIO_ReadOct (unsigned int *x);

extern void NumberIO_WriteOct (unsigned int x, unsigned int n);

extern void NumberIO_OctToStr (unsigned int x, unsigned int n, char *a, const int a_HIGH);

extern void NumberIO_StrToOct (const char *a, const int a_HIGH, unsigned int *x);

extern void NumberIO_ReadBin (unsigned int *x);

extern void NumberIO_WriteBin (unsigned int x, unsigned int n);

extern void NumberIO_BinToStr (unsigned int x, unsigned int n, char *a, const int a_HIGH);

extern void NumberIO_StrToBin (char *a, const int a_HIGH, unsigned int *x);

extern void NumberIO_StrToBinInt (char *a, const int a_HIGH, int *x);

extern void NumberIO_StrToHexInt (char *a, const int a_HIGH, int *x);

extern void NumberIO_StrToOctInt (char *a, const int a_HIGH, int *x);

#endif
