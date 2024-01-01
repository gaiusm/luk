/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/StrIO.mod" */


#include <p2c/p2c.h>


#define StrIOG
#include "GStrIO.h"


#ifndef ASCIIH
#include "GASCII.h"
#endif

#ifndef StdIOH
#include "GStdIO.h"
#endif


/* */
Static void WriteLn(void);

Static void ReadString(const int a_LOW, const int a_HIGH, Char *a);

Static void WriteString(const int a_LOW, const int a_HIGH, const Char *a);

Static void Erase(void);

Static void Echo(Char ch);

Static BOOLEAN AlphaNum(Char ch);


/*    */


Static BOOLEAN IsATTY;   /* Is default input from the keyboard? */


/*
   WriteLn - writes a carriage return and a newline
             character.
*/

Static void WriteLn(void)
{
  Echo(ASCII_cr);
  StdIO_Write(ASCII_lf);
}


/*
   ReadString - reads a sequence of characters into a string.
                Line editing accepts Del, Ctrl H, Ctrl W and
                Ctrl U.
*/

Static void ReadString(const int a_LOW, const int a_HIGH, Char *a)
{
  unsigned int n, high;
  Char ch;

  high = a_HIGH - a_LOW;
  n = 0;
  do {
    StdIO_Read(&ch);
    if ((ch == ASCII_del) || (ch == ASCII_bs)) {
      if (n == 0)
	StdIO_Write(ASCII_bel);
      else {
	Erase();
	n--;
      }
    } else if (ch == ASCII_nak) {
      while (n > 0) {
	Erase();
	n--;
      }
    } else if (ch == ASCII_etb) {
      if (n == 0)
	Echo(ASCII_bel);
      else if (AlphaNum(a[n - 1])) {
	do {
	  Erase();
	  n--;
	} while ((n != 0) && AlphaNum(a[n - 1]));
      } else {
	Erase();
	n--;
      }
    } else if (n <= high) {
      if (ch == ASCII_cr) {
	a[n] = ASCII_nul;
	n++;
      } else if (ch >= ' ') {
	Echo(ch);
	a[n] = ch;
	n++;
      } else if (ch == ASCII_eof) {
	a[n] = ch;
	n++;
	ch = ASCII_cr;
	if (n <= high)
	  a[n] = ASCII_nul;
      }
    } else if (ch != ASCII_cr)
      Echo(ASCII_bel);
  } while (ch != ASCII_cr);

  /* Ctrl U */
  /* Ctrl W */
}


/*
   WriteString - writes a string to the default output.
*/

Static void WriteString(const int a_LOW, const int a_HIGH, const Char *a)
{
  unsigned int n, high;

  high = a_HIGH - a_LOW;
  n = 0;
  while ((n <= high) && (a[n] != ASCII_nul)) {
    if (a[n] == '\\') {
      if (n + 1 <= high) {
	if (a[n + 1] == 'n') {
	  WriteLn();
	  n++;
	} else if (a[n + 1] == '\\') {
	  StdIO_Write('\\');
	  n++;
	}
      }
    } else
      StdIO_Write(a[n]);
    n++;
  }
}


/*
   Erase - writes a backspace, space and backspace to remove the
           last character displayed.
*/

Static void Erase(void)
{
  Echo(ASCII_bs);
  Echo(' ');
  Echo(ASCII_bs);
}


/*
   Echo - echos the character, ch, onto the output channel if IsATTY
          is true.
*/

Static void Echo(Char ch)
{
  if (IsATTY)
    StdIO_Write(ch);
}


/*
   AlphaNum- returns true if character, ch, is an alphanumeric character.
*/

Static BOOLEAN AlphaNum(Char ch)
{
  return ISALNUM(ch);
}


void _M2_StrIO_init(void)
{
  static int _was_initialized = 0;
  if (_was_initialized++)
    return;
  IsATTY = TRUE;
}
void _M2_StrIO_fini(void);

void _M2_StrIO_fini(void)
{
}


/* End. */
