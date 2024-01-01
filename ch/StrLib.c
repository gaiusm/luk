/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/StrLib.mod" */


/* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
                 2010
                 Free Software Foundation, Inc. */
/* This file is part of GNU Modula-2.

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA */


#include <p2c/p2c.h>


#define StrLibG
#include "GStrLib.h"

#ifndef ASCIIH
#include "GASCII.h"
#endif

/* */
Static BOOLEAN StrEqual(const int a_LOW, const int a_HIGH, const Char *a,
			const int b_LOW, const int b_HIGH, const Char *b);

Static unsigned int StrLen(const int a_LOW, const int a_HIGH, const Char *a);

Static void StrCopy(const int a_LOW, const int a_HIGH, const Char *a,
		    const int b_LOW, const int b_HIGH, Char *b);

Static void StrConCat(const int a_LOW, const int a_HIGH, const Char *a,
		      const int b_LOW, const int b_HIGH, const Char *b,
		      const int c_LOW, const int c_HIGH, Char *c);

Static BOOLEAN IsSubString(const int a_LOW, const int a_HIGH, const Char *a,
			   const int b_LOW, const int b_HIGH, const Char *b);


/*    */


/*
   StrConCat - combines a and b into c.
*/

Static void StrConCat(const int a_LOW, const int a_HIGH, const Char *a,
		      const int b_LOW, const int b_HIGH, const Char *b,
		      const int c_LOW, const int c_HIGH, Char *c)
{
  unsigned int Highb, Highc, i, j;

  Highb = StrLen(0L, b_HIGH - b_LOW, b);
  Highc = c_HIGH - c_LOW;
  StrCopy(0L, a_HIGH - a_LOW, a, 0L, c_HIGH - c_LOW, c);
  i = StrLen(0L, c_HIGH - c_LOW, c);
  j = 0;
  while ((j < Highb) && (i <= Highc)) {
    c[i] = b[j];
    i++;
    j++;
  }
  if (i <= Highc)
    c[i] = ASCII_nul;
}


/*
   StrLess - returns TRUE if string, a, alphabetically occurs before
             string, b.
*/

Static BOOLEAN StrLess(const int a_LOW, const int a_HIGH, const Char *a,
		       const int b_LOW, const int b_HIGH, const Char *b)
{
  unsigned int Higha, Highb, i;

  Higha = StrLen(0L, a_HIGH - a_LOW, a);
  Highb = StrLen(0L, b_HIGH - b_LOW, b);
  i = 0;
  while ((i < Higha) && (i < Highb)) {
    if (a[i] < b[i])
      return TRUE;
    /* must be equal, move on to next character */
    if (a[i] > b[i])
      return FALSE;
    i++;
  }
  return (Higha < Highb);   /* substrings are equal so we go on length */
}


Static BOOLEAN StrEqual(const int a_LOW, const int a_HIGH, const Char *a,
			const int b_LOW, const int b_HIGH, const Char *b)
{
  unsigned int i, higha, highb;

  higha = a_HIGH - a_LOW;
  highb = b_HIGH - b_LOW;
  i = 0;
  while ((i <= higha) && (i <= highb) && (a[i] != ASCII_nul) &&
	 (b[i] != ASCII_nul)) {
    if (a[i] != b[i])
      return FALSE;
    i++;
  }
  return (((i > higha) || (a[i] == ASCII_nul)) &&
	  ((i > highb) || (b[i] == ASCII_nul)));
}


Static unsigned int StrLen(const int a_LOW, const int a_HIGH, const Char *a)
{
  unsigned int High, Len;

  Len = 0;
  High = a_HIGH - a_LOW;
  while ((Len <= High) && (a[Len] != ASCII_nul))
    Len++;
  return Len;
}


Static void StrCopy(const int a_LOW, const int a_HIGH, const Char *a,
		    const int b_LOW, const int b_HIGH, Char *b)
{
  unsigned int Higha, Highb, n;

  n = 0;
  Higha = StrLen(0L, a_HIGH - a_LOW, a);
  Highb = b_HIGH - b_LOW;
  while ((n < Higha) && (n <= Highb)) {
    b[n] = a[n];
    n++;
  }
  if (n <= Highb)
    b[n] = ASCII_nul;
}


/*
   IsSubString - returns true if b is a subcomponent of a.
*/

Static BOOLEAN IsSubString(const int a_LOW, const int a_HIGH, const Char *a,
			   const int b_LOW, const int b_HIGH, const Char *b)
{
  unsigned int i, j, LengthA, LengthB;

  LengthA = StrLen(0L, a_HIGH - a_LOW, a);
  LengthB = StrLen(0L, b_HIGH - b_LOW, b);
  i = 0;
  if (LengthA <= LengthB)
    return FALSE;
  while (i <= (LengthA - LengthB)) {
    j = 0;
    while ((j < LengthB) && (a[i + j] == b[j]))
      j++;
    if (j == LengthB)
      return TRUE;
    i++;
  }
  return FALSE;
}


/*
   IsWhite - returns TRUE if, ch, is a space or a tab.
*/

Static BOOLEAN IsWhite(Char ch)
{
  return ((ch == ' ') || (ch == ASCII_tab));
}


/*
   StrRemoveWhitePrefix - copies string, into string, b, excluding any white
                          space infront of a.
*/

Static void StrRemoveWhitePrefix(const int a_LOW, const int a_HIGH,
  const Char *a, const int b_LOW, const int b_HIGH, Char *b)
{
  unsigned int i, j, higha, highb;

  i = 0;
  j = 0;
  higha = StrLen(0L, a_HIGH - a_LOW, a);
  highb = b_HIGH - b_LOW;
  while ((i < higha) && IsWhite(a[i]))
    i++;
  while ((i < higha) && (j <= highb)) {
    b[j] = a[i];
    i++;
    j++;
  }
  if (j <= highb)
    b[j] = ASCII_nul;
}


void _M2_StrLib_init(void);

void _M2_StrLib_init(void)
{
}
void _M2_StrLib_fini(void);

void _M2_StrLib_fini(void)
{
}


/* End. */
