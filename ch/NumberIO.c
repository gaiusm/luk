/* Output from p2c, the Pascal-to-C translator */
/* From input file "../luk-1.0/mod/NumberIO.mod" */


/* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
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
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA */


#include <p2c/p2c.h>


#define NumberIOG
#include "GNumberIO.h"


#ifndef ASCIIH
#include "GASCII.h"
#endif
/* p2c: ../luk-1.0/mod/NumberIO.mod:23: 
 * Warning: Could not find module StrIO [271] */

#include "GStrIO.h"

#ifndef StrLibH
#include "GStrLib.h"
#endif

/* */
Static void CardToStr(unsigned int x, unsigned int n, const int a_LOW,
		      const int a_HIGH, Char *a);

Static void StrToCard(const int a_LOW, const int a_HIGH, const Char *a,
		      unsigned int *x);

Static void IntToStr(int x, unsigned int n, const int a_LOW, const int a_HIGH,
		     Char *a);

Static void StrToInt(const int a_LOW, const int a_HIGH, const Char *a, int *x);

Static void HexToStr(unsigned int x, unsigned int n, const int a_LOW,
		     const int a_HIGH, Char *a);

Static void StrToHex(const int a_LOW, const int a_HIGH, const Char *a,
		     unsigned int *x);

Static void OctToStr(unsigned int x, unsigned int n, const int a_LOW,
		     const int a_HIGH, Char *a);

Static void StrToOct(const int a_LOW, const int a_HIGH, const Char *a,
		     unsigned int *x);

Static void BinToStr(unsigned int x, unsigned int n, const int a_LOW,
		     const int a_HIGH, Char *a);

Static void StrToBin(const int a_LOW, const int a_HIGH, const Char *a,
		     unsigned int *x);

Static void ReadOct(unsigned int *x);

Static void WriteOct(unsigned int x, unsigned int n);

Static void ReadBin(unsigned int *x);

Static void WriteBin(unsigned int x, unsigned int n);

Static void ReadCard(unsigned int *x);

Static void WriteCard(unsigned int x, unsigned int n);

Static void ReadInt(int *x);

Static void WriteInt(int x, unsigned int n);

Static void ReadHex(unsigned int *x);

Static void WriteHex(unsigned int x, unsigned int n);


/*    */


#define MaxLineLength   79
#define MaxDigits       20
#define MaxHexDigits    20
#define MaxOctDigits    40
#define MaxBits         64
/* ../luk-1.0/mod/NumberIO.mod, line 69: undefined symbol WriteString */
/* Translation aborted. */
--------------------------
