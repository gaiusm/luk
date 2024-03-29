
/*
 *  this is very hacked, beware - cos someone fucked up badly
 *  by having two different htonl definitions: unsigned int vs. int_32
 */

#ifndef _I386_BYTEORDER_H
#define _I386_BYTEORDER_H

#undef ntohl
#undef ntohs
#undef htonl
#undef htons

#ifndef __LITTLE_ENDIAN
#define __LITTLE_ENDIAN 1234
#endif

#ifndef __LITTLE_ENDIAN_BITFIELD
#define __LITTLE_ENDIAN_BITFIELD
#endif

/* For avoiding bswap on i386 */
#ifdef __KERNEL__
#include <linux/config.h>
#endif

extern unsigned long int	ntohl(unsigned long int);
extern unsigned short int	ntohs(unsigned short int);
extern unsigned long int	htonl(unsigned long int);
extern unsigned short int	htons(unsigned short int);

extern __inline__ unsigned long int	__ntohl(unsigned long int);
extern __inline__ unsigned short int	__ntohs(unsigned short int);
extern __inline__ unsigned long int	__constant_ntohl(unsigned long int);
extern __inline__ unsigned short int	__constant_ntohs(unsigned short int);

extern __inline__ unsigned long int
__ntohl(unsigned long int x)
{
#if defined(__KERNEL__) && !defined(CONFIG_M386)
	__asm__("bswap %0" : "=r" (x) : "0" (x));
#else
	__asm__("xchgb %b0,%h0\n\t"	/* swap lower bytes	*/
		"rorl $16,%0\n\t"	/* swap words		*/
		"xchgb %b0,%h0"		/* swap higher bytes	*/
		:"=q" (x)
		: "0" (x));
#endif	
	return x;
}

#define __constant_ntohl(x) \
	((unsigned long int)((((unsigned long int)(x) & 0x000000ffU) << 24) | \
			     (((unsigned long int)(x) & 0x0000ff00U) <<  8) | \
			     (((unsigned long int)(x) & 0x00ff0000U) >>  8) | \
			     (((unsigned long int)(x) & 0xff000000U) >> 24)))

extern __inline__ unsigned short int
__ntohs(unsigned short int x)
{
	__asm__("xchgb %b0,%h0"		/* swap bytes		*/
		: "=q" (x)
		:  "0" (x));
	return x;
}

#define __constant_ntohs(x) \
	((unsigned short int)((((unsigned short int)(x) & 0x00ff) << 8) | \
			      (((unsigned short int)(x) & 0xff00) >> 8))) \

#define __htonl(x) __ntohl(x)
#define __htons(x) __ntohs(x)
#define __constant_htonl(x) __constant_ntohl(x)
#define __constant_htons(x) __constant_ntohs(x)

#ifdef  __OPTIMIZE__
#  define ntohl(x) \
(__builtin_constant_p((long)(x)) ? \
 __constant_ntohl((x)) : \
 __ntohl((x)))
#  define ntohs(x) \
(__builtin_constant_p((short)(x)) ? \
 __constant_ntohs((x)) : \
 __ntohs((x)))
#  define htonl(x) \
(__builtin_constant_p((long)(x)) ? \
 __constant_htonl((x)) : \
 __htonl((x)))
#  define htons(x) \
(__builtin_constant_p((short)(x)) ? \
 __constant_htons((x)) : \
 __htons((x)))
#endif

#endif



/* Copyright (C) 1991,92,93,94,95,96,97,98 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#ifndef	_NETINET_IN_H

#define	_NETINET_IN_H	1
#include <features.h>

#include <sys/socket.h>
#include <sys/types.h>


__BEGIN_DECLS

/* Standard well-defined IP protocols.  */
enum
  {
    IPPROTO_IP = 0,	 /* Dummy protocol for TCP.  */
    IPPROTO_ICMP = 1,	 /* Internet Control Message Protocol.  */
    IPPROTO_IGMP = 2,	 /* Internet Group Management Protocol. */
    IPPROTO_IPIP = 4,	 /* IPIP tunnels (older KA9Q tunnels use 94).  */
    IPPROTO_TCP = 6,	 /* Transmission Control Protocol.  */
    IPPROTO_EGP = 8,	 /* Exterior Gateway Protocol.  */
    IPPROTO_PUP = 12,	 /* PUP protocol.  */
    IPPROTO_UDP = 17,	 /* User Datagram Protocol.  */
    IPPROTO_IDP = 22,	 /* XNS IDP protocol.  */
    IPPROTO_IPV6 = 41,   /* IPv6-in-IPv4 tunnelling.  */
    IPPROTO_ICMPV6 = 58, /* ICMPv6.  */

    IPPROTO_RAW = 255,	 /* Raw IP packets.  */
    IPPROTO_MAX
  };

/* Standard well-known ports.  */
enum
  {
    IPPORT_ECHO = 7,		/* Echo service.  */
    IPPORT_DISCARD = 9,		/* Discard transmissions service.  */
    IPPORT_SYSTAT = 11,		/* System status service.  */
    IPPORT_DAYTIME = 13,	/* Time of day service.  */
    IPPORT_NETSTAT = 15,	/* Network status service.  */
    IPPORT_FTP = 21,		/* File Transfer Protocol.  */
    IPPORT_TELNET = 23,		/* Telnet protocol.  */
    IPPORT_SMTP = 25,		/* Simple Mail Transfer Protocol.  */
    IPPORT_TIMESERVER = 37,	/* Timeserver service.  */
    IPPORT_NAMESERVER = 42,	/* Domain Name Service.  */
    IPPORT_WHOIS = 43,		/* Internet Whois service.  */
    IPPORT_MTP = 57,

    IPPORT_TFTP = 69,		/* Trivial File Transfer Protocol.  */
    IPPORT_RJE = 77,
    IPPORT_FINGER = 79,		/* Finger service.  */
    IPPORT_TTYLINK = 87,
    IPPORT_SUPDUP = 95,		/* SUPDUP protocol.  */


    IPPORT_EXECSERVER = 512,	/* execd service.  */
    IPPORT_LOGINSERVER = 513,	/* rlogind service.  */
    IPPORT_CMDSERVER = 514,
    IPPORT_EFSSERVER = 520,

    /* UDP ports.  */
    IPPORT_BIFFUDP = 512,
    IPPORT_WHOSERVER = 513,
    IPPORT_ROUTESERVER = 520,

    /* Ports less than this value are reserved for privileged processes.  */
    IPPORT_RESERVED = 1024,

    /* Ports greater this value are reserved for (non-privileged) servers.  */
    IPPORT_USERRESERVED = 5000
  };


/* Internet address.  */
struct in_addr
  {
    unsigned int s_addr;
  };


/* Definitions of the bits in an Internet address integer.

   On subnets, host and network parts are found according to
   the subnet mask, not these masks.  */

#define	IN_CLASSA(a)		((((unsigned) (a)) & 0x80000000) == 0)
#define	IN_CLASSA_NET		0xff000000
#define	IN_CLASSA_NSHIFT	24
#define	IN_CLASSA_HOST		(0xffffffff & ~IN_CLASSA_NET)
#define	IN_CLASSA_MAX		128

#define	IN_CLASSB(a)		((((unsigned) (a)) & 0xc0000000) == 0x80000000)
#define	IN_CLASSB_NET		0xffff0000
#define	IN_CLASSB_NSHIFT	16
#define	IN_CLASSB_HOST		(0xffffffff & ~IN_CLASSB_NET)
#define	IN_CLASSB_MAX		65536

#define	IN_CLASSC(a)		((((unsigned) (a)) & 0xe0000000) == 0xc0000000)
#define	IN_CLASSC_NET		0xffffff00
#define	IN_CLASSC_NSHIFT	8
#define	IN_CLASSC_HOST		(0xffffffff & ~IN_CLASSC_NET)

#define	IN_CLASSD(a)		((((unsigned) (a)) & 0xf0000000) == 0xe0000000)
#define	IN_MULTICAST(a)		IN_CLASSD(a)

#define	IN_EXPERIMENTAL(a)	((((unsigned) (a)) & 0xe0000000) == 0xe0000000)
#define	IN_BADCLASS(a)		((((unsigned) (a)) & 0xf0000000) == 0xf0000000)

/* Address to accept any incoming messages.  */
#define	INADDR_ANY		((unsigned) 0x00000000)
/* Address to send to all hosts.  */
#define	INADDR_BROADCAST	((unsigned) 0xffffffff)
/* Address indicating an error return.  */
#define	INADDR_NONE		0xffffffff

/* Network number for local host loopback.  */
#define	IN_LOOPBACKNET	127
/* Address to loopback in software to local host.  */
#ifndef INADDR_LOOPBACK
#define	INADDR_LOOPBACK	0x7f000001	/* Internet address 127.0.0.1.  */
#endif

/* Defines for Multicast INADDR.  */
#define INADDR_UNSPEC_GROUP	((u_int32_t) 0xe0000000U)     /* 224.0.0.0 */
#define INADDR_ALLHOSTS_GROUP	((u_int32_t) 0xe0000001U)     /* 224.0.0.1 */
#define INADDR_ALLRTRS_GROUP    ((u_int32_t) 0xe0000002U)     /* 224.0.0.2 */
#define INADDR_MAX_LOCAL_GROUP  ((u_int32_t) 0xe00000ffU)     /* 224.0.0.255 */


/* Get the definition of the macro to define the common sockaddr members.  */
#include <sockaddrcom.h>


/* Structure describing an Internet socket address.  */
struct sockaddr_in
  {
    __SOCKADDR_COMMON (sin_);
    unsigned short int sin_port;	/* Port number.  */
    struct in_addr sin_addr;		/* Internet address.  */

    /* Pad to size of `struct sockaddr'.  */
    unsigned char sin_zero[sizeof(struct sockaddr) -
			   __SOCKADDR_COMMON_SIZE -
			   sizeof(unsigned short int) -
			   sizeof(struct in_addr)];
  };


/* Options for use with `getsockopt' and `setsockopt' at the IP level.
   The first word in the comment at the right is the data type used;
   "bool" means a boolean value stored in an `int'.  */
#define	IP_TOS		   1	/* int; IP type of service and precedence.  */
#define	IP_TTL		   2	/* int; IP time to live.  */
#define	IP_HDRINCL	   3	/* int; Header is included with data.  */
#define	IP_OPTIONS	   4	/* ip_opts; IP per-packet options.  */
#define IP_MULTICAST_IF    32	/* in_addr; set/get IP multicast i/f */
#define IP_MULTICAST_TTL   33	/* u_char; set/get IP multicast ttl */
#define IP_MULTICAST_LOOP  34	/* i_char; set/get IP multicast loopback */
#define IP_ADD_MEMBERSHIP  35	/* ip_mreq; add an IP group membership */
#define IP_DROP_MEMBERSHIP 36	/* ip_mreq; drop an IP group membership */

/* To select the IP level.  */
#define SOL_IP	0

/* Structure used to describe IP options for IP_OPTIONS. The `ip_dst'
   field is used for the first-hop gateway when using a source route
   (this gets put into the header proper).  */
struct ip_opts
  {
    struct in_addr ip_dst;	/* First hop; zero without source route.  */
    char ip_opts[40];		/* Actually variable in size.  */
  };

/* Structure used for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP. */
struct ip_mreq
  {
    struct in_addr imr_multiaddr;	/* IP multicast address of group */
    struct in_addr imr_interface;	/* local IP address of interface */
  };


#if REALLY_FUCKED_UP
/* Functions to convert between host and network byte order.

   Please note that these functions normally take `unsigned long int' or
   `unsigned short int' values as arguments and also return them.  But
   this was a short-sighted decision since on different systems the types
   may have different representations but the values are always the same.  */

extern u_int32_t ntohl __P ((u_int32_t __netlong));
extern u_int16_t ntohs __P ((u_int16_t __netshort));
extern u_int32_t htonl __P ((u_int32_t __hostlong));
extern u_int16_t htons __P ((u_int16_t __hostshort));
#endif

#include <endian.h>

#if __BYTE_ORDER == __BIG_ENDIAN
/* The host byte order is the same as network byte order,
   so these functions are all just identity.  */
#define	ntohl(x)	(x)
#define	ntohs(x)	(x)
#define	htonl(x)	(x)
#define	htons(x)	(x)
#endif


/* Bind socket to a priviledged IP port.  */
extern int bindresvport __P ((int __sockfd, struct sockaddr_in *__sock_in));

__END_DECLS

#endif	/* netinet/in.h */
