/* This is the main program */
#define MAIN_LINE 1

#if defined(USE_CURSES)
#include <ncurses.h>
#endif

#include <stdio.h>

#if !defined(TRUE)
#    define TRUE (1==1)
#endif

#if defined(WORKING_HTONL_AND_COMPATIBLE_WITH_IN_H_AND_BYTEORDER_H)
#   include <asm/byteorder.h> */
#   include <linux/ip.h> */
#else
#   include "rawnet.h"
#endif

#include <unistd.h>
#include <sys/types.h>
#include <strings.h>
#include <sys/ioctl.h>
#include <signal.h>

#include <socketbits.h>
#include <netinet/ip.h>

#include <linux/if_ether.h>
#include <net/if.h>

#include <netinet/tcp.h>
#include <net/ethernet.h>

#include <asm/types.h>

#include "stat.h"

/* time include files */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/timeb.h>


#ifdef _LINUX_IF_ETHER_H
#undef NET_3	/* not using Net/3 definitions */
#endif

#ifndef ETH_HLEN
#define ETH_HLEN 14	/* Ethernet header length */
#endif

#ifndef SOCK_PACKET
#define SOCK_PACKET SOCK_RAW	/* If SOCK_PACKET is wrong, try SOCK_RAW */
#endif

#if 0
#ifndef NET_3
#define ether_head	ethhdr		/* ether_head structure */
#define ether_type	h_proto		/* Ethernet Packet Type ID field */
#define ip		iphdr		/* IP header structure */
#define ip_p		protocol	/* IP protocol field */
#define ip_hl		ihl		/* IP header length field */
#define th_sport	source		/* TCP source port field */
#define th_dport	dest		/* TCP destination port field */
#endif
#endif

#define ether_head	ether_header	/* ether_head structure */
#define ip		iphdr		/* IP header structure */
#define ip_p		protocol	/* IP protocol field */
#define ip_hl		ihl		/* IP header length field */
#define th_sport	source		/* TCP source port field */
#define th_dport	dest		/* TCP destination port field */

#define SN_RCV_BUF_SIZE	(sizeof(struct ether_head)+ \
				 sizeof(struct ip)+ \
                                 sizeof(struct ip_options)+ \
				 sizeof(struct tcphdr))
					/* above was 1600, but kernel discards*/
					/* excess and tells us full length,   */
					/* so we only need enough to analyze. */

void handle_frame (unsigned char *buf, int length, struct sockaddr *saddr);
void handle_ip (struct ip *buf, int length);

static int timingAnalysis=0;


main (int argc, char *argv[])
{
  int sd;
  struct ifreq ifr, oldifr;
  char *device = ETH;
  char in_char;
  struct sockaddr saddr;
  int sizeaddr;
  unsigned char buf[SN_RCV_BUF_SIZE];
  int length;

  {  /* Compound statement to make initializers vanish after init. */
    int op;

    memset( &regis, 0, sizeof(struct registers) );

    for( temp_int = 0; temp_int < SN_MAX_TCP_PORTS; temp_int++ ) {
      tcp_port_count[temp_int] = 0;
      strcpy(tcp_port_types[temp_int],"");
    }
    for( temp_int = 0; temp_int < SN_NUM_PORTS; temp_int++ ) {
      regis.tcp_ports[temp_int] = -1;
    }

    /* Initialize TCP port information to display */
    /* You can find these in /etc/services. */
    /* Probably also in RFC1700, along with many networking numbers. */
#   if( SN_NUM_PORTS > 0 )
    regis.tcp_ports[0] = 20;	/* FTP Data */
#   if( SN_NUM_PORTS > 1 )
    regis.tcp_ports[1] = 119;	/* NNTP */
#   if( SN_NUM_PORTS > 2 )
    regis.tcp_ports[2] = 111;   /* RPC/NFS */
#   if( SN_NUM_PORTS > 3 )
    regis.tcp_ports[3] = 80;	/* WWW/HTTP */
#   if( SN_NUM_PORTS > 4 )
    regis.tcp_ports[4] = 25;	/* Simple Mail Transfer Protocol */
#   if( SN_NUM_PORTS > 5 )
    regis.tcp_ports[5] = 42;	/* DNS */
#   if( SN_NUM_PORTS > 6 )
    regis.tcp_ports[6] = 137;   /* NETBIOS Name Service */
#   if( SN_NUM_PORTS > 7 )
    regis.tcp_ports[7] = 138;	/* NETBIOS Datagram */
#   if( SN_NUM_PORTS > 8 )
    regis.tcp_ports[8] = 139;	/* NETBIOS Session */
#   endif
#   endif
#   endif
#   endif
#   endif
#   endif
#   endif
#   endif
#   endif

    /* Initialize here the labels for the services listed above. */
    strcpy(tcp_port_types[20],  "FTP:");
    strcpy(tcp_port_types[23],  "Telnet:");
    strcpy(tcp_port_types[25],  "SMTP:");
    strcpy(tcp_port_types[42],  "DNS:");
    strcpy(tcp_port_types[79],  "Finger:");
    strcpy(tcp_port_types[80],  "WWW:");
    strcpy(tcp_port_types[101], "NIC Host NS:");
    strcpy(tcp_port_types[103], "X400:");
    strcpy(tcp_port_types[109], "POP2:");
    strcpy(tcp_port_types[111], "RPC/NFS:");
    strcpy(tcp_port_types[119], "NNTP:");
    strcpy(tcp_port_types[137], "NETB NS:");	/* NetBIOS Name Service */
    strcpy(tcp_port_types[138], "NETB Dg:");	/* NetBIOS Datagram */
    strcpy(tcp_port_types[139], "NETBIOS:");	/* NetBIOS Session Service */
    strcpy(tcp_port_types[194], "IRC:");	/* Internet Relay Chat */
    strcpy(tcp_port_types[515], "Printer:");	/* lpd print protocol */
    strcpy(tcp_port_types[520], "RIP:");

    if (argc == 1)
      {
        regis.g = 1;		/* General */
        regis.e = 1;		/* Ethernet totals */
        regis.t = 1;		/* Types of activity */
        regis.ip_option = 1;	/* IP activity */
      }
    else
      while ((op = getopt (argc, argv, "eghits")) != EOF)
        switch (op)
  	{
	  case 'e':
	    regis.e = 1;
	    break;
	  case 'g':
	    regis.g = 1;
	    break;
	  case 'i':
	    regis.ip_option = 1;
	    break;
	  case 't':
	    regis.t = 1;
	    break;
	  case 's':
	    timingAnalysis = 1;
	    break;
	  case 'h':
	  default:
	    usage (argv[0]);
	    break;
	  }

  /* INIT ALARMFUCTION: Alarm triggers update of the display */

#if defined(USE_CURSES)
  if (signal (SIGALRM, itstime) == SIG_ERR)
    {
      perror ("Signal error: ");
      exit (5);
    }
#endif

  /* OPEN SOCKET */

#if 1
  if ((sd = socket (AF_INET, SOCK_PACKET, htons (ETH_P_ALL))) < 0)
#else
  if ((sd = socket (AF_INET, SOCK_PACKET, htons (0x807))) < 0)
#endif
    {
      perror ("Can't get socket: ");
      exit (1);
    }

  /* SET PROMISC */

  strcpy (oldifr.ifr_name, device);
  if (ioctl (sd, SIOCGIFFLAGS, &oldifr) < 0)
    {
      close (sd);
      perror ("Can't get flags: ");
      exit (2);
    }

  /* This should be rewritten to cooperate with other net tools */
  ifr = oldifr;
  ifr.ifr_flags |= IFF_PROMISC;

  if (ioctl (sd, SIOCSIFFLAGS, &ifr) < 0)
    {
      close (sd);
      perror ("Can't set flags: ");
      exit (3);
    }

  } /* Compound statement to make initializer variables vanish after init. */
  /* END OF INITIALISATION */

#if defined(USE_CURSES)
  init_curses ();	/* initialize the screen */
  clrscr ();		/* clear the screen */
  set_null ();		/* clear all variables */
  alarm (1);		/* first screen update in about a second */
#endif


#if (1==0)
  while (TRUE) {
    fd_set readfds;
    fd_set writefds;
    int    nfound;
    FD_ZERO(&readfds);              /* readfds := {}  */
    FD_SET(0, &readfds);            /* INCL(readfds, 0) */
    FD_SET(sd, &readfds);           /* INCL(readfds, sd) */

    nfound = select(sd+1, &readfds, (fd_set *)0, (fd_set *)0, (struct timeval *)0);
    if (nfound < 0) {
      perror("problem in select");
    }
    if (FD_ISSET(0, &readfds)) {
      char ch=getchar();
      printf("ready for input (%c)\n", ch);
    } else if (FD_ISSET(sd, &readfds)) {
      /* This is the main data-gathering loop; keep it small and fast */
      sizeaddr = SN_RCV_BUF_SIZE;

      length = recvfrom (sd, buf, SN_RCV_BUF_SIZE, 0, &saddr, &sizeaddr);
      if (length < 0 ) 
	continue;
      handle_frame (buf, length, &saddr);
    }
  }
#else
  while (TRUE)
  {
    /* This is the main data-gathering loop; keep it small and fast */
    sizeaddr = SN_RCV_BUF_SIZE;

    length = recvfrom (sd, buf, SN_RCV_BUF_SIZE, 0, &saddr, &sizeaddr);
    if (length < 0 ) 
      continue;
    handle_frame (buf, length, &saddr);
  }
#endif

  /* TERMINATE */
#if defined(USE_CURSES)
  cleanup_curses ();
#endif

  /* This should be rewritten to cooperate with other net tools */
  if (ioctl (sd, SIOCSIFFLAGS, &oldifr) < 0)
    {
      close (sd);
      perror ("Can't set flags: ");
      exit (4);
    }

  close (sd);
  exit (0);
}

void
handle_other (unsigned char *buf, int length)
{
  packet_type=ntohs(packet_type);
  if( packet_type < 1501 )
  {  /* if IEEE 802.3 packet instead of Ethernet packet (per RFC 1700) */
    regis.new_ethernet_count++;
    /* Some sources say 0xf0, others say 0xf0f0. Need authoritative source. */
    if( buf[14] == 0xf0 /* && buf[15] == 0xf0 */ )  /* NetBIOS SAP */
              regis.netbios_on_802++;
    else
    {  /* else it's not NetBIOS on 802.3 so we don't know what it is */
      regis.unknown_type++;
      regis.unknown_frame_type = -1;
      regis.unknown_sap = buf[14]*256+buf[15]; /* squeeze SAP values in int */
      /* Hmm.. 0x42 is Spanning Tree, but shouldn't be too many of them. */
    }  /* else it's not NetBIOS on 802.3 so we don't know what it is */
  }  /* if IEEE 802.3 packet instead of Ethernet packet */
}

void
handle_ip (struct ip *buf, int length)
{
  static int x;

  switch (buf->ip_p) 	/* IP Protocol */
    {

    case IPPROTO_TCP:	/* TCP */
      /* The below may count packets twice, but probably both are not displayed */
      if( buf->ip_hl == 5 )
      {  /* if IP header is normal length we can assume 20 octects length */
        if( (x=ntohs(((struct tcphdr *)((void *)buf+20))->th_sport)) < SN_MAX_TCP_PORTS )
          tcp_port_count[ x ]++;				/* count source port */
        if( (x=ntohs(((struct tcphdr *)((void *)buf+20))->th_dport)) < SN_MAX_TCP_PORTS )
          tcp_port_count[ x ]++;				/* count dest port */
      }  /* IP header normal length */
      else
      {  /* IP header with options */
        /* Incidentally, the (void *) is to cause byte-level math not sizeof(struct ip) */
        if( (x=ntohs(((struct tcphdr *)((void *)buf+((buf->ip_hl)*4)))->th_sport)) < SN_MAX_TCP_PORTS )
          tcp_port_count[ x ]++;				/* count source port */
        if( (x=ntohs(((struct tcphdr *)((void *)buf+((buf->ip_hl)*4)))->th_dport)) < SN_MAX_TCP_PORTS )
          tcp_port_count[ x ]++;				/* count dest port */
      }  /* IP header with options */
      regis.tcp_count++;	/* At least this will only count once :-) */
      break;
    case IPPROTO_ICMP:		/* ICMP protocol */
      regis.icmp_count++;
      break;
#if defined(IP_VINES)
    case IP_VINES:		/* Vines protocol in IP */
      regis.vines++;
      break;
#endif
    default:
      /* Don't increment "Other" because are not displaying "Other IP" counts */
      /* This packet does show in the "IP" total, but not elsewhere. */
      break;
    }
}


static char *findEthernetAddress (char *buf)
{
  static char localString[1000];

  sprintf(localString, "source %2x:%2x:%2x:%2x:%2x:%2x   dest %2x:%2x:%2x:%2x:%2x:%2x",
	  ((struct ether_head *)buf)->ether_shost[0],
	  ((struct ether_head *)buf)->ether_shost[1],
	  ((struct ether_head *)buf)->ether_shost[2],
	  ((struct ether_head *)buf)->ether_shost[3],
	  ((struct ether_head *)buf)->ether_shost[4],
	  ((struct ether_head *)buf)->ether_shost[5],
	  ((struct ether_head *)buf)->ether_dhost[0],
	  ((struct ether_head *)buf)->ether_dhost[1],
	  ((struct ether_head *)buf)->ether_dhost[2],
	  ((struct ether_head *)buf)->ether_dhost[3],
	  ((struct ether_head *)buf)->ether_dhost[4],
	  ((struct ether_head *)buf)->ether_dhost[5]);
  return( (char *)&localString );
}

static struct timeb start_tp;
static struct timeb end_tp;
static int totalLength=0;
static int lastLength;
static int numBlocks=0;


void timeCompilationServer (int length)
{
  int t;
  double d;

  if (length == lastLength) {
    totalLength += length;
    numBlocks++;
  } else {
    if (ftime(&end_tp) == -1) {
      perror("ftime");
    }

    t=(end_tp.time*1000+end_tp.millitm)-
      (start_tp.time*1000+start_tp.millitm);

    t %= 1000000;
    d = (numBlocks * 1.0 * lastLength) / t;

    printf("time to send %6d blocks of %6d bytes is %d.%03d seconds  (%4.3f K bytes/sec)\n",
	   numBlocks, lastLength, t / 1000, t % 1000, d);

    /* now for the next time around */
    totalLength = 0;
    numBlocks   = 0;
    lastLength  = length;
    if (ftime(&start_tp) == -1) {
      perror("ftime");
    }
  }
}


void dumpData (char *buf)
{
  int i;

  for (i=0; i<64; i++) {
    printf("%2x ", (int)buf[i]);
  }
  printf("\n");
}


void
handle_frame (unsigned char *buf, int length, struct sockaddr *saddr)
{
  char  name[100];
  char *ethernetAddressString;

  /* Now the grand totals by interface type */
  if (strncmp (saddr->sa_data, "eth", 3) == 0) {
    regis.etherbytes += length;
    regis.ethercount++;
  } else {
    regis.otherbytes += length;
    regis.othercount++;
  };

  packet_type           = ((struct ether_head *)buf)->ether_type;  /* Ethernet packet type ID field */
  ethernetAddressString = findEthernetAddress(buf);

  switch( packet_type )
    {
    case __constant_ntohs(ETH_P_IP):
      regis.ip_count++;
      if( regis.ip_option ) handle_ip ((struct ip *)((void *)buf+ETH_HLEN), length);
      sprintf(name, "ip (%s)", ethernetAddressString);
      displayTraffic(name);
      break;
    case __constant_ntohs(ETH_P_ARP):
      regis.arp++;
      sprintf(name, "arp (%s)", ethernetAddressString);
      displayTraffic(name);
      break;
    case __constant_ntohs(ETH_P_RARP):
      regis.rarp++;
      sprintf(name, "rarp (%s)", ethernetAddressString);
      displayTraffic(name);
      break;
    case __constant_ntohs(ETH_P_IPX):
      regis.ipx++;
      sprintf(name, "ipx (%s)", ethernetAddressString);
      displayTraffic(name);
      break;
    case __constant_ntohs(0x80D5):
      regis.sna_on_ethernet++;		/* SNA on Ethernet */
      sprintf(name, "sna (%s)", ethernetAddressString);
      displayTraffic(name);
      break;
    case __constant_ntohs(0x0BAD):      /* Vines protocol */
      regis.vines++;
      sprintf(name, "vines (%s)", ethernetAddressString);
      displayTraffic(name);
      break;
    case __constant_ntohs(0x0807):      /* compilation server! */
      if (timingAnalysis) {
	timeCompilationServer(length);
      } else {
	sprintf(name, "compilation server (%s)", ethernetAddressString);
	displayTraffic(name);
	dumpData(buf);
      }
      break;
    default:
      if( regis.t ) handle_other (buf, length);
      sprintf(name, "unrecognised packet type (hex 0x%x   dec %d)  %s", packet_type, packet_type, ethernetAddressString);
      displayTraffic(name);
      break;
    }
}

void
usage (char *arg)
{
  fprintf (stderr, "\n%s [-egith]\n\n", arg);
  fprintf (stderr, "    -e  show Ethernet window\n");
  fprintf (stderr, "    -g  show General window\n");
  fprintf (stderr, "    -i  show TCP/IP window\n");
  fprintf (stderr, "    -t  show Types window\n");
  fprintf (stderr, "    -s  timing analysis of compilation server\n");
  fprintf (stderr, "    -h  show this message\n");
  exit (0);
}
