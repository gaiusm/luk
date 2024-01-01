

#if defined(USE_CURSES)
#   include <ncurses.h>
#endif

#include "rawnet.h"
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include "stat.h"


/* #undef the following to show ARP and RARP separately and not show ICMP */
#define COMBINE_ARP_RARP


#if defined(USE_CURSES)
void
itstime (int errnum)
{
  extern struct registers regis;
  int len, noframes, true_noframes;
  long int nobytes;
  char eth[20];
  WINDOW *genwin = NULL;
  WINDOW *etherwin = NULL;
  WINDOW *plipwin = NULL;
  WINDOW *typewin = NULL;

  /* This code calculates Kilobytes Per Second by dividing by the time */
  /* period for which the timer was set.  This would be more accurate  */
  /* on a busy system if the actual time since set_null() was used,    */
  /* at an increase in CPU time.  The way it is coded now allows the   */
  /* compiler to perform part of the calculation at compile time.      */

nobytes = regis.etherbytes + regis.otherbytes;
noframes = true_noframes = regis.ethercount + regis.othercount;
  if (noframes == 0)
  {
    noframes = 1;		/* dirty, i know, but it's easier and doesn't give errors*/
  }

  if( rewrite_labels ) {
    clrscr();
    mvprintw (0, (COLS - 18) / 2, "NETWORK MONITOR");
  }

  if (regis.g)
  {
    genwin = subwin (stdscr, 6, 22, 2, 10);
    if( genwin != NULL ) {
      if( rewrite_labels && genwin != NULL ) {
        mvwprintw (genwin, 0, 0, "Guess-timates");
        mvwprintw (genwin, 2, 0, "KB/sec:");
        mvwprintw (genwin, 3, 0, "Frames/sec:");
        mvwprintw (genwin, 4, 0, "Av. frame len:");
      }
      mvwprintw (genwin, 2, 14, "%7.2f", 
                 nobytes / (1024.0*SN_UPDATE_SECS));	/* KB/sec */

      /* Note that the maximum is 14,200 64 bit frames per second [WRL-TR-88.4] */
      /* So %5d would be sufficient. */
      mvwprintw (genwin, 3, 14, "%7d", 
                 true_noframes/SN_UPDATE_SECS);		/* frames/sec */
      if (true_noframes == 0)
      {
        len = 0;
      }
      else
        len = nobytes / noframes;
      mvwprintw (genwin, 4, 14, "%7d", len);	/* frame length */
    }
  }

  if (regis.e)
    if (regis.g)
      etherwin = subwin (stdscr, 7, 22, 9, 10);
    else
      etherwin = subwin (stdscr, 7, 22, 2, 10);
  if (regis.e && etherwin != NULL)
  {
    if( rewrite_labels ) {
      strcpy (eth, "ETHERNET: ");
      strncat (eth, ETH, 9);
      mvwprintw (etherwin, 0, 0, eth);
      mvwprintw (etherwin, 2, 0, "KB/sec:");
      mvwprintw (etherwin, 3, 0, "Frames/sec:");
      mvwprintw (etherwin, 4, 0, "802.3 Fr/sec:");
      mvwprintw (etherwin, 5, 0, "Av. frame len:");
      mvwprintw (etherwin, 6, 0, "Load:");
    }
    mvwprintw (etherwin, 2, 14, "%7.2f", 
               regis.etherbytes / (1024.0*SN_UPDATE_SECS));/* KB/sec */
    mvwprintw (etherwin, 3, 14, "%7d", 
               regis.ethercount/SN_UPDATE_SECS);	/* frames/sec */
    if (regis.ethercount == 0)
      len = 0;
    else
      len = regis.etherbytes / regis.ethercount;
    mvwprintw (etherwin, 4, 14, "%7d%", 
               regis.new_ethernet_count/SN_UPDATE_SECS);  /* IEEE 802.3 frames per second */
    mvwprintw (etherwin, 5, 14, "%7d", len);		/* average frame len */
    mvwprintw (etherwin, 6, 15, "%6.2f%%",
	       regis.etherbytes * 100.0 / (1250000.0*SN_UPDATE_SECS));	/* percent load */
    /* The 1,250,000 number is 10Mbits divided by 8 bytes per bit */
  }

  if (regis.t)
   if (regis.g || regis.e)
      typewin = subwin (stdscr, 21, 40, 2, 40);
    else
      typewin = subwin (stdscr, 21, 40, 2, 6);
  if (regis.t && typewin != NULL)
  {
    if( rewrite_labels ) {
      mvwprintw (typewin, 0, 0, "TYPES");
      mvwprintw (typewin, 0, 17, "FRAMES");
      mvwprintw (typewin, 2, 0, "IP:");
#ifdef COMBINE_ARP_RARP
      mvwprintw (typewin, 3, 0, "ARP+RARP:");
      mvwprintw (typewin, 4, 0, "ICMP:");
#else
      mvwprintw (typewin, 3, 0, "ARP:");
      mvwprintw (typewin, 4, 0, "RARP:");
#endif
      mvwprintw (typewin, 5, 0, "IPX:");
      mvwprintw (typewin, 6, 0, "Vines:");
      mvwprintw (typewin, 7, 0, "NetB 802:");
      mvwprintw (typewin, 8, 0, "SNAether:");
      mvwprintw (typewin, 9, 0, "Other:");

      if( regis.ip_option ) {
        int temp_int;

        mvwprintw (typewin,  11, 0, "TCP/IP");

	for( temp_int = 0; temp_int < SN_NUM_PORTS; temp_int++ ) {
	  if( regis.tcp_ports[temp_int] )
            mvwprintw (typewin,  12+temp_int, 0, 
                       tcp_port_types[regis.tcp_ports[temp_int]]);
          else
            mvwprintw (typewin,  12+temp_int, 0, "          " );
        }
      }
    } /* if rewrite_labels */

      mvwprintw (typewin, 0, 9, "%7d", true_noframes);

      mvwprintw (typewin, 2, 9, "%7d %6.1f%%", regis.ip_count, regis.ip_count * 100.0 / noframes);
#ifdef COMBINE_ARP_RARP
      mvwprintw (typewin, 3, 9, "%7d %6.1f%%", regis.arp+regis.rarp, (regis.arp+regis.rarp) * 100.0 / noframes);
      mvwprintw (typewin, 4, 9, "%7d %6.1f%%", regis.icmp_count, regis.icmp_count * 100.0 / noframes);
#else
      mvwprintw (typewin, 3, 9, "%7d %6.1f%%", regis.arp, regis.arp * 100.0 / noframes);
      mvwprintw (typewin, 4, 9, "%7d %6.1f%%", regis.rarp, regis.rarp * 100.0 / noframes);
#endif
      mvwprintw (typewin, 5, 9, "%7d %6.1f%%", regis.ipx, regis.ipx * 100.0 / noframes);
      /* regis.vines is both 0x0BAD packets and Vines_under_IP */
      mvwprintw (typewin, 6, 9, "%7d %6.1f%%", regis.vines, regis.vines * 100.0 / noframes);
      mvwprintw (typewin, 7,  9, "%7d %6.1f%%", regis.netbios_on_802, regis.netbios_on_802 * 100.0 / noframes);
      mvwprintw (typewin, 8,  9, "%7d %6.1f%%", regis.sna_on_ethernet, regis.sna_on_ethernet * 100.0 / noframes);

      if( regis.unknown_frame_type > 0 )
        mvwprintw (typewin, 9,  6, "%04X", regis.unknown_frame_type );
      else
        mvwprintw (typewin, 9,  6, "    " );
      mvwprintw (typewin, 9, 10, "%6d %6.1f%%", regis.unknown_type, regis.unknown_type * 100.0 / noframes);
      if( regis.unknown_sap > 0 )
        mvwprintw (typewin, 10,  2, "SAP:%04X", regis.unknown_sap );
      else
        mvwprintw (typewin, 10,  2, "        ");
      
      if( regis.ip_option ) {
        mvwprintw (typewin, 11, 9, "%7d %6.1f%%", regis.tcp_count, regis.tcp_count * 100.0 / noframes);
        /* For speed, I used inline code instead of a for-next loop */
#       if( SN_NUM_PORTS > 0)
        if( regis.tcp_ports[0] )
          mvwprintw (typewin, 12, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[0]], tcp_port_count[regis.tcp_ports[0]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 1)
        if( regis.tcp_ports[1] )
          mvwprintw (typewin, 13, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[1]], tcp_port_count[regis.tcp_ports[1]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 2)
        if( regis.tcp_ports[2] )
          mvwprintw (typewin, 14, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[2]], tcp_port_count[regis.tcp_ports[2]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 3)
        if( regis.tcp_ports[3] )
          mvwprintw (typewin, 15, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[3]], tcp_port_count[regis.tcp_ports[3]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 4)
        if( regis.tcp_ports[4] )
          mvwprintw (typewin, 16, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[4]], tcp_port_count[regis.tcp_ports[4]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 5)
        if( regis.tcp_ports[5] )
          mvwprintw (typewin, 17, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[5]], tcp_port_count[regis.tcp_ports[5]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 6)
        if( regis.tcp_ports[6] )
          mvwprintw (typewin, 18, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[6]], tcp_port_count[regis.tcp_ports[6]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 7)
        if( regis.tcp_ports[7] )
          mvwprintw (typewin, 19, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[7]], tcp_port_count[regis.tcp_ports[7]] * 100.0 / noframes);
#       endif
#       if( SN_NUM_PORTS > 8)
        if( regis.tcp_ports[8] )
          mvwprintw (typewin, 20, 9, "%7d %6.1f%%", tcp_port_count[regis.tcp_ports[8]], tcp_port_count[regis.tcp_ports[8]] * 100.0 / noframes);
#       endif
      }

  }

  rewrite_labels = 0;  /* labels have been written */

  set_null ();

  touchwin (stdscr);	/* make screen ready to be refreshed */
  refresh ();		/* update the screen with the new info */

  /* delete the subwindows now that the screen has been updated */
  if (genwin != NULL) 
  {
    delwin(genwin);
    genwin = NULL;
  }
  if (etherwin != NULL) 
  {
    delwin(etherwin);
    etherwin = NULL;
  }
  if (typewin != NULL) 
  {
    delwin(typewin);
    typewin = NULL;
  }

  if (signal (SIGALRM, itstime) == SIG_ERR)
    {
      perror ("Signal error: ");
      exit (5);
    }
  alarm (SN_UPDATE_SECS);
  return;

}
#else


void displayTraffic (char *str)
{
  static lastbytes;

  int noframes, true_noframes;
  long int nobytes;

  nobytes = regis.etherbytes + regis.otherbytes;
  noframes = true_noframes = regis.ethercount + regis.othercount;
  if (noframes == 0)
  {
    noframes = 1;
  }

  printf("%7.2f B  %7d frames/sec   average frame length %7d   found %s\n",
	 (nobytes - lastbytes)/1.0,
	 true_noframes/SN_UPDATE_SECS,
	 nobytes / noframes,
	 str);
  lastbytes = nobytes;
}
#endif
