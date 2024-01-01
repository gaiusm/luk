#include "rawnet.h"
#include "stat.h"

void
set_null()
{
 extern struct registers regis;

 regis.ethercount = 0;
 regis.etherbytes = 0;
 regis.othercount = 0;
 regis.otherbytes = 0;
 regis.ip_count = 0;
 regis.icmp_count = 0;
 regis.arp = 0;
 regis.rarp = 0;
 regis.ipx = 0;
 regis.tcp_count = 0;
 regis.vines = 0;
 regis.sna_on_ethernet = 0;
 regis.new_ethernet_count = 0;
 regis.netbios_on_802 = 0;
 regis.unknown_type = 0;
 regis.unknown_frame_type = 0;
 regis.unknown_sap = 0;

 if( regis.ip_option )
 {
   for( temp_int = 0; temp_int < SN_MAX_TCP_PORTS; temp_int++ )
     tcp_port_count[temp_int]=0;
  }

}
