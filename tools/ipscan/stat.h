#define ETH "eth0"
#define SN_UPDATE_SECS	1	/* Number of seconds between updates */
#define SN_NUM_PORTS	9	/* Number of TCP ports to show */
#define SN_MAX_TCP_PORTS 1024   /* Number of TCP ports to tally */
#define SN_PORT_TYPE_LEN 20     /* Length of TCP port type labels */

void itstime(int errnum);
void set_null();
void usage(char *arg);

struct registers{
	int e :1;	/* Ethernet window */
	int t :1;	/* Types window */
	int g :1;	/* General window */
	int ip_option :1;	/* IP activity */

        int  ethercount;
        long etherbytes;

	int  othercount;
	long otherbytes;

	/* TCP/IP Packet types */
	int ip_count;
	int icmp_count;
	int arp;
	int rarp;
	int ipx;
        int tcp_count;

        /* Vines protocol */
        int vines;

        /* SNA on Ethernet protocol */
        int sna_on_ethernet;

        /* IEEE802.3 protocol */
        int new_ethernet_count;
        int netbios_on_802;

        /* unknown types */
	int unknown_type;
	int unknown_frame_type;		/* store last unknown frame code */
	int unknown_sap;		/* store last unknown sap codes */

        /* TCP port numbers to display */
	int tcp_ports[SN_NUM_PORTS];

        };

#ifdef MAIN_LINE

struct registers regis;
int     packet_type;
int	rewrite_labels = 1;
int	temp_int;

char      tcp_port_types[SN_MAX_TCP_PORTS][SN_PORT_TYPE_LEN+1];
u_int16_t tcp_port_count[SN_MAX_TCP_PORTS];

#else

extern struct registers regis;
extern int     packet_type;
extern int	rewrite_labels;
extern int	temp_int;

extern char tcp_port_types[SN_MAX_TCP_PORTS][SN_PORT_TYPE_LEN+1];
extern u_int16_t tcp_port_count[SN_MAX_TCP_PORTS];

#endif
