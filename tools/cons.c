#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdlib.h>

/*
 * Author     : Gaius Mulley
 * Title      : cons
 * Description: a really trivial terminal emulation program
 * Date       : Sat Aug  5 10:04:53  1995
 *
 *
 * $Header: /usr/src/cvs/gm2/m2/os/tools/cons.c,v 1.2 1997-07-11 13:26:04 gaius Exp $
 *
 * $Log: cons.c,v $
 * Revision 1.2  1997-07-11 13:26:04  gaius
 * made remote debugging serial device driver more efficient. It now
 * recognises an 16550A and enables the 16 byte FIFO
 *
 * Revision 1.1.1.1  1996/05/28 10:21:13  gaius
 * Modula-2 realtime system sources imported
 *
 *
 */


#define MAXCHARS  4096
#define BAUD      38400
#define ERROR(X)       (fprintf(stderr, "%s:%d error %s\n", __FILE__, __LINE__, X) && \
			(fflush(stderr)))
#if !defined(TRUE)
#  define TRUE           (1==1)
#endif
#if !defined(FALSE)
#  define FALSE          (1==0)
#endif


/* prototypes */
static void selectChars (int infd, int outfd);
static void setupDevice (char *device);
static void resetDevice (char *device);
static int checkFinished (char *s, int n);
static int max (int a, int b);


static char *deviceName="/dev/ttyS0" ;

static int max (int a, int b)
{
  if (a > b) {
    return( a );
  } else {
    return( b );
  }
}


static void setupDevice (char *device)
{
  char commandLine[MAXCHARS];

  printf("terminal emulation program running on: %s\n", device);
  /*
   * 8 bits, disable RTS/CTS, do not generate/expect parity, xon/xoff, no cr -> lf, no echo
   * BAUD baud
   */
  sprintf(commandLine, "stty raw -icanon -inlcr -icrnl -echo cs8 -parenb -crtscts ixoff %d < %s\n", BAUD, device);
  system(commandLine);

  /* now for our keyboard: raw no cr/lf and no echo */
  sprintf(commandLine, "stty raw -icanon -inlcr -icrnl -echo\n");
  system(commandLine);
}


static void resetDevice (char *device)
{
  char commandLine[MAXCHARS];

#if 0  
  sprintf(commandLine, "stty sane < %s\n", device);
  system(commandLine);
#endif
  /* reset our keyboard */
  sprintf(commandLine, "stty sane\n");
  system(commandLine);
  printf("\nterminal emulation program finished\n");
}


static int checkFinished (char *s, int n)
{
  int i=0;

  while (i < n) {
    if (s[i] == '!') {
      return( TRUE );
    } else {
      i++;
    }
  }
  return( FALSE );
}


/*
 * selectChars - takes keyboard input and passes it to device
 *               takes device input and passes it to screen.
 */

static void selectChars (int infd, int outfd)
{
  fd_set readfds;
  fd_set writefds;
  int maxfds, nfound, n;
  char buff[MAXCHARS];
  int finished=FALSE;

  do {
    /* setup input bitset */
    FD_ZERO(&readfds);              /* readfds := {}  */
    FD_SET(0, &readfds);            /* INCL(readfds, 0) */
    FD_SET(infd, &readfds);         /* INCL(readfds, infd) */

    /* setup output bitset */
    FD_ZERO(&writefds);             /* writefds := {}  */
#if 0
    FD_SET(outfd, &writefds);       /* INCL(writefds, outfd) */
#endif

    maxfds = max(infd, outfd)+1;
    nfound = select(maxfds, &readfds, (fd_set *)0, (fd_set *)0, (struct timeval *)0);
    if (FD_ISSET(0, &readfds)) {
      n = read(0, buff, MAXCHARS);
      finished = checkFinished(buff, n);
      if (write(outfd, buff, n) != n) {
	ERROR("device write failed");
      }
    }
    if (FD_ISSET(infd, &readfds)) {
      n = read(infd, buff, MAXCHARS);
      if (write(1, buff, n) != n) {
	ERROR("stdout write failed");
      }
    }
  } while (! finished);
}


int main (int argc, char *argv[])
{
  int infd, outfd;

  if (argc == 2) {
    deviceName = argv[1];
  }
  setupDevice(deviceName);
  infd = open(deviceName, O_RDONLY);
  if (infd < 0) {
    perror("failed to open device for input"); exit(1);
  }
  outfd = open(deviceName, O_RDWR, 0666);
  if (outfd < 0) {
    perror("failed to open device for output"); exit(1);
  }
  selectChars(infd, outfd);
  resetDevice(deviceName);
}
