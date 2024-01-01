#include <sys/types.h>

char *strcpy (char *from, char *to)
{
  int i=0;

  while (from[i] != (char)0) {
    to[i] = from[i];
    i++;
  }
  to[i] = (char)0;
  return to;
}

int strlen (char *s)
{
  int i=0;

  while (s[i] != (char)0) {
    i++;
  }
  return( i );
}

void *memcpy(void *dest, const void *src, size_t n)
{
  unsigned char *d = (unsigned char *)dest;
  unsigned char *s = (unsigned char *)src;

  while (n > 0) {
    *d = *s;
    dest++;
    src++;
    n--;
  }
  return dest;
}

void *__gxx_personality_v0;
