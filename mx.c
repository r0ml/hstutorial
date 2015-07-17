
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void hexify(char *buf, int len, char *inp) {
  for(int i=0;i<len;i++) {
    sprintf(buf+(i*2), "%02x", 0xff & (unsigned int) inp[i]);
  }
  return;
}

/*
int main(int argc, char *argv[] ) {
  if (argc < 2) { printf("%s\n", "missing argument"); exit(1); }
  int j = strlen(argv[1]);
  char * buf = malloc(1+2*j);
  buf[2*j]='\0';
  hexify(buf, j, argv[1]);
  printf("%s\n", buf);
  free(buf);
}
*/


