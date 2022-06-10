#include <stdio.h>
#include <stdlib.h>

extern int asm_main(char *);

int main() {
  char *hp; int r, i;

  hp = malloc(4000000);
  if (hp == NULL) return 1;

  fprintf(stderr, "hp = %p\n", hp);

  for (i = 0; i < 10000000; i++) {
    r = asm_main(hp);
  }
  fprintf(stderr, "r = %d\n", r);

  return 0;
}
