#include <stdio.h>
#include <stdlib.h>

extern int asm_main(char *);

int main() {
  char *hp; int r;

  hp = malloc(4000000);
  if (hp == NULL) return 1;

  fprintf(stderr, "hp = %p\n", hp);

  r = asm_main(hp);
  fprintf(stderr, "r = %d\n", r);

  return 0;
}
