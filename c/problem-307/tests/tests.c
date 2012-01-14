#include "../src/problem-307.h"
#include <stdio.h>
#include <time.h>

int main() {
  srand((long)time(NULL)); // seed the RNG 

  int heads = 0;
  for(int i = 0; i < 1000000 ; i++) {
    if (coin_flip()) { heads++; }
  }
  printf("the number of heads is %d/1000000\n", heads);

  printf("%d\n", (int)time(NULL));
}

