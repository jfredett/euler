#include "./problem-307.h"
#include <stdio.h>
#include <time.h>

int main() {
  int successes = 0;
  int total = 0;

  srand((long)time(NULL)); // seed the RNG 

  while(1) {
    if (SIMULATE) { successes++; }
    total++;

    printf("Probability: %f\n", ((float) successes) / ((float) total));

    //guard against big long loop
    if (total > 10) { break; }
  }
}

