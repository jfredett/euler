#include <stdio.h>

#define NUMBER_OF_DEFECTS 20000
#define NUMBER_OF_CHIPS 1000000
#define SIMULATE simulation(NUMBER_OF_DEFECTS, NUMBER_OF_CHIPS)


int simulation(int k, int n) {
  return 0;
}

int main() {
  int successes = 0;
  int total = 0;

  while(1) {
    if (SIMULATE) { successes++; }
    total++;
    printf("Probability: %f", successes / total);
  }
}

