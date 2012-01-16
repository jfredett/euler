#include <stdlib.h>

int die_roll(int qty, int sides) {
  int sum = 0;
  for(int i = 0; i < qty; i++) {
    sum += (rand() % sides) + 1;
  }
  return sum;
}

int simulation() {
  return die_roll(9,4) > die_roll(6,6);
}
