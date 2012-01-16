#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "./problem-205.h"


int main() {
  long iterations = 0;
  long peter_wins = 0;
  long colin_wins = 0;

  srand((int) time(NULL));

  while(++iterations) {

    if(simulation()) 
      peter_wins++; 
    else 
      colin_wins++; 

    if (iterations % 10000000 == 0) {
      system("clear");
      printf("Number of Peter wins: %ld\n", peter_wins);
      printf("Number of Colin wins: %ld\n", colin_wins);
      printf("Number of total iterations: %ld\n", iterations);
      printf(
          "Probability of Peter winning over Colin: %.15f\n", 
          (double) peter_wins / (double) iterations
      );
    }
  }
}
