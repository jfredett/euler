#include <stdio.h>
#include <stdlib.h>

int coin_flip() {
  return !!(rand() % 2);
}

int simulation(int k, int n) {
  int* chips = (int*) malloc(sizeof(int) * n); //allocate an array of `n` integers
  int result = 1;

  while (k > 0) {
    for(int i = 0; i < n; i++) { 
      if (coin_flip()) { continue; }

      if(++chips[i] < 3) {
        // no critical defects yet, but we assigned a chip defect, so decrement
        // the count
        k--;
      } else {
        // this batch has a critical defect
        result = 0;
        //that's right, bitches, I need to bust free of nested loops.
        //Since I don't want to duplicate the free (and potentially expose
        //myself to some memory leak because I screw something else up)
        //this is a way to keep things DRY., and avoid nasty logic.
        goto end_of_loop; 
      }
    }
  }
  end_of_loop:

  free(chips);
  return result;
}
