#include <stdio.h>
#include "../src/random_list.c"
#include "../src/shuffle_three.c" 


#define ASSERTION(stmt) if(stmt) { printf("PASS"); } else { printf("FAIL"); }

int test_shuffle_three() {
   

}

int test_random_list() {
  // provides a pointer to an array of appropriate length
  int[]* list = random_list(10);
  ASSERTION(sizeof(&list) == (sizeof(int) * 10))
}


int main() {
   test_random_list();
}



